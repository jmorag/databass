module Databass.Blog where

import Control.Monad (guard)
import Data.Coerce (coerce)
import Data.Kind
import qualified Data.Map.Strict as M
import Data.Proxy
import Data.Type.Map
import qualified Data.Type.Map
import Data.Type.Set (Sort, type (:++))
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

-- data Var (label :: Symbol) = Var

-- data Mapping k v = k :-> v

-- This binds more tightly than list cons (:), which is convenient for pattern matching
type (k :: Symbol) ::: (v :: Type) = k ':-> v
infixr 6 :::

-- data Tuple (attrs :: [Mapping Symbol Type]) where
--   Empty :: Tuple '[]
--   Ext :: Var label -> a -> Tuple as -> Tuple (label ::: a ': as)

type Tuple = Map

data Color = Red | Green | Blue

-- Some example headings
type SHeading =
  '[ "S#" ::: Int
   , "SNAME" ::: String
   , "STATUS" ::: Int
   , "CITY" ::: String
   ]

type PHeading =
  '[ "P#" ::: Int
   , "PNAME" ::: String
   , "COLOR" ::: Color
   , "WEIGHT" ::: Double
   , "CITY" ::: String
   ]

type SPHeading =
  '[ "S#" ::: Int
   , "P#" ::: Int
   , "QTY" ::: Int
   ]

sExample :: Tuple SHeading
sExample = Ext Var 1 $ Ext Var "Smith" $ Ext Var 20 $ Ext Var "London" Empty

data Relation heading key val
  = ( Submap key heading -- Assert the key is a subset of the heading
    , Submap val heading -- Assert the rest of the tuple is also a subset of the heading
    , Split key val heading -- Assert that we can split the heading into keys and vals
    , Unionable key val -- Assert we can stitch keys and vals together
    , Union key val ~ heading -- Assert that when we perform the stitching operation the result is the heading
    , IsMap heading -- Assert that there are no duplicates in the heading and that attributes are sorted
    ) =>
    MkRelation

s ::
  Relation
    (AsMap '["S#" ::: Int, "SNAME" ::: String, "STATUS" ::: Int, "CITY" ::: String])
    '["S#" ::: Int]
    (AsMap '["SNAME" ::: String, "STATUS" ::: Int, "CITY" ::: String])
s = MkRelation

s' ::
  Rel
    (AsMap '["S#" ::: Int, "SNAME" ::: String, "STATUS" ::: Int, "CITY" ::: String])
    '["S#"]
s' = MkRelation

type family Rel (heading :: [Mapping Symbol Type]) (key :: [Symbol]) where
  Rel heading key = Relation heading (heading :!! key) (heading :\\ key)

-- | Type level key lookup
type family (m :: [Mapping Symbol Type]) :! (c :: Symbol) :: Type where
  (label ::: a ': rest) :! label = a
  (attr ': rest) :! label = rest :! label
  '[] :! label = TypeError ( 'Text "Could not find " ':<>: 'ShowType label)

-- | Type level multi-key lookup
type family (m :: [Mapping Symbol Type]) :!! (cs :: [Symbol]) :: [Mapping Symbol Type] where
  m :!! (label ': ls) = (label ::: (m :! label)) ': (m :!! ls)
  m :!! '[] = '[]

-- | Type level key removal
-- type family (m :: [Mapping Symbol Type]) :\ (c :: Symbol) :: [Mapping Symbol Type] where
--   (label ::: a ': rest) :\ label = rest
--   (attr ': rest) :\ label = attr ': (rest :\ label)
--   '[] :\ label = TypeError ( 'Text "Could not find " ':<>: 'ShowType label)

-- | Type level multi-key removal
type family (m :: [Mapping Symbol Type]) :\\ (cs :: [Symbol]) :: [Mapping Symbol Type] where
  m :\\ (label ': ls) = (m :\ label) :\\ ls
  m :\\ '[] = m

type family RelationToMap relation where
  RelationToMap (Relation heading key val) = M.Map (Tuple key) (Tuple val)

type family RelationsToDB (relations :: [Mapping Symbol Type]) where
  RelationsToDB '[] = '[]
  RelationsToDB (name ::: relation ': rest) = name ::: RelationToMap relation ': RelationsToDB rest

data Query (t :: [Mapping Symbol Type]) (relations :: [Mapping Symbol Type]) where
  RelationId ::
    ( relation ~ Relation heading key val
    , (relations :! name) ~ relation
    , IsMember name (RelationToMap relation) (RelationsToDB relations)
    ) =>
    Var name ->
    Relation heading key val ->
    Query heading relations
  Rename ::
    (Sortable (Rename a b t)) =>
    Var a ->
    Var b ->
    Query t relations ->
    Query (Sort (Rename a b t)) relations
  Restrict :: (Tuple t -> Bool) -> Query t relations -> Query t relations
  Project :: (Submap t' t) => Query t relations -> Query t' relations
  Extend ::
    (Member l t ~ 'False, Sortable (l ::: a ': t)) =>
    Var l ->
    (Tuple t -> a) ->
    Query t relations ->
    Query (Sort (l ::: a ': t)) relations
  Join ::
    ( common ~ Intersection t' t
    , Eq (Tuple common)
    , Split common t'_rest t'
    , Split common t_rest t
    , Sortable (common :++ (t'_rest :++ t_rest))
    ) =>
    Proxy t'_rest ->
    Proxy t_rest ->
    Query t' tables ->
    Query t tables ->
    Query (Sort (common :++ (t'_rest :++ t_rest))) tables

runQuery :: Query t relations -> Tuple (RelationsToDB relations) -> [Tuple t]
runQuery q db = case q of
  RelationId name (MkRelation :: Relation heading key val) ->
    let relation = lookp name db
     in fmap (\(k :: Tuple key, v :: Tuple val) -> k `union` v) (M.toList relation)
  Rename a b q' -> map (quicksort . renameTuple a b) $ runQuery q' db
  Restrict f q' -> filter f (runQuery q' db)
  Project q' -> map submap (runQuery q' db)
  Extend label f q' -> map (\t -> quicksort (Ext label (f t) t)) (runQuery q' db)
  Join (_ :: Proxy t_l_rest) (_ :: Proxy t_r_rest) q1 q2 -> do
    l :: Tuple t_l <- runQuery q1 db
    r :: Tuple t_r <- runQuery q2 db
    let (l_common, l_rest) = split @(Intersection t_l t_r) @t_l_rest l
        (r_common, r_rest) = split @(Intersection t_l t_r) @t_r_rest r
    guard (l_common == r_common)
    pure (quicksort $ append l_common (append l_rest r_rest))

type family Rename (x :: Symbol) (y :: Symbol) (relation :: [Mapping Symbol Type]) where
  Rename a b '[] = '[]
  Rename a b ((a ::: t) ': rest) = (b ::: t) ': Rename a b rest
  Rename a b ((b ::: t) ': rest) =
    TypeError
      ( 'Text "Cannot rename "
          ':<>: 'Text a
          ':<>: 'Text " to "
          ':<>: 'Text b
          ':$$: 'Text "The name already exists in the tuple"
      )
  Rename a b (c ': rest) = c ': Rename a b rest

renameTuple :: Var a -> Var b -> Tuple t -> Tuple (Rename a b t)
renameTuple _a _b = unsafeCoerce

-- This doesn't compile
-- renameTuple' :: Var a -> Var b -> Tuple t -> Tuple (Rename a b t)
-- renameTuple' _a _b = coerce

type family Intersection (t :: [Mapping Symbol Type]) (t' :: [Mapping Symbol Type]) :: [Mapping Symbol Type] where
  Intersection t '[] = '[]
  Intersection '[] t = '[]
  Intersection (a ': as) (a ': bs) = a ': Intersection as bs
  Intersection (l ::: a ': as) (r ::: b ': bs) =
    IntersectionCase (CmpSymbol l r) l r a as b bs

type family IntersectionCase (ordering :: Ordering) l r a as b bs where
  IntersectionCase 'LT l r a as b bs = Intersection as (r ::: b ': bs)
  IntersectionCase 'GT l r a as b bs = Intersection (l ::: a ': as) bs
  IntersectionCase 'EQ l r a as a bs = l ::: a ': Intersection as bs
  IntersectionCase 'EQ l r a as b bs =
    TypeError
      ( 'Text "Cannot join on attribute '"
          ':<>: 'Text l
          ':<>: 'Text "'"
          ':$$: 'Text l
          ':<>: 'Text "has type "
          ':<>: 'ShowType a
          ':<>: 'Text " in the first relation and type "
          ':<>: 'ShowType b
          ':<>: 'Text "in the second"
      )
