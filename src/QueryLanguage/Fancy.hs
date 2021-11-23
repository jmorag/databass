-- | Embedding of relational model as per chapter 2 of third manifesto
module QueryLanguage.Fancy where

import qualified Control.Foldl as L
import Control.Lens (Lens', lens, over)
import Data.Binary
import Data.Binary.Get (getInt64le, isolate)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import Data.List (partition)
import qualified Data.Map.Strict as M
import Data.Type.Map
import Data.Type.Set (AsSet, Sort, type (:++))
import GHC.TypeLits
import Relude hiding (Identity, Map, get, put, undefined)
import Unsafe.Coerce (unsafeCoerce)
import qualified Prelude as P

type Tuple = Map

-- | Kind constrained version of :-> that binds more tightly
type (k :: Symbol) ::: (v :: Type) = k ':-> v

infixr 6 :::

instance Binary (Tuple '[]) where
  put Empty = pure ()
  get = pure Empty

-- TODO, compactify serialization format. 64 bits for length is usually too much
instance (Binary x, Binary (Tuple ts)) => Binary (Tuple (l ::: x ': ts)) where
  put (Ext _ x xs) = do
    let bytes = runPut $ put x
    putInt64le (BL.length bytes)
    put x
    put xs
  get = do
    size <- getInt64le
    x <- isolate (fromIntegral size) get
    Ext Var x <$> get

type family GetAttr attr where
  GetAttr (l ::: a) = a
  GetAttr x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

type family GetLabel attr where
  GetLabel (l ::: a) = l
  GetLabel x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

type family GetLabels attrs where
  GetLabels '[] = '[]
  GetLabels (l ::: a ': xs) = l ': GetLabels xs
  GetLabels x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

type family T heading (key :: [Symbol]) where
  T heading key = Table (AsMap heading) (AsMap heading :!! AsSet key) (AsMap heading :\\ AsSet key)

type IsHeading heading k v =
  ( Submap k heading
  , Submap v heading
  , Split k v heading
  , Unionable k v
  , Union k v ~ heading
  )

data Table heading k v = (IsHeading heading k v) => MkTable

data TableOp heading k v = Insert (Tuple heading) | DeleteByKey (Tuple k)

type family Rename (a :: Symbol) (b :: Symbol) (t :: [Mapping Symbol Type]) :: [Mapping Symbol Type] where
  Rename a b '[] = '[]
  Rename a b ((a ::: t) ': rest) = (b ::: t) ': rest
  Rename a b (c ': rest) = c ': Rename a b rest

-- | Delete multiple elements from a map by key O(n^2)
type family (m :: [Mapping Symbol Type]) :\\ (cs :: [Symbol]) :: [Mapping Symbol Type] where
  m :\\ (label ': ls) = (m :\ label) :\\ ls
  m :\\ '[] = m

-- | Type level key lookup
type family (m :: [Mapping Symbol Type]) :! (c :: Symbol) :: Type where
  (label ::: a ': rest) :! label = a
  (attr ': rest) :! label = rest :! label
  '[] :! label = TypeError ( 'Text "Could not find " ':<>: 'ShowType label)

-- | Type level multi-key lookup from a map O(n^2)
type family (m :: [Mapping Symbol Type]) :!! (cs :: [Symbol]) :: [Mapping Symbol Type] where
  m :!! (label ': ls) = (label ::: (m :! label)) ': (m :!! ls)
  m :!! '[] = '[]

type family Intersection t t' where
  Intersection t t' = Intersection' (Sort t) (Sort t')

type family Intersection' (t :: [Mapping Symbol Type]) (t' :: [Mapping Symbol Type]) :: [Mapping Symbol Type] where
  Intersection' t '[] = '[]
  Intersection' '[] t = '[]
  Intersection' (a ': as) (a ': bs) = a ': Intersection' as bs
  Intersection' (l ::: a ': as) (r ::: b ': bs) = Intersection'Case (CmpSymbol l r) l r a as b bs

type family Intersection'Case (ordering :: Ordering) l r a as b bs where
  Intersection'Case 'LT l r a as b bs = Intersection' as (r ::: b ': bs)
  Intersection'Case 'GT l r a as b bs = Intersection' (l ::: a ': as) bs
  Intersection'Case 'EQ l r a as b bs = TypeError ( 'Text "Unreachable, oh god please")

data Query (t :: [Mapping Symbol Type]) (tables :: [Mapping Symbol Type]) where
  Identity ::
    ( (tables :! name) ~ Table heading k v
    , IsMember name (Table heading k v) tables
    , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
    ) =>
    Var name ->
    Table heading k v ->
    Query heading tables
  Rename :: forall a b t tables. Var a -> Var b -> Query t tables -> Query (Rename a b t) tables
  Restrict :: (Tuple t -> Bool) -> Query t tables -> Query t tables
  Project :: (Submap t' t) => Query t tables -> Query t' tables
  Join ::
    ( Eq (Tuple (Intersection t' t))
    , common ~ Intersection t' t
    , Submap common t'
    , Submap common t
    , Submap t'_rest t'
    , Submap t_rest t
    , t'_rest ~ (t' :\\ GetLabels common)
    , t_rest ~ (t :\\ GetLabels common)
    ) =>
    Query t' tables ->
    Query t tables ->
    Query (common :++ (t'_rest :++ t_rest)) tables
  -- Union :: Query t tables -> Query t tables -> Query t tables
  -- Intersection :: Query t tables -> Query t tables -> Query t tables
  -- Difference :: Query t tables -> Query t tables -> Query t tables
  Extend ::
    forall (l :: Symbol) (a :: Type) (t :: [Mapping Symbol Type]) tables.
    (Member l t ~ 'False) =>
    Var l ->
    (Tuple t -> a) ->
    Query t tables ->
    Query (l ::: a ': t) tables
  Summarize ::
    forall l a t t' tables.
    (Submap t' t, Member l t' ~ 'False, Eq (Tuple t')) =>
    Var l ->
    Query t' tables ->
    L.Fold (Tuple t) a ->
    Query t tables ->
    Query (l ::: a ': t') tables
  Group ::
    forall (l :: Symbol) (attrs :: [Symbol]) (t :: [Mapping Symbol Type]) grouped rest tables.
    (grouped ~ (t :!! attrs), rest ~ (t :\\ attrs), Split grouped rest t) =>
    Var l ->
    Proxy attrs ->
    Query t tables ->
    Query (l ::: Tuple grouped ': rest) tables
  Ungroup ::
    forall l t tables nested rest.
    (IsMember l (Tuple nested) t, rest ~ (t :\ l), Submap rest t) =>
    Var l ->
    Proxy nested ->
    Proxy rest ->
    Query t tables ->
    Query (nested :++ rest) tables

{- | Heterogeneous list of database statements. 'k_c' and 'v_c' signify
 constraints needed to materialize table contents in bits. For example, the
 MapDB representation using 'M.Map' from containers with the primary key of
 the table as the map key needs '(k_c ~ Ord)' and can leave v_c unspecified.
 To serialize the database to a b-tree on disk, you would need
 '(k_c ~ Binary, v_c ~ Binary)'.
-}
data
  DBStatement
    (tables :: [Mapping Symbol Type])
    (k_c :: Type -> Constraint)
    (v_c :: Type -> Constraint)
  where
  EmptyDB :: DBStatement '[] k_c v_c
  CreateTable ::
    (Member name tables ~ 'False, IsHeading heading k v) =>
    DBStatement tables k_c v_c ->
    DBStatement ((name ::: Table heading k v) ': tables) k_c v_c
  DeleteTable ::
    forall (name :: Symbol) tables remaining k_c v_c.
    (Member name tables ~ 'True, (tables :\ name) ~ remaining, Submap (MapDB' remaining) (MapDB' tables)) =>
    Var name ->
    Proxy remaining ->
    DBStatement tables k_c v_c ->
    DBStatement remaining k_c v_c
  TableStatement ::
    ( IsHeading heading k v
    , (MapDB' tables :! name) ~ M.Map (Tuple k) (Tuple v)
    , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
    , Updatable name (M.Map (Tuple k) (Tuple v)) (MapDB' tables) (MapDB' tables)
    , k_c (Tuple k)
    , v_c (Tuple v)
    ) =>
    Var name ->
    TableOp heading k v ->
    DBStatement tables k_c v_c ->
    DBStatement tables k_c v_c

type family MapDB tables where
  MapDB tables = Tuple (MapDB' tables)

type family MapDB' tables where
  MapDB' '[] = '[]
  MapDB' (name ::: Table heading k v ': rest) = (name ::: M.Map (Tuple k) (Tuple v)) ': MapDB' rest

runQuery :: forall tables t. Query t tables -> MapDB tables -> [Tuple t]
runQuery q mem = case q of
  Identity name (MkTable :: Table heading k v) ->
    M.toList (lookp name mem) & map \(k, v) -> (k :: Tuple k) `union` (v :: Tuple v)
  Rename Var Var q' -> unsafeCoerce $ runQuery q' mem
  Restrict pred q' -> filter pred (runQuery q' mem)
  Project q' -> map submap (runQuery q' mem)
  -- TODO: This is the most naive possible nested loop O(n*m) join algorithm
  -- See https://en.wikipedia.org/wiki/Category:Join_algorithms for more ideas
  Join q1 q2 -> do
    l :: Tuple t_l <- runQuery q1 mem
    r :: Tuple t_r <- runQuery q2 mem
    let l_common = submap @(Intersection t_l t_r) l
        r_common = submap @(Intersection t_l t_r) r
        l_rest = submap @(t_l :\\ GetLabels (Intersection t_l t_r)) l
        r_rest = submap @(t_r :\\ GetLabels (Intersection t_l t_r)) r
    guard (l_common == r_common)
    pure (append l_common (append l_rest r_rest))
  Extend var f q -> runQuery q mem & map \tuple -> Ext var (f tuple) tuple
  Summarize var projection folder q ->
    let proj = runQuery projection mem
        tuples = runQuery q mem
     in go proj tuples
    where
      go [] _ = []
      go (p : ps) tuples =
        let (these, rest) = partition (\tuple -> p == submap tuple) tuples
         in Ext var (L.fold folder these) p : go ps rest
  Group var _ q ->
    runQuery q mem & map \tuple ->
      let (grouped, rest) = split tuple
       in Ext var grouped rest
  Ungroup var (_ :: Proxy nested) (_ :: Proxy rest) q ->
    runQuery q mem & map \tuple ->
      let nested = lookp @_ @(Tuple nested) var tuple
          rest = submap @rest tuple
       in append nested rest

class Unconstrained a
instance Unconstrained a

materializeMapDB :: forall tables v_c. DBStatement tables Ord v_c -> MapDB tables
materializeMapDB EmptyDB = Empty
materializeMapDB (CreateTable rest) = Ext Var M.empty (materializeMapDB rest)
materializeMapDB (DeleteTable _ (_ :: Proxy remaining) rest) = submap @(MapDB' remaining) (materializeMapDB rest)
materializeMapDB (TableStatement name (s :: TableOp heading k v) rest) = over (colLens' name) (tableUpdateMap s) (materializeMapDB rest)

-- | Update the type at label l
type family ChangeType (l :: Symbol) (t' :: Type) (t :: [Mapping Symbol Type]) where
  ChangeType l a (l ::: b ': rest) = l ::: a ': rest
  ChangeType l a (l' ::: b ': rest) = l' ::: b ': ChangeType l a rest
  ChangeType l a '[] = '[]

colLens' ::
  forall (label :: Symbol) m t.
  (IsMember label t m, t ~ (m :! label), Updatable label t m m) =>
  Var label ->
  Lens' (Tuple m) t
colLens' var = lens (lookp var) (`update` var)

tableUpdateMap ::
  (IsHeading heading k v, Ord (Tuple k)) =>
  TableOp heading k v ->
  M.Map (Tuple k) (Tuple v) ->
  M.Map (Tuple k) (Tuple v)
tableUpdateMap (Insert tuple) =
  let (key, val) = split tuple in M.insert key val
tableUpdateMap (DeleteByKey key) = M.delete key
