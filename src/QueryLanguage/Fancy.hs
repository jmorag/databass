-- | Embedding of relational model as per chapter 2 of third manifesto
module QueryLanguage.Fancy where

import qualified Control.Foldl as L
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
  , Binary (Tuple heading)
  , Binary (Tuple k)
  , Binary (Tuple v)
  )

data Table heading k v where
  MkTable :: forall (heading :: [Mapping Symbol Type]) k v. (IsHeading heading k v) => Table heading k v

type family IsKey (labels :: [Symbol]) (t :: [Mapping Symbol Type]) :: Constraint where
  IsKey (label ': ls) (label ::: a ': rest) = IsKey ls rest
  IsKey (label ': ls) (a ': rest) = IsKey (label ': ls) rest
  IsKey '[] tuple = ()
  IsKey labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels)

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
  Identity :: ((tables :! name) ~ Table heading k v, KnownSymbol name) => Var name -> Table heading k v -> Query heading tables
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

data Database (tables :: [Mapping Symbol Type]) where
  EmptyDB :: Database '[]
  CreateTable ::
    (Member name tables ~ 'False, KnownSymbol name) =>
    Var name ->
    Table heading k v ->
    Database tables ->
    Database ((name ::: Table heading k v) ': tables)
  DeleteTable ::
    forall (name :: Symbol) tables remaining.
    (KnownSymbol name, Member name tables ~ 'True, (tables :\ name) ~ remaining) =>
    Var name ->
    Database tables ->
    Database remaining
  Insert ::
    ((tables :! name) ~ Table heading k v, KnownSymbol name) =>
    Var name ->
    Table heading k v ->
    Tuple heading ->
    Database tables ->
    Database tables

type family TableHeading table :: [Mapping Symbol Type] where
  TableHeading (Table heading k v) = heading
  TableHeading a = TypeError ( 'Text "Can only call 'TableHeading' on Table, not " ':$$: 'ShowType a)

newtype MemDB tables = MemDB {getMemDB :: M.Map String (M.Map LByteString LByteString)}
  deriving (Show)

runQuery ::
  forall t tables tables'.
  (Sort tables ~ Sort tables') =>
  Query t tables ->
  MemDB tables' ->
  [Tuple t]
runQuery q mem = case q of
  Identity name (MkTable :: Table heading k v) ->
    M.toList (getMemDB mem M.! str name) & map \(k, v) -> (decode k :: Tuple k) `union` (decode v :: Tuple v)
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

materializeDB :: forall tables. Database tables -> MemDB tables
materializeDB EmptyDB = MemDB mempty
materializeDB (CreateTable name MkTable rest) =
  MemDB $
    M.insert (str name) mempty (getMemDB (materializeDB rest))
materializeDB (DeleteTable name rest) = MemDB $ M.delete (str name) (getMemDB $ materializeDB rest)
materializeDB (Insert name (MkTable :: Table heading k v) t rest) =
  MemDB $
    M.update (Just . M.insert (encode key) (encode val)) (str name) (getMemDB $ materializeDB rest)
  where
    kv :: (Split k v heading) => (Tuple k, Tuple v)
    kv = split t
    (key, val) = kv

str :: forall k. KnownSymbol k => Var k -> String
str Var = symbolVal (Proxy @k)
