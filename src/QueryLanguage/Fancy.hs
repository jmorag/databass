{-# LANGUAGE UndecidableInstances #-}

-- | Embedding of relational model as per chapter 2 of third manifesto
module QueryLanguage.Fancy where

import qualified Control.Foldl as L
import Control.Lens (Lens, Lens', lens)
import Data.Binary
import Data.Binary.Get (getInt64le, isolate)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Type.Map
import Data.Type.Set (AsSet, Sort, type (:++))
import GHC.TypeLits
import Relude hiding (Identity, Map, get, put, undefined)
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)

(|>) :: forall (k :: Symbol) v m. v -> Map m -> Map ((k ':-> v) : m)
(|>) = Ext (Var @k)

infixr 5 |>

instance IsLabel k (v -> Map m -> Map ((k ':-> v) : m)) where
  fromLabel = Ext (Var @k)

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
  GetAttr x = TypeError ('ShowType x ':$$: 'Text " is not an attribute")

type family GetLabel attr where
  GetLabel (l ::: a) = l
  GetLabel x = TypeError ('ShowType x ':$$: 'Text " is not an attribute")

type family GetLabels attrs where
  GetLabels '[] = '[]
  GetLabels (l ::: a ': xs) = l ': GetLabels xs
  GetLabels x = TypeError ('ShowType x ':$$: 'Text " is not an attribute")

type family T heading (key :: [Symbol]) where
  T heading key = Table (AsMap heading) (AsMap heading :!! AsSet key) (AsMap heading :\\ AsSet key)

type IsHeading heading k v =
  ( Submap k heading,
    Submap v heading,
    Split k v heading,
    Unionable k v,
    Union k v ~ heading,
    Binary (Tuple heading),
    Binary (Tuple k),
    Binary (Tuple v)
  )

data Table heading k v where
  MkTable :: forall (heading :: [Mapping Symbol Type]) k v. (IsHeading heading k v) => Table heading k v

type family IsKey (labels :: [Symbol]) (t :: [Mapping Symbol Type]) :: Constraint where
  IsKey (label ': ls) (label ::: a ': rest) = IsKey ls rest
  IsKey (label ': ls) (a ': rest) = IsKey (label ': ls) rest
  IsKey '[] tuple = ()
  IsKey labels '[] = TypeError ('Text "Could not find " ':<>: 'ShowType labels)

type family Rename (a :: Symbol) (b :: Symbol) (t :: [Mapping Symbol Type]) :: [Mapping Symbol Type] where
  Rename a b '[] = '[]
  Rename a b ((a ::: t) ': rest) = (b ::: t) ': rest
  Rename a b (c ': rest) = c ': Rename a b rest

type family ChangeType (l :: Symbol) (t' :: Type) (t :: [Mapping Symbol Type]) where
  ChangeType l a (l ::: b ': rest) = l ::: a ': rest
  ChangeType l a (l' ::: b ': rest) = l' ::: b ': ChangeType l a rest
  ChangeType l a '[] = '[]

-- | Delete multiple elements from a map by key
type family (m :: [Mapping Symbol Type]) :\\ (cs :: [Symbol]) :: [Mapping Symbol Type] where
  (label ::: a ': rest) :\\ (label ': ls) = rest :\\ ls
  (a ': rest) :\\ (label ': ls) = a ': rest :\\ (label ': ls)
  tuple :\\ '[] = tuple
  '[] :\\ labels = TypeError ('Text "Could not find " ':<>: 'ShowType labels)

-- | Type level key lookup
type family (m :: [Mapping Symbol Type]) :! (c :: Symbol) :: Type where
  (label ::: a ': rest) :! label = a
  (attr ': rest) :! label = rest :! label
  '[] :! label = TypeError ('Text "Could not find " ':<>: 'ShowType label)

-- | Type level multi-key lookup from a map (O(n^2))
type family (m :: [Mapping Symbol Type]) :!! (cs :: [Symbol]) :: [Mapping Symbol Type] where
  m :!! (label ': ls) = (label ::: (m :! label)) ': (m :!! ls)
  m :!! '[] = '[]

type family UnNest t where
  UnNest (l ::: Query ts tables) = ts
  UnNest (l ::: t) = TypeError ('ShowType (l ::: t) ':<>: 'Text " is not relation valued")
  UnNest x = TypeError ('ShowType x ':$$: 'Text " is not an attribute")

data Query (t :: [Mapping Symbol Type]) (tables :: [Mapping Symbol Type]) where
  Identity :: ((tables :! name) ~ Table heading k v, KnownSymbol name) => Var name -> Table heading k v -> Query heading tables
  Rename :: forall a b t tables. Var a -> Var b -> Query t tables -> Query (Rename a b t) tables
  Restrict :: (Tuple t -> Bool) -> Query t tables -> Query t tables
  Project :: (Submap t' t) => Query t tables -> Query t' tables
  Join :: Query t' tables -> Query t tables -> Query (Union t' t) tables
  Union :: Query t tables -> Query t tables -> Query t tables
  Intersection :: Query t tables -> Query t tables -> Query t tables
  Difference :: Query t tables -> Query t tables -> Query t tables
  Extend ::
    forall (l :: Symbol) (a :: Type) (t :: [Mapping Symbol Type]) tables.
    (Member l t ~ 'False) =>
    Var l ->
    (Tuple t -> a) ->
    Query t tables ->
    Query (Sort (l ::: a ': t)) tables
  Summarize ::
    forall l a t t' tables.
    (Submap t' t, Member l t' ~ 'False) =>
    Var l ->
    Query t' tables ->
    L.Fold (Tuple t') a ->
    Query t tables ->
    Query (Sort (l ::: a ': t')) tables
  Group ::
    forall (l :: Symbol) (attrs :: [Symbol]) (t :: [Mapping Symbol Type]) tables.
    Var l ->
    Proxy attrs ->
    Query t tables ->
    Query (Sort (l ::: Query (AsMap t :!! AsSet attrs) tables ': AsSet t :\\ AsSet attrs)) tables
  Ungroup ::
    forall l t tables.
    Var l ->
    Query t tables ->
    Query (Sort (UnNest (l ::: (t :! l)) :++ t :\ l)) tables

instance (Typeable heading) => Show (Query heading tables) where
  show _ = "\nTODO Show contents" & go (typeRep @heading)
    where
      go :: TypeRep a -> ShowS
      go (Con nil) | tyConName nil == "'[]" = ("|" <>)
      go (App (App _ (App (App _ label) ty)) rest) =
        ("| " <>) . shows label . (" " <>) . go ty . (" " <>) . go rest
      go (App (Con relation) nested)
        | tyConName relation == "Query" =
          ("(" <>) . go nested . (")" <>)
      go other = shows other

-- | Lens for getting a column out of a tuple
col ::
  forall (label :: Symbol) (m :: [Mapping Symbol Type]) (n :: [Mapping Symbol Type]) (t :: Type) (t' :: Type).
  ( IsMember label t m,
    t ~ (m :! label),
    Updatable label t' m n,
    ChangeType label t' m ~ n
  ) =>
  Var label ->
  Lens (Tuple m) (Tuple n) t t'
col var = lens (lookp var) (`update` var)

-- asMap' :: forall (l :: [Symbol]) (s :: [Mapping Symbol Type]). _
-- asMap' = asMap

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
  TableHeading a = TypeError ('Text "Can only call 'TableHeading' on Table, not " ':$$: 'ShowType a)

runQuery ::
  forall t tables1 tables2.
  (Sort tables1 ~ Sort tables2) =>
  Query t tables1 ->
  Database tables2 ->
  [Tuple t]
runQuery q db =
  let mem = materializeDB db
   in case q of
        Identity name (MkTable :: Table heading k v) ->
          M.toList (mem M.! str name) & map \(k, v) -> (decode k :: Tuple k) `union` (decode v :: Tuple v)
        Rename Var Var q' -> unsafeCoerce $ runQuery q' db
        Restrict pred q' -> filter pred (runQuery q' db)
        Project q' -> map submap (runQuery q' db)
        -- Join q1 q2 ->
        _ -> []

materializeDB :: forall tables. Database tables -> M.Map String (M.Map LByteString LByteString)
materializeDB EmptyDB = mempty
materializeDB (CreateTable name MkTable rest) =
  M.insert (str name) mempty (materializeDB rest)
materializeDB (DeleteTable name rest) = M.delete (str name) (materializeDB rest)
materializeDB (Insert name (MkTable :: Table heading k v) t rest) =
  M.update (Just . M.insert (encode key) (encode val)) (str name) (materializeDB rest)
  where
    kv :: (Split k v heading) => (Tuple k, Tuple v)
    kv = split t
    (key, val) = kv

str :: forall k. KnownSymbol k => Var k -> String
str Var = symbolVal (Proxy @k)
