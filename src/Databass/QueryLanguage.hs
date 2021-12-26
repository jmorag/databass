-- | Embedding of relational model as per chapter 2 of third manifesto
module Databass.QueryLanguage (
  Tuple,
  (:::),
  GetLabels,
  T,
  IsHeading,
  Table (..),
  Rename,
  (:\\),
  (:\),
  (:!),
  (:!!),
  Intersection,
  Query(..),
  MapDB,
  MapDB',
  ChangeType,
  colLens'
) where

import qualified Control.Foldl as L
import Control.Lens (Lens', lens)
import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import Data.Binary
import Data.Binary.Get (getInt64le, isolate)
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as M
import Data.Type.Map
import Data.Type.Set (AsSet, Sort, type (:++))
import GHC.TypeLits
import Relude hiding (Identity, Map, get, put)

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

class ToJSON' (a :: [Mapping Symbol Type]) where
  toJSON' :: Tuple a -> [Pair]
  toEncoding' :: Tuple a -> Series

instance ToJSON' '[] where
  toJSON' Empty = []
  toEncoding' Empty = mempty

instance
  (KnownSymbol label, ToJSON' as, ToJSON a) =>
  ToJSON' (label ::: a ': as)
  where
  toJSON' (Ext _ x xs) =
    fromString (symbolVal (Proxy @label)) .= x : toJSON' xs
  toEncoding' (Ext _ x xs) =
    fromString (symbolVal (Proxy @label)) .= x <> toEncoding' xs

instance ToJSON' xs => ToJSON (Tuple xs) where
  toJSON = object . toJSON'
  toEncoding = pairs . toEncoding'

class FromJSON' (xs :: [Mapping Symbol Type]) where
  parseObject :: Object -> Parser (Tuple xs)

instance FromJSON' '[] where
  parseObject o = pure Empty

instance
  (FromJSON x, KnownSymbol label, FromJSON' xs) =>
  FromJSON' (label ::: x ': xs)
  where
  parseObject o =
    Ext (Var @label)
      <$> (o .: fromString (symbolVal (Proxy @label)))
      <*> parseObject o

instance FromJSON' xs => FromJSON (Tuple xs) where
  parseJSON = withObject "Tuple" parseObject

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
    ( common ~ Intersection t' t
    , Eq (Tuple common)
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

type family MapDB tables where
  MapDB tables = Tuple (MapDB' tables)

type family MapDB' tables where
  MapDB' '[] = '[]
  MapDB' (name ::: Table heading k v ': rest) = (name ::: M.Map (Tuple k) (Tuple v)) ': MapDB' rest

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