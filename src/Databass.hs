{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Nicer type-application based API for databass operations
-- placed in a separate module to segregate code using AllowAmbiguousTypes
module Databass (
  module Databass.QueryLanguage,
  createTable,
  initDB,
  insert,
  insertMany,
  insertWithDefault,
  insertManyWithDefault,
  deleteByKey,
  updateTable,
  table,
  rename,
  restrict,
  project,
  join,
  (><),
  qunion,
  difference,
  intersection,
  (\\),
  extend,
  group,
  ungroup,
  summarize,
  summarize',
  (<|),
  t,
  col,
  runQuery,
) where

import qualified Control.Foldl as L
import Control.Lens (Lens, Lens', LensLike, lens)
import qualified Data.Map.Strict as M
import Data.Type.Map hiding ((:\))
import Data.Type.Set (AsSet, Sort, type (:++))
import qualified Databass.MapDB as MapDB
import Databass.QueryLanguage
import GHC.TypeLits
import Relude hiding (Map, get, group, join, put)
import qualified Prelude as P

-- | Intended usage is to have a named table type that you pass to 'createTable'
-- via type applications
--
-- > type UserTable = "users" ::: T '["id" ::: Int, "name" ::: String, "age" ::: Int] '["id"]
-- > createTable @UserTable
createTable ::
  forall namedTable tables name heading k v.
  (namedTable ~ (name ::: Table heading k v), IsHeading heading k v, Ord (Tuple k), Member name tables ~ 'False) =>
  MapDB tables ->
  MapDB ((name ::: Table heading k v) ': tables)
createTable = MapDB.createTable (Var @name) (Proxy @tables) (Proxy @k) (Proxy @v) (MkTable :: Table heading k v)

initDB :: forall tables. (MapDB.InitDB tables) => MapDB tables
initDB = MapDB.initDB (Proxy @tables)

insert ::
  forall name tables heading k v.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Tuple heading ->
  MapDB tables ->
  MapDB tables
insert = MapDB.insert (Var @name) (Proxy @k) (Proxy @tables)

insertWithDefault ::
  forall name cols tables heading k v.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  , Unionable (heading :!! cols) (heading :\\ cols)
  , Union (heading :!! cols) (heading :\\ cols) ~ heading
  , tables :! name ~ Table heading k v
  ) =>
  Tuple (heading :!! cols) ->
  Tuple (heading :\\ cols) ->
  MapDB tables ->
  MapDB tables
insertWithDefault t def = MapDB.insert (Var @name) (Proxy @k) (Proxy @tables) (t `union` def)

insertMany ::
  forall name tables heading k v t.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Foldable t
  , Ord (Tuple k)
  ) =>
  t (Tuple heading) ->
  MapDB tables ->
  MapDB tables
insertMany tuples db = foldr (insert @name @tables @heading @k) db tuples

insertManyWithDefault ::
  forall name cols tables heading k v t.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  , Unionable (heading :!! cols) (heading :\\ cols)
  , Union (heading :!! cols) (heading :\\ cols) ~ heading
  , Foldable t
  , tables :! name ~ Table heading k v
  ) =>
  Tuple (heading :\\ cols) ->
  t (Tuple (heading :!! cols)) ->
  MapDB tables ->
  MapDB tables
insertManyWithDefault def ts db =
  foldr (\t -> insertWithDefault @name @cols @tables @_ @k t def) db ts

insertIncreasing ::
  forall name tables heading k v t.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Foldable t
  , Ord (Tuple k)
  , Enum (Tuple k)
  , Bounded (Tuple k)
  ) =>
  Tuple v ->
  MapDB tables ->
  MapDB tables
insertIncreasing = MapDB.insertIncreasing (Var @name) (Proxy @k) (Proxy @tables)

insertManyIncreasing ::
  forall name tables heading k v t.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Foldable t
  , Ord (Tuple k)
  , Enum (Tuple k)
  , Bounded (Tuple k)
  , Foldable t
  ) =>
  t (Tuple v) ->
  MapDB tables ->
  MapDB tables
insertManyIncreasing vals db =
  foldr (MapDB.insertIncreasing (Var @name) (Proxy @k) (Proxy @tables)) db vals

deleteByKey ::
  forall name tables heading k v.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Tuple k ->
  MapDB tables ->
  MapDB tables
deleteByKey = MapDB.deleteByKey (Var @name) (Proxy @tables) (Proxy @v)

updateTable ::
  forall name tables heading k v.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  (Tuple heading -> Bool) ->
  (Tuple heading -> Tuple heading) ->
  MapDB tables ->
  MapDB tables
updateTable = MapDB.updateTable (Var @name) (Proxy @tables) (Proxy @k) (Proxy @heading)

table ::
  forall name tables heading k v.
  ( (tables :! name) ~ Table heading k v
  , IsMember name (Table heading k v) tables
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , IsHeading heading k v
  ) =>
  Query heading tables
table = TableIdentity Proxy (Var @name) (MkTable :: Table heading k v)

rename :: forall a b t tables. (Sortable (Rename a b t)) => Query t tables -> Query (Sort (Rename a b t)) tables
rename = Rename (Var @a) (Var @b)

restrict :: (Tuple t -> Bool) -> Query t tables -> Query t tables
restrict = Restrict

project ::
  forall (labels :: [Symbol]) heading heading' tables.
  (Submap heading' heading, heading' ~ (heading :!! labels)) =>
  Query heading tables ->
  Query heading' tables
project = Project @heading'

join
  , (><) ::
    forall t' t common t'_rest t_rest tables.
    ( Eq (Tuple (Intersection t' t))
    , common ~ Intersection t' t
    , Split common t'_rest t'
    , Split common t_rest t
    , t'_rest ~ (t' :\\ GetLabels common)
    , t_rest ~ (t :\\ GetLabels common)
    , Sortable (common :++ (t'_rest :++ t_rest))
    ) =>
    Query t' tables ->
    Query t tables ->
    Query (Sort (common :++ (t'_rest :++ t_rest))) tables
join = Join (Proxy @t'_rest) (Proxy @t_rest)
(><) = Join (Proxy @t'_rest) (Proxy @t_rest)

qunion :: Eq (Tuple t) => Query t tables -> Query t tables -> Query t tables
qunion = QueryUnion

intersection :: Eq (Tuple t) => Query t tables -> Query t tables -> Query t tables
intersection = Intersection

difference, (\\) :: Eq (Tuple t) => Query t tables -> Query t tables -> Query t tables
difference = Difference
(\\) = Difference

extend ::
  forall (l :: Symbol) (a :: Type) (t :: [Mapping Symbol Type]) tables.
  (Member l t ~ 'False, Sortable (l ::: a ': t)) =>
  (Tuple t -> a) ->
  Query t tables ->
  Query (Sort (l ::: a ': t)) tables
extend = Extend (Var @l)

summarize ::
  forall l a t t' tables.
  (Submap t' t, Member l t' ~ 'False, Eq (Tuple t'), Sortable (l ::: a ': t')) =>
  Query t' tables ->
  L.Fold (Tuple t) a ->
  Query t tables ->
  Query (Sort (l ::: a ': t')) tables
summarize = Summarize (Var @l)

-- | 'summarize' but the projection is on the same initial query
summarize' ::
  forall l ls a t t' tables.
  (t' ~ (t :!! ls), Submap t' t, Member l t' ~ 'False, Eq (Tuple t'), Sortable (l ::: a ': t')) =>
  L.Fold (Tuple t) a ->
  Query t tables ->
  Query (Sort (l ::: a ': t')) tables
summarize' folder q = Summarize (Var @l) (Project @t' q) folder q

group ::
  forall name (attrs :: [Symbol]) t t' tables grouped rest.
  ( grouped ~ (t :!! attrs)
  , rest ~ (t :\\ attrs)
  , Split grouped rest t
  , Sortable (name ::: [Tuple grouped] ': rest)
  , Ord (Tuple rest)
  ) =>
  Query t tables ->
  Query (Sort (name ::: [Tuple grouped] ': rest)) tables
group = Group (Var @name) (Proxy @grouped) (Proxy @rest)

ungroup ::
  forall l t tables nested rest.
  ( [Tuple nested] ~ (t :! l)
  , rest ~ (t :\ l)
  , Split '[l ::: [Tuple nested]] rest t
  , Sortable (nested :++ rest)
  ) =>
  Query t tables ->
  Query (Sort (nested :++ rest)) tables
ungroup = Ungroup (Var @l) (Proxy @nested) (Proxy @rest)

(<|) :: forall (k :: Symbol) v m. v -> Map m -> Map ((k ':-> v) : m)
(<|) = Ext (Var @k)

infixr 5 <|

-- | 't' is meant to be used with '<|'
--
-- @
-- t @'["id", "age"] (1 <| 25 <| Empty) == asMap $ Ext (Var @"id") 1 $ Ext (Var @"age") 25 Empty
-- @
t :: forall labels s . (Sortable s, Nubable (Sort s), GetLabels s ~ labels) => Map s -> Map (AsMap s)
t = asMap

-- | Lens for getting a column out of a tuple
col ::
  forall (label :: Symbol) (m :: [Mapping Symbol Type]) (n :: [Mapping Symbol Type]) (t :: Type) (t' :: Type).
  ( IsMember label t m
  , t ~ (m :! label)
  , Updatable label t' m n
  , ChangeType label t' m ~ n
  ) =>
  Lens (Tuple m) (Tuple n) t t'
col = lens (lookp (Var @label)) (`update` (Var @label))

runQuery :: MapDB tables -> Query t tables -> [Tuple t]
runQuery = flip MapDB.runQuery

instance
  ( Functor f
  , IsMember label t m
  , t ~ (m :! label)
  , Updatable label t' m n
  , ChangeType label t' m ~ n
  ) =>
  IsLabel label (LensLike f (Tuple m) (Tuple n) t t')
  where
  fromLabel = lens (lookp (Var @label)) (`update` (Var @label))

instance (label ~ l) => IsLabel label (Var l) where
  fromLabel = Var @l
