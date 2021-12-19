{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Nicer type-application based API for databass operations
-- placed in a separate module to segregate code using AllowAmbiguousTypes
module Databass (
  module Databass.QueryLanguage,
  createTable,
  initDB,
  insert,
  insertMany,
  deleteByKey,
  updateTable,
  table,
  rename,
  restrict,
  project,
  join,
  (><),
  extend,
  group,
  ungroup,
  summarize,
  (<|),
  col,
  runQuery,
) where

import qualified Control.Foldl as L
import Control.Lens (Lens, Lens', lens)
import qualified Data.Map.Strict as M
import Data.Type.Map
import Data.Type.Set (AsSet, Sort, type (:++))
import qualified Databass.MapDB as MapDB
import Databass.QueryLanguage
import GHC.TypeLits
import Relude hiding (Identity, Map, get, join, put, undefined, group)
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
createTable = MapDB.createTable (Var @name) (Proxy @tables) (MkTable :: Table heading k v)

initDB :: forall tables. (MapDB.InitDB tables) => MapDB tables
initDB = MapDB.initDB (Proxy @tables)

insert ::
  forall name tables heading k v.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ M.Map (Tuple k) (Tuple v)
  , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
  , Updatable name (M.Map (Tuple k) (Tuple v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Tuple heading ->
  MapDB tables ->
  MapDB tables
insert = MapDB.insert (Var @name) (Proxy @tables)

insertMany ::
  forall name tables heading k v t.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , (MapDB' tables :! name) ~ M.Map (Tuple k) (Tuple v)
  , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
  , Updatable name (M.Map (Tuple k) (Tuple v)) (MapDB' tables) (MapDB' tables)
  , Foldable t
  , Ord (Tuple k)
  ) =>
  t (Tuple heading) ->
  MapDB tables ->
  MapDB tables
insertMany tuples db = foldr (insert @name @tables) db tuples

deleteByKey ::
  forall name tables heading k v.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ M.Map (Tuple k) (Tuple v)
  , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
  , Updatable name (M.Map (Tuple k) (Tuple v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Tuple k ->
  MapDB tables ->
  MapDB tables
deleteByKey = MapDB.deleteByKey (Var @name) (Proxy @tables)

updateTable ::
  forall name tables heading k v.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ M.Map (Tuple k) (Tuple v)
  , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
  , Updatable name (M.Map (Tuple k) (Tuple v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  (Tuple heading -> Bool) ->
  (Tuple heading -> Tuple heading) ->
  MapDB tables ->
  MapDB tables
updateTable = MapDB.updateTable (Var @name) (Proxy @tables)

table ::
  forall name tables heading k v.
  ( (tables :! name) ~ Table heading k v
  , IsMember name (Table heading k v) tables
  , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
  , IsHeading heading k v
  ) =>
  Query heading tables
table = Identity (Var @name) (MkTable :: Table heading k v)

rename :: forall a b t tables. Query t tables -> Query (Rename a b t) tables
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
join = Join
(><) = Join

extend ::
  forall (l :: Symbol) (a :: Type) (t :: [Mapping Symbol Type]) tables.
  (Member l t ~ 'False) =>
  (Tuple t -> a) ->
  Query t tables ->
  Query (l ::: a ': t) tables
extend = Extend Var

summarize ::
  forall l a t t' tables.
  (Submap t' t, Member l t' ~ 'False, Eq (Tuple t')) =>
  Query t' tables ->
  L.Fold (Tuple t) a ->
  Query t tables ->
  Query (l ::: a ': t') tables
summarize = Summarize (Var @l)

group ::
  forall name (attrs :: [Symbol]) t t' tables grouped rest.
  (grouped ~ (t :!! attrs), rest ~ (t :\\ attrs), Split grouped rest t) =>
  Query t tables ->
  Query (name ::: Tuple grouped ': rest) tables
group = Group (Var @name) (Proxy @attrs)

ungroup ::
  forall l t tables nested rest.
  ( Tuple nested ~ (t :! l)
  , IsMember l (Tuple nested) t
  , rest ~ (t :\ l)
  , Submap rest t
  ) =>
  Query t tables ->
  Query (nested :++ rest) tables
ungroup = Ungroup (Var @l) (Proxy @nested) Proxy

(<|) :: forall (k :: Symbol) v m. v -> Map m -> Map ((k ':-> v) : m)
(<|) = Ext (Var @k)

infixr 5 <|

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

{- TODO: investigate a quasiquoter for creating tuples
[row_auto_incr S# start 1|SNAME STATUS CITY
     1  Smith 20     London
     2  Jones Blake
|]
-}

runQuery :: MapDB tables -> Query t tables -> [Tuple t]
runQuery = flip MapDB.runQuery
