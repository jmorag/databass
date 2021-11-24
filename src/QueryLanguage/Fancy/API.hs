{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Nicer type-application based API for databass operations
 placed in a separate module to segregate code using AllowAmbiguousTypes
-}
module QueryLanguage.Fancy.API where

import qualified Control.Foldl as L
import Control.Lens (Lens, Lens', lens)
import qualified Data.Map.Strict as M
import Data.Type.Map
import Data.Type.Set (AsSet, Sort, type (:++))
import GHC.TypeLits
import QueryLanguage.Fancy
import Relude hiding (Identity, Map, get, put, undefined)
import qualified Prelude as P

{- | Intended usage is to have a named table type that you pass to 'createTable'
 via type applications

 > type UserTable = "users" ::: T '["id" ::: Int, "name" ::: String, "age" ::: Int] '["id"]
 > createTable @UserTable
-}
createTable ::
  forall namedTable name heading k v tables k_c v_c.
  ( Member name tables ~ 'False
  , IsHeading heading k v
  , namedTable ~ (name ::: Table heading k v)
  ) =>
  DBStatement tables k_c v_c ->
  DBStatement (name ::: Table heading k v ': tables) k_c v_c
createTable = CreateTable

insert ::
  forall name tables heading k v k_c v_c.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , (MapDB' tables :! name) ~ M.Map (Tuple k) (Tuple v)
  , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
  , Updatable name (M.Map (Tuple k) (Tuple v)) (MapDB' tables) (MapDB' tables)
  , k_c (Tuple k)
  , v_c (Tuple v)
  ) =>
  Tuple heading ->
  DBStatement tables k_c v_c ->
  DBStatement tables k_c v_c
insert tuple = TableStatement (Var @name) (Insert tuple :: TableOp heading k v)

insertMany ::
  forall name tables heading k v t k_c v_c.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , (MapDB' tables :! name) ~ M.Map (Tuple k) (Tuple v)
  , IsMember name (M.Map (Tuple k) (Tuple v)) (MapDB' tables)
  , Updatable name (M.Map (Tuple k) (Tuple v)) (MapDB' tables) (MapDB' tables)
  , Foldable t
  , k_c (Tuple k)
  , v_c (Tuple v)
  ) =>
  t (Tuple heading) ->
  DBStatement tables k_c v_c ->
  DBStatement tables k_c v_c
insertMany tuples db = foldr (insert @name) db tuples

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

project ::
  forall (labels :: [Symbol]) heading heading' tables.
  (Submap heading' heading, heading' ~ (heading :!! labels)) =>
  Query heading tables ->
  Query heading' tables
project = Project @heading'

(><) ::
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

-- for the repl
testQuery :: Show (Tuple t) => DBStatement tables Ord v_c -> Query t tables -> IO ()
testQuery db q = runQuery q (materializeMapDB db) & mapM_ Relude.print
