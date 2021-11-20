{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Nicer type-application based API for databass operations
 placed in a separate module to segregate code using AllowAmbiguousTypes
-}
module QueryLanguage.Fancy.API where

import qualified Control.Foldl as L
import Control.Lens (Lens, Lens', lens)
import Data.Type.Map
import Data.Type.Set (AsSet, Sort, type (:++))
import GHC.TypeLits
import QueryLanguage.Fancy
import Relude hiding (Identity, Map, get, put, undefined)

createTable ::
  forall table name heading k v tables.
  ( IsHeading heading k v
  , Member name tables ~ 'False
  , KnownSymbol name
  , table ~ (name ::: Table heading k v)
  ) =>
  Database tables ->
  Database (name ::: Table heading k v ': tables)
createTable = CreateTable (Var @name) (MkTable :: Table heading k v)

insert ::
  forall name tables heading k v.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , KnownSymbol name
  ) =>
  Tuple (TableHeading (tables :! name)) ->
  Database tables ->
  Database tables
insert = Insert (Var @name) MkTable

insertMany ::
  forall name tables heading k v t.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , KnownSymbol name
  , Foldable t
  ) =>
  t (Tuple (TableHeading (tables :! name))) ->
  Database tables ->
  Database tables
insertMany tuples db = foldr (insert @name) db tuples

table ::
  forall name tables heading k v.
  ( IsHeading heading k v
  , Table heading k v ~ (tables :! name)
  , KnownSymbol name
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

summarize ::
  forall l a t t' tables.
  (Submap t' t, Member l t' ~ 'False) =>
  Query t' tables ->
  L.Fold (Tuple t') a ->
  Query t tables ->
  Query (Sort (l ::: a ': t')) tables
summarize = Summarize (Var @l)

group ::
  forall name (attrs :: [Symbol]) t t' tables.
  Query t tables ->
  Query (Sort (name ::: Query (t :!! attrs) tables ': (t :\\ attrs))) tables
group = Group (Var @name) (Proxy @attrs)

ungroup ::
  forall l t tables.
  Query t tables ->
  Query (Sort (UnNest (l ::: (t :! l)) :++ (t :\ l))) tables
ungroup = Ungroup (Var @l)

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
