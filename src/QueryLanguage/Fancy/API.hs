{-# LANGUAGE AllowAmbiguousTypes #-}

{- | Nicer type-application based API for databass operations
 placed in a separate module to segregate code using AllowAmbiguousTypes
-}
module QueryLanguage.Fancy.API where

import Control.Lens (Lens, Lens', lens)
import Data.Type.Map
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

project ::
  forall (labels :: [Symbol]) heading heading' tables.
  (Submap heading' heading, heading' ~ (heading :!! labels)) =>
  Query heading tables ->
  Query heading' tables
project = Project @heading'

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
