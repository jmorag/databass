{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Embedding of relational model as per chapter 2 of third manifesto
module QueryLanguage.Fancy2 where

-- The running example
-- ╔═════════════════════════════════════════════════════════════════╗
-- ║     S                                          SP               ║
-- ║    ┌────┬───────┬────────┬────────┐           ┌────┬────┬─────┐ ║
-- ║    │ S# │ SNAME │ STATUS │ CITY   │           │ S# │ P# │ QTY │ ║
-- ║    ├════┼───────┼────────┼────────┤           ├════┼════┼─────┤ ║
-- ║    │ S1 │ Smith │     20 │ London │           │ S1 │ P1 │ 300 │ ║
-- ║    │ S2 │ Jones │     10 │ Paris  │           │ S1 │ P2 │ 200 │ ║
-- ║    │ S3 │ Blake │     30 │ Paris  │           │ S1 │ P3 │ 400 │ ║
-- ║    │ S4 │ Clark │     20 │ London │           │ S1 │ P4 │ 200 │ ║
-- ║    │ S5 │ Adams │     30 │ Athens │           │ S1 │ P5 │ 100 │ ║
-- ║    └────┴───────┴────────┴────────┘           │ S1 │ P6 │ 100 │ ║
-- ║     P                                         │ S2 │ P1 │ 300 │ ║
-- ║    ┌────┬───────┬───────┬────────┬────────┐   │ S2 │ P2 │ 400 │ ║
-- ║    │ P# │ PNAME │ COLOR │ WEIGHT │ CITY   │   │ S3 │ P2 │ 200 │ ║
-- ║    ├════┼───────┼───────┼────────┼────────┤   │ S4 │ P2 │ 200 │ ║
-- ║    │ P1 │ Nut   │ Red   │   12.0 │ London │   │ S4 │ P4 │ 300 │ ║
-- ║    │ P2 │ Bolt  │ Green │   17.0 │ Paris  │   │ S4 │ P5 │ 400 │ ║
-- ║    │ P3 │ Screw │ Blue  │   17.0 │ Oslo   │   └────┴────┴─────┘ ║
-- ║    │ P4 │ Screw │ Red   │   14.0 │ London │                     ║
-- ║    │ P5 │ Cam   │ Blue  │   12.0 │ Paris  │                     ║
-- ║    │ P6 │ Cog   │ Red   │   19.0 │ London │                     ║
-- ║    └────┴───────┴───────┴────────┴────────┘                     ║
-- ╚═════════════════════════════════════════════════════════════════╝

-- VAR S REAL RELATION  { S# S#, SNAME NAME, STATUS INTEGER, CITY CHAR } KEY { S# } ;
-- VAR P REAL RELATION  { P# P#, PNAME NAME, COLOR COLOR, WEIGHT WEIGHT, CITY CHAR } KEY { P# } ;
-- VAR SP REAL RELATION { S# S#, P# P#, QTY QTY } KEY { S#, P# } ;

-- import Prelude.Singletons

import Data.Map.Strict qualified as M
import Data.Singletons.Base.TypeError
import GHC.TypeLits.Singletons
import Relude hiding (show)
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)

newtype Attribute (label :: Symbol) (a :: Type) = Attr {getAttr :: a}
  deriving (Eq, Ord)

instance (KnownSymbol label, Show value) => Show (Attribute label value) where
  show (Attr a) = symbolVal (Proxy @label) <> ": " <> show a

-- Currently doesn't respect order semantics of proper tuples
-- Use something like type-map instead
data Tuple a where
  TNil :: Tuple '[]
  (:::) :: Attribute label value -> Tuple xs -> Tuple (Attribute label value ': xs)

infixr 5 :::

instance Show (Tuple '[]) where
  show TNil = ""

instance
  (KnownSymbol label, Show value, Show (Tuple rest)) =>
  Show (Tuple (Attribute label value ': rest))
  where
  show (Attr val ::: rest) = symbolVal (Proxy @label) <> ": " <> show val <> " | " <> show rest

instance Eq (Tuple '[]) where
  TNil == TNil = True

instance (Eq value, Eq (Tuple rest)) => Eq (Tuple (Attribute label value ': rest)) where
  Attr x ::: xs == Attr y ::: ys = x == y && xs == ys

instance Ord (Tuple '[]) where
  compare TNil TNil = EQ

instance (Ord value, Ord (Tuple rest)) => Ord (Tuple (Attribute label value ': rest)) where
  compare (Attr x ::: xs) (Attr y ::: ys) = compare x y <> compare xs ys

data Relation heading key where
  Rel :: Relude.Map (Lookup key heading) (Remove key heading) -> Relation heading key

instance
  (Typeable heading, Show (Remove key heading), Show (Lookup key heading)) =>
  Show (Relation heading key)
  where
  show (Rel m) = "" & h <> shows m
    where
      h = case typeRep @heading of
        App _tuple list -> go list
        other -> shows other
      go :: TypeRep a -> ShowS
      go (Con _nil) = ("|\n" <>)
      go (App (App _ (App (App _ label) ty)) rest) =
        ("| " <>) . shows label . (" " <>) . shows ty . (" " <>) . go rest
      go other = shows other

type family TCons a tup where
  TCons attr (Tuple attrs) = Tuple (attr ': attrs)

-- Remove and Lookup rely on order of attributes in key being the same as in
-- heading.
type family Remove key heading where
  Remove (label ': ls) (Tuple (Attribute label a ': rest)) = Remove ls (Tuple rest)
  Remove (label ': ls) (Tuple (a ': rest)) = TCons a (Remove (label ': ls) (Tuple rest))
  Remove '[] tuple = tuple
  Remove labels (Tuple '[]) = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels ':<>: 'Text " in table heading")

type family Lookup key heading where
  Lookup (label ': ls) (Tuple (Attribute label a ': rest)) =
    TCons (Attribute label a) (Lookup ls (Tuple rest))
  Lookup (label ': ls) (Tuple (a ': rest)) = Lookup (label ': ls) (Tuple rest)
  Lookup '[] tuple = Tuple '[]
  Lookup labels (Tuple '[]) = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels ':<>: 'Text " in table heading")

type Heading = Tuple [Attribute "id" Int, Attribute "name" String, Attribute "age" Double]

people :: Relation Heading '["id"]
people = Rel (M.singleton (Attr 1 ::: TNil) (Attr "Joseph" ::: Attr 25 ::: TNil))

tableDee :: Relation (Tuple '[]) '[]
tableDee = Rel (M.singleton TNil TNil)

data As (a :: Symbol) (b :: Symbol) = As

type family RenameKey a b key where
  RenameKey a b '[] = '[]
  RenameKey a b (a ': rest) = b ': rest
  RenameKey a b (c ': rest) = c ': RenameKey a b rest

type family RenameHeading a b heading where
  RenameHeading a b (Tuple '[]) = Tuple '[]
  RenameHeading a b (Tuple (Attribute a t ': rest)) = Tuple (Attribute b t ': rest)
  RenameHeading a b (Tuple (c ': rest)) = TCons c (RenameHeading a b (Tuple rest))

rename :: Relation heading key -> As a b -> Relation (RenameHeading a b heading) (RenameKey a b key)
rename (Rel m) _ = Rel (unsafeCoerce m)

-- restrict :: Relation heading key -> (heading -> Bool) -> Relation heading key
-- restrict (Rel m) pred =

-- data Query db result where
--   Identity :: Query db result
--   Rename ::
