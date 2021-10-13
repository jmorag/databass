{-# LANGUAGE AllowAmbiguousTypes #-}
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
module QueryLanguage.Fancy where

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

import GHC.TypeLits
import Relude hiding (show, Set)
import Type.Reflection
import Data.Type.Set hiding (Proxy)

newtype Attribute (label :: Symbol) (a :: Type) = Attr {getAttr :: a}
  deriving (Eq, Ord)

instance (KnownSymbol label, Show value) => Show (Attribute label value) where
  show (Attr a) = symbolVal (Proxy @label) <> ": " <> show a

type instance Cmp (Attribute l1 a) (Attribute l2 b) = CmpSymbol l1 l2

(<|) :: e -> Set s -> Set (e : s)
(<|) = Ext
infixr 5 <|

type Tuple = Set

data Relation t where
  EmptyRel :: Relation t
  Insert :: t -> Relation t -> Relation t
  Rename :: As a b -> Relation t -> Relation (Rename a b t)
  Restrict :: (t -> Bool) -> Relation t -> Relation t
  -- Use forall to make type application API better
  Project :: forall attrs t. Relation t -> Relation (Lookup attrs t)
  Join :: Relation t -> Relation t' -> Relation (ComputeJoin t t')

instance (Typeable heading) => Show (Relation heading) where
  show _ = "TODO Show contents" & h
    where
      h = case typeRep @heading of
        App _tuple list -> go list
        other -> shows other
      go :: TypeRep a -> ShowS
      go (Con _nil) = ("|\n" <>)
      go (App (App _ (App (App _ label) ty)) rest) =
        ("| " <>) . shows label . (" " <>) . shows ty . (" " <>) . go rest
      go other = shows other

-- Very confused why we need the empty case as well...
type family TCons (a :: Type) tup where
  TCons attr (Tuple '[]) = Tuple '[attr]
  TCons attr (Tuple as) = Tuple (attr ': as)

-- Remove and Lookup rely on order of attributes in key being the same as in
-- heading.
type family RemoveAttrs labels t where
  RemoveAttrs (label ': ls) (Tuple (Attribute label a ': rest)) = RemoveAttrs ls (Tuple rest)
  RemoveAttrs (label ': ls) (Tuple (a ': rest)) = TCons a (RemoveAttrs (label ': ls) (Tuple rest))
  RemoveAttrs '[] tuple = tuple
  RemoveAttrs labels (Tuple '[]) = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels ':<>: 'Text " in table heading")

type family Lookup key t where
  Lookup (label ': ls) (Tuple (Attribute label a ': rest)) =
    TCons (Attribute label a) (Lookup ls (Tuple rest))
  Lookup (label ': ls) (Tuple (a ': rest)) = Lookup (label ': ls) (Tuple rest)
  Lookup '[] tuple = Tuple '[]
  Lookup labels (Tuple '[]) = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels ':<>: 'Text " in table heading")

data As (a :: Symbol) (b :: Symbol) = As

type family Rename a b t where
  Rename a b (Tuple '[]) = Tuple '[]
  Rename a b (Tuple (Attribute a t ': rest)) = Tuple (Attribute b t ': rest)
  Rename a b (Tuple (c ': rest)) = TCons c (Rename a b (Tuple rest))

type family ComputeJoin t t' where
  ComputeJoin (Tuple xs) (Tuple ys) = Tuple (Union xs ys)

type Person = Tuple '[Attribute "id" Int, Attribute "name" String, Attribute "age" Double]

people :: Relation Person
people = Insert (Attr 1 <| Attr "Joseph" <| Attr 25 <| Empty) EmptyRel

true :: Relation (Tuple '[])
true = Insert Empty EmptyRel

false :: Relation (Tuple '[])
false = EmptyRel

renamedPeople = Rename (As @"id" @"ID") people

projectedPeople = Project @'["id", "name"] people
