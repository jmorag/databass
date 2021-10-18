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

import Control.Foldl (Fold)
-- for Cmp Symbol instance
import Data.Type.Map ()
import Data.Type.Set hiding (Proxy)
import GHC.TypeLits
import Relude hiding (Set, show)
import Type.Reflection

newtype Attribute (label :: Symbol) (a :: Type) = Attr {getAttr :: a}
  deriving (Eq, Ord)

instance (KnownSymbol label, Show value) => Show (Attribute label value) where
  show (Attr a) = symbolVal (Proxy @label) <> ": " <> show a

type instance Cmp (Attribute l1 a) (Attribute l2 b) = CmpSymbol l1 l2

(<|) :: e -> Set s -> Set (e : s)
(<|) = Ext

infixr 5 <|

type Tuple = Set

-- TODO make relevant operations multi-arity??
data Relation (t :: [Type]) where
  EmptyRel :: Relation t
  Insert :: Tuple t -> Relation t -> Relation t
  Rename :: forall a b t. Relation t -> Relation (AsSet (Rename a b t))
  Restrict :: (Tuple t -> Bool) -> Relation t -> Relation t
  -- Use forall to make type application API better
  Project :: forall attrs t. Relation t -> Relation (Lookup (AsSet attrs) (AsSet t))
  Join :: Relation t -> Relation t' -> Relation (Union t t')
  Union :: Relation t -> Relation t -> Relation t
  Intersection :: Relation t -> Relation t -> Relation t
  Difference :: Relation t -> Relation t -> Relation t
  Extend ::
    forall l a t.
    (NonMember (Attribute l a) t) =>
    (Tuple t -> a) ->
    Relation t ->
    Relation (Sort (Attribute l a ': t))
  Summarize ::
    forall l a t t'.
    (Subset t' t, NonMember (Attribute l a) t') =>
    Relation t' ->
    Fold (Tuple t') a ->
    Relation t ->
    Relation (Sort (Attribute l a ': t'))
  Group ::
    forall l attrs t.
    Relation t ->
    Relation (Sort (Attribute l (Relation (Lookup (AsSet attrs) (AsSet t))) ': RemoveAttrs (AsSet attrs) (AsSet t)))
  Ungroup ::
    forall l t.
    Relation t ->
    Relation (Sort (UnNest (Get l t) :++ RemoveAttrs '[l] t))

instance (Typeable heading) => Show (Relation heading) where
  show _ = "TODO Show contents" & go (typeRep @heading)
    where
      go :: TypeRep a -> ShowS
      go (Con _nil) = ("|\n" <>)
      go (App (App _ (App (App _ label) ty)) rest) =
        ("| " <>) . shows label . (" " <>) . shows ty . (" " <>) . go rest
      go other = shows other

type family RemoveAttrs (labels :: [Symbol]) (t :: [Type]) :: [Type] where
  RemoveAttrs (label ': ls) (Attribute label a ': rest) = RemoveAttrs ls rest
  RemoveAttrs (label ': ls) (a ': rest) = a ': RemoveAttrs (label ': ls) rest
  RemoveAttrs '[] tuple = tuple
  RemoveAttrs labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels ':<>: 'Text " in table heading")

type family Lookup (labels :: [Symbol]) (t :: [Type]) :: [Type] where
  Lookup (label ': ls) (Attribute label a ': rest) = Attribute label a ': Lookup ls rest
  Lookup (label ': ls) (a ': rest) = Lookup (label ': ls) rest
  Lookup '[] tuple = '[]
  Lookup labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels ':<>: 'Text " in table heading")

type family Get (label :: Symbol) (t :: [Type]) :: Type where
  Get label (Attribute label a ': rest) = Attribute label a
  Get label (attr ': rest) = Get label rest
  Get label '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType label ':<>: 'Text " in table heading")

type family UnNest t where
  UnNest (Attribute l (Relation ts)) = ts
  UnNest (Attribute l t) = TypeError ( 'ShowType (Attribute l t) ':<>: 'Text " is not relation valued")
  UnNest x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

type family Rename (a :: Symbol) (b :: Symbol) (t :: [Type]) :: [Type] where
  Rename a b '[] = '[]
  Rename a b (Attribute a t ': rest) = Attribute b t ': rest
  Rename a b (c ': rest) = c ': Rename a b rest

type Person = '[Attribute "id" Int, Attribute "name" String, Attribute "age" Double]

people :: Relation Person
people = Insert (Attr 1 <| Attr "Joseph" <| Attr 25 <| Empty) EmptyRel

true :: Relation '[]
true = Insert Empty EmptyRel

false :: Relation '[]
false = EmptyRel

renamedPeople = Rename (As @"id" @"ID") people

projectedPeople = Project @'["name", "id"] people
