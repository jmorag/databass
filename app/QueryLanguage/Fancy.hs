{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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

import Control.Foldl (Fold)
import Control.Foldl qualified as L
-- for Cmp Symbol instance
import Data.Type.Map ()
import Data.Type.Set hiding (Proxy)
import GHC.TypeLits
import Relude hiding (Set, show)
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)

-- TODO: Consider shorter name / symbol for these, they get typed a lot
newtype Attribute (label :: Symbol) (a :: Type) = Attr {getAttr :: a}
  deriving (Eq, Ord, Functor)

type family GetAttr attr where
  GetAttr (Attribute l a) = a
  GetAttr x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

type family GetLabel attr where
  GetLabel (Attribute l a) = l
  GetLabel x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

instance (KnownSymbol label, Show value) => Show (Attribute label value) where
  show (Attr a) = symbolVal (Proxy @label) <> ": " <> show a

type instance Cmp (Attribute l1 a) (Attribute l2 b) = CmpSymbol l1 l2

(<|) :: e -> Set s -> Set (e : s)
(<|) = Ext

infixr 5 <|

type Tuple = Set

class Index label attrs where
  index :: Tuple attrs -> GetAttr (Get label attrs)

instance {-# OVERLAPPABLE #-} Index label (Attribute label a ': rest) where
  index (Ext attr _) = getAttr attr

instance {-# OVERLAPS #-} Index label attrs => Index label (attr ': attrs) where
  -- Worrying, but appears to work fine?
  index (Ext _ rest) = unsafeCoerce $ index @label rest

-- TODO make relevant operations multi-arity??
-- Also, should this be named "Query"?
data Relation (t :: [Type]) where
  -- It is unclear that these first three should be in this type, as they aren't
  -- "pure" relation operators
  EmptyRel :: Relation t
  Insert :: Tuple t -> Relation t -> Relation t
  Update :: (Tuple t -> Tuple t) -> Relation t -> Relation t
  Rename :: forall a b t. Relation t -> Relation (AsSet (Rename a b t))
  Restrict :: (Tuple t -> Bool) -> Relation t -> Relation t
  -- Use forall to make type application API better
  Project :: forall attrs t. Relation t -> Relation (Lookup (AsSet attrs) (AsSet t))
  Join :: Relation t' -> Relation t -> Relation (Union t' t)
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
  show _ = "\nTODO Show contents" & go (typeRep @heading)
    where
      go :: TypeRep a -> ShowS
      go (Con nil) | tyConName nil == "'[]" = ("|" <>)
      go (App (App _ (App (App _ label) ty)) rest) =
        ("| " <>) . shows label . (" " <>) . go ty . (" " <>) . go rest
      go (App (Con relation) nested)
        | tyConName relation == "Relation" =
          ("(" <>) . go nested . (")" <>)
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

type SHeading = '[Attribute "S#" Int, Attribute "SNAME" String, Attribute "STATUS" Int, Attribute "CITY" String]

s :: Relation SHeading
s =
  EmptyRel
    & Insert (Attr 1 <| Attr "Smith" <| Attr 20 <| Attr "London" <| Empty)
    & Insert (Attr 2 <| Attr "Jones" <| Attr 10 <| Attr "Paris" <| Empty)
    & Insert (Attr 3 <| Attr "Blake" <| Attr 30 <| Attr "Paris" <| Empty)
    & Insert (Attr 4 <| Attr "Clark" <| Attr 20 <| Attr "London" <| Empty)
    & Insert (Attr 5 <| Attr "Adams" <| Attr 30 <| Attr "Athens" <| Empty)

data Color = Red | Green | Blue deriving (Show, Eq)

type PHeading = '[Attribute "P#" Int, Attribute "PNAME" String, Attribute "COLOR" Color, Attribute "WEIGHT" Double, Attribute "CITY" String]

p :: Relation PHeading
p =
  EmptyRel
    & Insert (Attr 1 <| Attr "Nut" <| Attr Red <| Attr 12 <| Attr "London" <| Empty)
    & Insert (Attr 2 <| Attr "Bolt" <| Attr Green <| Attr 17 <| Attr "Paris" <| Empty)
    & Insert (Attr 3 <| Attr "Screw" <| Attr Blue <| Attr 17 <| Attr "Oslo" <| Empty)
    & Insert (Attr 4 <| Attr "Screw" <| Attr Red <| Attr 14 <| Attr "London" <| Empty)
    & Insert (Attr 5 <| Attr "Cam" <| Attr Blue <| Attr 12 <| Attr "Paris" <| Empty)
    & Insert (Attr 6 <| Attr "Cog" <| Attr Red <| Attr 19 <| Attr "London" <| Empty)

type SPHeading = '[Attribute "S#" Int, Attribute "P#" Int, Attribute "QTY" Int]

sp :: Relation SPHeading
sp =
  EmptyRel
    & Insert (Attr 1 <| Attr 1 <| Attr 300 <| Empty)
    & Insert (Attr 1 <| Attr 2 <| Attr 200 <| Empty)
    & Insert (Attr 1 <| Attr 3 <| Attr 400 <| Empty)
    & Insert (Attr 1 <| Attr 4 <| Attr 200 <| Empty)
    & Insert (Attr 1 <| Attr 5 <| Attr 100 <| Empty)
    & Insert (Attr 1 <| Attr 6 <| Attr 100 <| Empty)
    & Insert (Attr 2 <| Attr 1 <| Attr 300 <| Empty)
    & Insert (Attr 2 <| Attr 2 <| Attr 400 <| Empty)
    & Insert (Attr 3 <| Attr 2 <| Attr 200 <| Empty)
    & Insert (Attr 4 <| Attr 2 <| Attr 200 <| Empty)
    & Insert (Attr 4 <| Attr 4 <| Attr 300 <| Empty)
    & Insert (Attr 4 <| Attr 5 <| Attr 400 <| Empty)

extendEx = s & Extend @"TRIPLE" (\t -> index @"STATUS" t * 3)

summarizeEx = sp & Summarize @"P_COUNT" (Project @'["S#"] s) L.length

groupEx = sp & Group @"PQ" @'["P#", "QTY"]

ungroupEx = groupEx & Ungroup @"PQ"

renameEx = s & Rename @"S#" @"id"
