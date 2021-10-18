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
import Relude hiding (Identity, Set, show)
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)

-- Tagged Attribtue
newtype (label :: Symbol) ::: (a :: Type) = A {getAttr :: a}
  deriving (Eq, Ord, Functor)

type family GetAttr attr where
  GetAttr (l ::: a) = a
  GetAttr x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

type family GetLabel attr where
  GetLabel (l ::: a) = l
  GetLabel x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

instance (KnownSymbol label, Show value) => Show (label ::: value) where
  show attr = symbolVal (Proxy @label) <> ": " <> show (getAttr attr)

type instance Cmp (l1 ::: a) (l2 ::: b) = CmpSymbol l1 l2

(<|) :: e -> Set s -> Set (e : s)
(<|) = Ext

infixr 5 <|

type Tuple = Set

class Index label attrs where
  index :: Tuple attrs -> GetAttr (Get label attrs)

instance {-# OVERLAPPABLE #-} Index label (label ::: a ': rest) where
  index (Ext attr _) = getAttr attr

instance {-# OVERLAPS #-} Index label attrs => Index label (attr ': attrs) where
  -- Worrying, but appears to work fine?
  index (Ext _ rest) = unsafeCoerce $ index @label rest

-- TODO make relevant operations multi-arity??
data Query (t :: [Type]) where
  Identity :: Query t
  Rename :: forall a b t. Query t -> Query (AsSet (Rename a b t))
  Restrict :: (Tuple t -> Bool) -> Query t -> Query t
  -- Use forall to make type application API better
  Project :: forall attrs t. Query t -> Query (Lookup (AsSet attrs) (AsSet t))
  Join :: Query t' -> Query t -> Query (Union t' t)
  Union :: Query t -> Query t -> Query t
  Intersection :: Query t -> Query t -> Query t
  Difference :: Query t -> Query t -> Query t
  Extend ::
    forall l a t.
    (NonMember (l ::: a) t) =>
    (Tuple t -> a) ->
    Query t ->
    Query (Sort (l ::: a ': t))
  Summarize ::
    forall l a t t'.
    (Subset t' t, NonMember (l ::: a) t') =>
    Query t' ->
    Fold (Tuple t') a ->
    Query t ->
    Query (Sort (l ::: a ': t'))
  Group ::
    forall l attrs t.
    Query t ->
    Query (Sort (l ::: Query (Lookup (AsSet attrs) (AsSet t)) ': RemoveAttrs (AsSet attrs) (AsSet t)))
  Ungroup ::
    forall l t.
    Query t ->
    Query (Sort (UnNest (Get l t) :++ RemoveAttrs '[l] t))

instance (Typeable heading) => Show (Query heading) where
  show _ = "\nTODO Show contents" & go (typeRep @heading)
    where
      go :: TypeRep a -> ShowS
      go (Con nil) | tyConName nil == "'[]" = ("|" <>)
      go (App (App _ (App (App _ label) ty)) rest) =
        ("| " <>) . shows label . (" " <>) . go ty . (" " <>) . go rest
      go (App (Con relation) nested)
        | tyConName relation == "Query" =
          ("(" <>) . go nested . (")" <>)
      go other = shows other

type family RemoveAttrs (labels :: [Symbol]) (t :: [Type]) :: [Type] where
  RemoveAttrs (label ': ls) (label ::: a ': rest) = RemoveAttrs ls rest
  RemoveAttrs (label ': ls) (a ': rest) = a ': RemoveAttrs (label ': ls) rest
  RemoveAttrs '[] tuple = tuple
  RemoveAttrs labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels ':<>: 'Text " in table heading")

type family Lookup (labels :: [Symbol]) (t :: [Type]) :: [Type] where
  Lookup (label ': ls) (label ::: a ': rest) = label ::: a ': Lookup ls rest
  Lookup (label ': ls) (a ': rest) = Lookup (label ': ls) rest
  Lookup '[] tuple = '[]
  Lookup labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels ':<>: 'Text " in table heading")

type family Get (label :: Symbol) (t :: [Type]) :: Type where
  Get label (label ::: a ': rest) = label ::: a
  Get label (attr ': rest) = Get label rest
  Get label '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType label ':<>: 'Text " in table heading")

type family UnNest t where
  UnNest (l ::: Query ts) = ts
  UnNest (l ::: t) = TypeError ( 'ShowType (l ::: t) ':<>: 'Text " is not relation valued")
  UnNest x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

type family Rename (a :: Symbol) (b :: Symbol) (t :: [Type]) :: [Type] where
  Rename a b '[] = '[]
  Rename a b (a ::: t ': rest) = b ::: t ': rest
  Rename a b (c ': rest) = c ': Rename a b rest

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

type SHeading = '["S#" ::: Int, "SNAME" ::: String, "STATUS" ::: Int, "CITY" ::: String]

s :: Query SHeading
s = Identity

data Color = Red | Green | Blue deriving (Show, Eq)

type PHeading = '["P#" ::: Int, "PNAME" ::: String, "COLOR" ::: Color, "WEIGHT" ::: Double, "CITY" ::: String]

p :: Query PHeading
p = Identity

type SPHeading = '["S#" ::: Int, "P#" ::: Int, "QTY" ::: Int]

sp :: Query SPHeading
sp = Identity

extendEx = s & Extend @"TRIPLE" (\t -> index @"STATUS" t * 3)

summarizeEx = sp & Summarize @"P_COUNT" (Project @'["S#"] s) L.length

groupEx = sp & Group @"PQ" @'["P#", "QTY"]

ungroupEx = groupEx & Ungroup @"PQ"

renameEx = s & Rename @"S#" @"id"
