{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}

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

type Tuple = Set

instance Eq (Tuple '[]) where
  _ == _ = True

instance Ord (Tuple '[]) where
  compare _ _ = EQ

instance (Ord e, Ord (Tuple s)) => Ord (Tuple (e ': s)) where
  compare (Ext x xs) (Ext y ys) = compare x y <> compare xs ys

-- TODO make relevant operations multi-arity??
data Query (t :: [Type]) where
  Identity :: Table t key -> Query t
  Rename :: forall a b t. Query t -> Query (AsSet (Rename a b t))
  Restrict :: (Tuple t -> Bool) -> Query t -> Query t
  -- Use forall to make type application API better
  Project :: forall attrs t. Query t -> Query (LookupKey (AsSet attrs) (AsSet t))
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
    Query (Sort (l ::: Query (LookupKey (AsSet attrs) (AsSet t)) ': RemoveAttrs (AsSet attrs) (AsSet t)))
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
  RemoveAttrs labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels)

type family RemoveAttr (label :: Symbol) (t :: [Type]) :: [Type] where
  RemoveAttr label (label ::: a ': rest) = rest
  RemoveAttr label (x ': rest) = x ': RemoveAttr label rest
  RemoveAttr label '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType label)

class RemoveLabel (label :: Symbol) (t :: [Type]) where
  removeLabel :: Tuple t -> Tuple (RemoveAttr label t)

instance {-# OVERLAPPABLE #-} RemoveLabel label (label ::: a ': rest) where
  removeLabel (Ext _ rest) = rest

instance {-# OVERLAPS #-} RemoveLabel label rest => RemoveLabel label (label' ::: a ': rest) where
  removeLabel :: Tuple (label' ::: a ': rest) -> Tuple (RemoveAttr label (label' ::: a ': rest))
  removeLabel (Ext x rest) = unsafeCoerce $ Ext x (removeLabel @label rest)

type family LookupKey (labels :: [Symbol]) (t :: [Type]) :: [Type] where
  LookupKey (label ': ls) (label ::: a ': rest) = label ::: a ': LookupKey ls rest
  LookupKey (label ': ls) (a ': rest) = LookupKey (label ': ls) rest
  LookupKey '[] tuple = '[]
  LookupKey labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels)

type family IsKey (labels :: [Symbol]) (t :: [Type]) :: Constraint where
  IsKey (label ': ls) (label ::: a ': rest) = IsKey ls rest
  IsKey (label ': ls) (a ': rest) = IsKey (label ': ls) rest
  IsKey '[] tuple = ()
  IsKey labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels)

type family Get (label :: Symbol) (t :: [Type]) :: Type where
  Get label (label ::: a ': rest) = label ::: a
  Get label (attr ': rest) = Get label rest
  Get label '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType label)

class Index label attrs where
  index :: Tuple attrs -> GetAttr (Get label attrs)

instance {-# OVERLAPPABLE #-} Index label (label ::: a ': rest) where
  index (Ext attr _) = getAttr attr

instance {-# OVERLAPS #-} Index label attrs => Index label (attr ': attrs) where
  -- Worrying, but appears to work fine?
  index (Ext _ rest) = unsafeCoerce $ index @label rest

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
s = Identity (MkTable @'["S#"])

data Color = Red | Green | Blue deriving (Show, Eq)

type PHeading = '["P#" ::: Int, "PNAME" ::: String, "COLOR" ::: Color, "WEIGHT" ::: Double, "CITY" ::: String]

p :: Query PHeading
p = Identity (MkTable @'["P#"])

type SPHeading = '["S#" ::: Int, "P#" ::: Int, "QTY" ::: Int]

sp :: Query SPHeading
sp = Identity (MkTable @'["S#", "P#"])

extendEx = s & Extend @"TRIPLE" (\t -> index @"STATUS" t * 3)

summarizeEx = sp & Summarize @"P_COUNT" (Project @'["S#"] s) L.length

groupEx = sp & Group @"PQ" @'["P#", "QTY"]

ungroupEx = groupEx & Ungroup @"PQ"

renameEx = s & Rename @"S#" @"id"

data Table heading key where
  MkTable ::
    forall (key :: [Symbol]) (heading :: [Type]).
    (IsKey (AsSet key) (AsSet heading)) =>
    Table heading key

type family TableHeading table where
  TableHeading (Table heading key) = heading
  TableHeading a = TypeError ( 'Text "Can only call 'TableHeading' on Table, not " ':$$: 'ShowType a)

data Database tables where
  EmptyDB :: Database '[]
  CreateTable ::
    forall name heading key tables.
    Table heading key ->
    Database tables ->
    Database ((name ::: Table heading key) ': tables)
  DeleteTable ::
    forall (name :: Symbol) tables.
    Database tables ->
    Database (RemoveAttr name tables)
  Insert ::
    forall (name :: Symbol) (tables :: [Type]).
    Tuple (TableHeading (GetAttr (Get name tables))) ->
    Database tables ->
    Database tables

db =
  EmptyDB
    & CreateTable @"Supplier" (MkTable @'["S#"] @SHeading)
    & CreateTable @"Part" (MkTable @'["P#"] @PHeading)
    & CreateTable @"SP" (MkTable @'["S#", "P#"] @SPHeading)
    & Insert @"Supplier" (Ext (A 1) $ Ext (A "Smith") $ Ext (A 20) $ Ext (A "London") Empty)
    & Insert @"Supplier" (Ext (A 2) $ Ext (A "Jones") $ Ext (A 10) $ Ext (A "Paris") Empty)
    & Insert @"Supplier" (Ext (A 3) $ Ext (A "Blake") $ Ext (A 30) $ Ext (A "Paris") Empty)
    & Insert @"Supplier" (Ext (A 4) $ Ext (A "Clark") $ Ext (A 20) $ Ext (A "London") Empty)
    & Insert @"Supplier" (Ext (A 5) $ Ext (A "Adams") $ Ext (A 30) $ Ext (A "Athens") Empty)
    & Insert @"Part" (Ext (A 1) $ Ext (A "Nut") $ Ext (A Red) $ Ext (A 12) $ Ext (A "London") Empty)
    & Insert @"Part" (Ext (A 2) $ Ext (A "Bolt") $ Ext (A Green) $ Ext (A 17) $ Ext (A "Paris") Empty)
    & Insert @"Part" (Ext (A 3) $ Ext (A "Screw") $ Ext (A Blue) $ Ext (A 17) $ Ext (A "Oslo") Empty)
    & Insert @"Part" (Ext (A 4) $ Ext (A "Screw") $ Ext (A Red) $ Ext (A 14) $ Ext (A "London") Empty)
    & Insert @"Part" (Ext (A 5) $ Ext (A "Cam") $ Ext (A Blue) $ Ext (A 12) $ Ext (A "Paris") Empty)
    & Insert @"Part" (Ext (A 6) $ Ext (A "Cog") $ Ext (A Red) $ Ext (A 19) $ Ext (A "London") Empty)
    & Insert @"SP" (Ext (A 1) $ Ext (A 1) $ Ext (A 300) Empty)
    & Insert @"SP" (Ext (A 1) $ Ext (A 2) $ Ext (A 200) Empty)
    & Insert @"SP" (Ext (A 1) $ Ext (A 3) $ Ext (A 400) Empty)
    & Insert @"SP" (Ext (A 1) $ Ext (A 4) $ Ext (A 200) Empty)
    & Insert @"SP" (Ext (A 1) $ Ext (A 5) $ Ext (A 100) Empty)
    & Insert @"SP" (Ext (A 1) $ Ext (A 6) $ Ext (A 100) Empty)
    & Insert @"SP" (Ext (A 2) $ Ext (A 1) $ Ext (A 300) Empty)
    & Insert @"SP" (Ext (A 2) $ Ext (A 2) $ Ext (A 400) Empty)
    & Insert @"SP" (Ext (A 3) $ Ext (A 2) $ Ext (A 200) Empty)
    & Insert @"SP" (Ext (A 4) $ Ext (A 2) $ Ext (A 200) Empty)
    & Insert @"SP" (Ext (A 4) $ Ext (A 4) $ Ext (A 300) Empty)
    & Insert @"SP" (Ext (A 4) $ Ext (A 5) $ Ext (A 400) Empty)

runQuery :: Query t -> Database tables -> [Tuple t]
runQuery q db = []

type family TableMap table where
  TableMap (name ::: Table heading key) = name ::: Map (Tuple key) (Tuple heading)

type family Maps tables where
  Maps '[] = '[]
  Maps (t ': ts) = TableMap t ': Maps ts

type family OrdKeys (tables :: [Type]) :: Constraint where
  OrdKeys '[] = ()
  OrdKeys (name ::: Table heading key ': ts) = (Ord (Tuple key), OrdKeys ts)

materializeDB :: (OrdKeys tables) => Database tables -> Tuple (Maps tables)
materializeDB EmptyDB = Empty
materializeDB (CreateTable MkTable rest) = Ext (A mempty) (materializeDB rest)
-- TODO: Use "visible type application in patterns" once GHC 9.2 is viable
-- materializeDB (DeleteTable (rest :: Database (RemoveAttr name tables))) = removeLabel @name (materializeDB rest)
