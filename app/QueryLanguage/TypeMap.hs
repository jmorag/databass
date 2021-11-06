{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
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
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-signatures #-}

-- |
module QueryLanguage.TypeMap where

import Control.Foldl (Fold)
import Control.Foldl qualified as L
import Data.Binary
import Data.Binary.Get (getInt64le, isolate)
import Data.Binary.Put
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as M
import Data.Type.Map
import Data.Type.Set (AsSet, Sort, type (:++))
import GHC.TypeLits
import Relude hiding (Identity, Map, get, put, undefined)
import Type.Reflection

(|>) :: forall (k :: Symbol) v m. v -> Map m -> Map ((k ':-> v) : m)
(|>) = Ext (Var @k)

infixr 5 |>

type Tuple = Map

-- | Kind constrained version of :-> that binds more tightly
type (k :: Symbol) ::: (v :: Type) = k ':-> v

infixr 6 :::

instance Binary (Tuple '[]) where
  put Empty = pure ()
  get = pure Empty

instance (Binary x, Binary (Tuple ts)) => Binary (Tuple (l ::: x ': ts)) where
  put (Ext _ x xs) = do
    let bytes = runPut $ put x
    putInt64le (BL.length bytes)
    put x
    put xs
  get = do
    size <- getInt64le
    x <- isolate (fromIntegral size) get
    Ext Var x <$> get

type family GetAttr attr where
  GetAttr (l ::: a) = a
  GetAttr x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

data Table heading key k v where
  MkTable ::
    forall (key :: [Symbol]) (heading :: [Mapping Symbol Type]) k v.
    (IsKey key heading, Binary (Tuple heading), k ~ (heading :!! key), v ~ (heading :\\ key)) =>
    Table heading key k v

-- | Type level multi-key lookup from a map
type family (m :: [Mapping Symbol Type]) :!! (cs :: [Symbol]) :: [Mapping Symbol Type] where
  (label ::: a ': rest) :!! (label ': ls) = label ::: a ': rest :!! ls
  (a ': rest) :!! (label ': ls) = rest :!! (label ': ls)
  tuple :!! '[] = '[]
  '[] :!! labels = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels)

type family IsKey (labels :: [Symbol]) (t :: [Mapping Symbol Type]) :: Constraint where
  IsKey (label ': ls) (label ::: a ': rest) = IsKey ls rest
  IsKey (label ': ls) (a ': rest) = IsKey (label ': ls) rest
  IsKey '[] tuple = ()
  IsKey labels '[] = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels)

type family Rename (a :: Symbol) (b :: Symbol) (t :: [Mapping Symbol Type]) :: [Mapping Symbol Type] where
  Rename a b '[] = '[]
  Rename a b ((a ::: t) ': rest) = (b ::: t) ': rest
  Rename a b (c ': rest) = c ': Rename a b rest

-- | Delete multiple elements from a map by key
type family (m :: [Mapping Symbol Type]) :\\ (cs :: [Symbol]) :: [Mapping Symbol Type] where
  (label ::: a ': rest) :\\ (label ': ls) = rest :\\ ls
  (a ': rest) :\\ (label ': ls) = a ': rest :\\ (label ': ls)
  tuple :\\ '[] = tuple
  '[] :\\ labels = TypeError ( 'Text "Could not find " ':<>: 'ShowType labels)

type family (m :: [Mapping Symbol Type]) :! (c :: Symbol) :: Type where
  (label ::: a ': rest) :! label = a
  (attr ': rest) :! label = rest :! label
  '[] :! label = TypeError ( 'Text "Could not find " ':<>: 'ShowType label)

type family UnNest t where
  UnNest (l ::: Query ts) = ts
  UnNest (l ::: t) = TypeError ( 'ShowType (l ::: t) ':<>: 'Text " is not relation valued")
  UnNest x = TypeError ( 'ShowType x ':$$: 'Text " is not an attribute")

data Query (t :: [Mapping Symbol Type]) where
  Identity :: Table t key k v -> Query t
  Rename :: forall a b t. Query t -> Query (AsMap (Rename a b t))
  Restrict :: (Tuple t -> Bool) -> Query t -> Query t
  Project :: forall attrs t. Query t -> Query (AsMap t :!! AsSet attrs)
  Join :: Query t' -> Query t -> Query (Union t' t)
  Union :: Query t -> Query t -> Query t
  Intersection :: Query t -> Query t -> Query t
  Difference :: Query t -> Query t -> Query t
  Extend ::
    forall (l :: Symbol) (a :: Type) (t :: [Mapping Symbol Type]).
    (Member l t ~ 'False) =>
    (Tuple t -> a) ->
    Query t ->
    Query (Sort (l ::: a ': t))
  Summarize ::
    forall l a t t'.
    (Submap t' t, Member l t' ~ 'False) =>
    Query t' ->
    Fold (Tuple t') a ->
    Query t ->
    Query (Sort (l ::: a ': t'))
  Group ::
    forall (l :: Symbol) (attrs :: [Symbol]) (t :: [Mapping Symbol Type]).
    Query t ->
    Query (Sort (l ::: Query (AsMap t :!! AsSet attrs) ': AsSet t :\\ AsSet attrs))
  Ungroup ::
    forall l t.
    Query t ->
    Query (Sort (UnNest (l ::: (t :! l)) :++ t :\ l))

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

data Color = Red | Green | Blue deriving (Show, Eq, Generic)

instance Binary Color

type PHeading = '["P#" ::: Int, "PNAME" ::: String, "COLOR" ::: Color, "WEIGHT" ::: Double, "CITY" ::: String]

p :: Query PHeading
p = Identity (MkTable @'["P#"])

type SPHeading = '["S#" ::: Int, "P#" ::: Int, "QTY" ::: Int]

sp :: Query SPHeading
sp = Identity (MkTable @'["S#", "P#"])

extendEx = s & Extend @"TRIPLE" (\t -> lookp' @"STATUS" t * 3)

summarizeEx = sp & Summarize @"P_COUNT" (Project @'["S#"] s) L.length

groupEx = sp & Group @"PQ" @'["P#", "QTY"]

ungroupEx = groupEx & Ungroup @"PQ"

renameEx = s & Rename @"S#" @"id"

lookp' ::
  forall (label :: Symbol) (m :: [Mapping Symbol Type]) (t :: Type).
  (IsMember label t m, t ~ (m :! label)) =>
  Tuple m ->
  t
lookp' = lookp (Var @label)

-- | Non type-changing update
update' :: forall v t m. (Updatable v t m m, t ~ (m :! v)) => t -> Tuple m -> Tuple m
update' x m = update m (Var @v) x

data Database (tables :: [Mapping Symbol Type]) where
  EmptyDB :: Database '[]
  CreateTable ::
    forall name heading key tables k v.
    (KnownSymbol name) =>
    Var name ->
    Table heading key k v ->
    Database tables ->
    Database ((name ::: Table heading key k v) ': tables)
  DeleteTable ::
    forall (name :: Symbol) tables remaining.
    (KnownSymbol name) =>
    Var name ->
    Database tables ->
    Database remaining
  Insert ::
    forall (name :: Symbol) (tables :: [Mapping Symbol Type]) heading key k v.
    (KnownSymbol name, Table heading key k v ~ (tables :! name), Binary (Tuple k), Binary (Tuple v), Split k v heading) =>
    Var name ->
    Proxy (Table heading key k v) ->
    Tuple heading ->
    Database tables ->
    Database tables

insert ::
  forall name tables heading key k v.
  (KnownSymbol name, (tables :! name) ~ Table heading key k v, Binary (Tuple k), Binary (Tuple v), Split k v heading) =>
  Tuple (TableHeading (tables :! name)) ->
  Database tables ->
  Database tables
insert = Insert (Var @name) Proxy

type family TableHeading table :: [Mapping Symbol Type] where
  TableHeading (Table heading key k v) = heading
  TableHeading a = TypeError ( 'Text "Can only call 'TableHeading' on Table, not " ':$$: 'ShowType a)

type family TableKey table :: [Symbol] where
  TableKey (Table heading key k v) = key
  TableKey a = TypeError ( 'Text "Can only call 'TableKey' on Table, not " ':$$: 'ShowType a)

db =
  EmptyDB
    & CreateTable (Var @"Supplier") (MkTable @'["S#"] @SHeading)
    & CreateTable (Var @"Part") (MkTable @'["P#"] @PHeading)
    & CreateTable (Var @"SP") (MkTable @'["S#", "P#"] @SPHeading)
    & insert @"Supplier" (1 |> "Smith" |> 20 |> "London" |> Empty)
    & insert @"Supplier" (2 |> "Jones" |> 10 |> "Paris" |> Empty)
    & insert @"Supplier" (3 |> "Blake" |> 30 |> "Paris" |> Empty)
    & insert @"Supplier" (4 |> "Clark" |> 20 |> "London" |> Empty)
    & insert @"Supplier" (5 |> "Adams" |> 30 |> "Athens" |> Empty)
    & insert @"Part" (1 |> "Nut" |> Red |> 12 |> "London" |> Empty)
    & insert @"Part" (2 |> "Bolt" |> Green |> 17 |> "Paris" |> Empty)
    & insert @"Part" (3 |> "Screw" |> Blue |> 17 |> "Oslo" |> Empty)
    & insert @"Part" (4 |> "Screw" |> Red |> 14 |> "London" |> Empty)
    & insert @"Part" (5 |> "Cam" |> Blue |> 12 |> "Paris" |> Empty)
    & insert @"Part" (6 |> "Cog" |> Red |> 19 |> "London" |> Empty)
    & insert @"SP" (1 |> 1 |> 300 |> Empty)
    & insert @"SP" (1 |> 2 |> 200 |> Empty)
    & insert @"SP" (1 |> 3 |> 400 |> Empty)
    & insert @"SP" (1 |> 4 |> 200 |> Empty)
    & insert @"SP" (1 |> 5 |> 100 |> Empty)
    & insert @"SP" (1 |> 6 |> 100 |> Empty)
    & insert @"SP" (2 |> 1 |> 300 |> Empty)
    & insert @"SP" (2 |> 2 |> 400 |> Empty)
    & insert @"SP" (3 |> 2 |> 200 |> Empty)
    & insert @"SP" (4 |> 2 |> 200 |> Empty)
    & insert @"SP" (4 |> 4 |> 300 |> Empty)
    & insert @"SP" (4 |> 5 |> 400 |> Empty)
    & DeleteTable (Var @"Supplier")

-- runQuery :: Query t -> Database tables -> [Tuple t]
-- runQuery q db = []

materializeDB :: forall tables. Database tables -> M.Map String (M.Map LByteString LByteString)
materializeDB EmptyDB = mempty
materializeDB (CreateTable (Var :: Var name) MkTable rest) =
  M.insert (str @name) mempty (materializeDB rest)
materializeDB (DeleteTable (Var :: Var name) rest) = M.delete (str @name) (materializeDB rest)
materializeDB (Insert (Var :: Var name) (Proxy :: Proxy (Table heading key k v)) t rest) =
  M.update (Just . M.insert (encode key) (encode val)) (str @name) (materializeDB rest)
  where
    kv :: (Split k v heading) => (Tuple k, Tuple v)
    kv = split t
    (key, val) = kv

str :: forall k. KnownSymbol k => String
str = symbolVal (Proxy @k)
