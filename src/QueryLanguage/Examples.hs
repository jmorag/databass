{-# LANGUAGE OverloadedLabels #-}

-- |
module QueryLanguage.Examples where

import qualified Control.Foldl as L
import Data.Binary
import Data.Type.Map
import GHC.TypeLits
import QueryLanguage.TypeMap
import Relude hiding (Identity)
import Data.Type.Set (Sort)

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

type Suppliers = "suppliers" ::: T SHeading '["S#"]

data Color = Red | Green | Blue deriving (Show, Eq, Generic)

instance Binary Color

type PHeading = '["P#" ::: Int, "PNAME" ::: String, "COLOR" ::: Color, "WEIGHT" ::: Double, "CITY" ::: String]

type Parts = "parts" ::: T PHeading '["P#"]

type SPHeading = '["S#" ::: Int, "P#" ::: Int, "QTY" ::: Int]

type SP = "suppliers-parts" ::: T SPHeading '["S#", "P#"]

type Tables = AsMap '[Suppliers, Parts, SP]

s :: Query _ Tables
s = Identity (Var @"suppliers") MkTable

p :: Query _ Tables
p = Identity (Var @"parts") MkTable

sp :: Query _ Tables
sp = Identity (Var @"suppliers-parts") MkTable

extendEx = s & Extend (Var @"TRIPLE") (\t -> lookp' (Var @"STATUS") t * 3)

summarizeEx = sp & Summarize (Var @"P_COUNT") (Project @'["S#" ::: Int] s) L.length

projectEx = s & Project @'["S#" ::: Int]

groupEx = sp & Group (Var @"PQ") (Proxy @'["P#", "QTY"])

ungroupEx = groupEx & Ungroup (Var @"PQ")

renameEx = s & Rename (Var @"S#") (Var @"id")

db =
  EmptyDB
    & createTable (Proxy @Suppliers)
    & createTable (Proxy @Parts)
    & createTable (Proxy @SP)
    & insertMany
      (Var @"suppliers")
      ( map
          (asMap @SHeading)
          [ 1 |> "Smith" |> 20 |> "London" |> Empty,
            2 |> "Jones" |> 10 |> "Paris" |> Empty,
            3 |> "Blake" |> 30 |> "Paris" |> Empty,
            4 |> "Clark" |> 20 |> "London" |> Empty,
            5 |> "Adams" |> 30 |> "Athens" |> Empty
          ]
      )
    & insertMany
      (Var @"parts")
      ( map
          (asMap @PHeading)
          [ 1 |> "Nut" |> Red |> 12 |> "London" |> Empty,
            2 |> "Bolt" |> Green |> 17 |> "Paris" |> Empty,
            3 |> "Screw" |> Blue |> 17 |> "Oslo" |> Empty,
            4 |> "Screw" |> Red |> 14 |> "London" |> Empty,
            5 |> "Cam" |> Blue |> 12 |> "Paris" |> Empty,
            6 |> "Cog" |> Red |> 19 |> "London" |> Empty
          ]
      )
    & insertMany
      (Var @"suppliers-parts")
      ( map
          (asMap @SPHeading)
          [ 1 |> 1 |> 300 |> Empty,
            1 |> 2 |> 200 |> Empty,
            1 |> 3 |> 400 |> Empty,
            1 |> 4 |> 200 |> Empty,
            1 |> 5 |> 100 |> Empty,
            1 |> 6 |> 100 |> Empty,
            2 |> 1 |> 300 |> Empty,
            2 |> 2 |> 400 |> Empty,
            3 |> 2 |> 200 |> Empty,
            4 |> 2 |> 200 |> Empty,
            4 |> 4 |> 300 |> Empty,
            4 |> 5 |> 400 |> Empty
          ]
      )
    -- & DeleteTable (Var @"suppliers")

createTable ::
  ( IsHeading heading k v,
    Member name tables ~ 'False,
    KnownSymbol name
  ) =>
  Proxy (name ::: Table heading k v) ->
  Database tables ->
  Database (name ::: Table heading k v ': tables)
createTable (Proxy :: Proxy (name ::: table)) =
  CreateTable (Var @name) (MkTable :: table)

insert ::
  ( IsHeading heading k v,
    Table heading k v ~ (tables :! name),
    KnownSymbol name
  ) =>
  Var name ->
  Tuple (TableHeading (tables :! name)) ->
  Database tables ->
  Database tables
insert var = Insert var MkTable

insertMany ::
  ( IsHeading heading k v,
    Table heading k v ~ (tables :! name),
    KnownSymbol name,
    Foldable t
  ) =>
  Var name ->
  t (Tuple (TableHeading (tables :! name))) ->
  Database tables ->
  Database tables
insertMany var tuples db = foldr (insert var) db tuples
