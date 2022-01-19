{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import qualified Control.Foldl as L
import Control.Lens hiding (Empty, Identity, (<|))
import Data.Aeson
import Data.Binary
import Data.Type.Map
import Databass
import GHC.Generics

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

type SHeading =
  '[ "S#" ::: Int
   , "SNAME" ::: String
   , "STATUS" ::: Int
   , "CITY" ::: String
   ]

type Suppliers = "suppliers" ::: T (AsMap SHeading) '["S#"]

data Color = Red | Green | Blue deriving (Show, Eq, Generic)

instance Binary Color

instance ToJSON Color

type PHeading =
  '[ "P#" ::: Int
   , "PNAME" ::: String
   , "COLOR" ::: Color
   , "WEIGHT" ::: Double
   , "CITY" ::: String
   ]

type Parts = "parts" ::: T (AsMap PHeading) '["P#"]

type SPHeading =
  '[ "S#" ::: Int
   , "P#" ::: Int
   , "QTY" ::: Int
   ]

type SP = "suppliers-parts" ::: T (AsMap SPHeading) '["P#", "S#"]

type Tables = '[SP, Parts, Suppliers]

s :: Query _ Tables
s = table @"suppliers"

p :: Query _ Tables
p = table @"parts"

sp :: Query _ Tables
sp = table @"suppliers-parts"

fancyQuery :: Query _ Tables
fancyQuery = s & project @'["CITY", "STATUS"] & restrict (\t -> t ^. col @"STATUS" < 30)

extendEx = s & extend @"TRIPLE" (\t -> t ^. col @"STATUS" * 3)

summarizeEx = sp & summarize @"P_COUNT" (project @'["S#"] s) L.length

summarizeEx2 =
  s
    & summarize @"AVG_STATUS"
      (project @'["CITY"] s)
      (L.premap (fromIntegral . view (col @"STATUS")) L.mean)

projectEx = s & project @'["S#"]

groupEx = sp & group @"PQ" @'["P#", "QTY"]

ungroupEx = groupEx & ungroup @"PQ"

renameEx = s & rename @"S#" @"id"

joinEx = s >< p

sTup1, sTup2 :: Tuple (AsMap SHeading)
sTup1 = asMap @SHeading $ 1 <| "Smith" <| 20 <| "London" <| Empty
sTup2 = asMap @SHeading $ 2 <| "Smith" <| 10 <| "Paris" <| Empty

pTup1, pTup2 :: Tuple (AsMap PHeading)
pTup1 = asMap @PHeading $ 1 <| "Nut" <| Red <| 12 <| "London" <| Empty
pTup2 = asMap @PHeading $ 2 <| "Bolt" <| Green <| 17 <| "Paris" <| Empty

spTup1, spTup2 :: Tuple (AsMap SPHeading)
spTup1 = asMap @SPHeading $ 1 <| 1 <| 300 <| Empty
spTup2 = asMap @SPHeading $ 1 <| 2 <| 200 <| Empty

db =
  initDB @Tables
    & insertMany @"suppliers" @Tables
      ( map
          (asMap @SHeading)
          [ 1 <| "Smith" <| 20 <| "London" <| Empty
          , 2 <| "Jones" <| 10 <| "Paris" <| Empty
          , 3 <| "Blake" <| 30 <| "Paris" <| Empty
          , 4 <| "Clark" <| 20 <| "London" <| Empty
          , 5 <| "Adams" <| 30 <| "Athens" <| Empty
          ]
      )
    & insertMany @"parts" @Tables
      ( map
          (asMap @PHeading)
          [ 1 <| "Nut" <| Red <| 12 <| "London" <| Empty
          , 2 <| "Bolt" <| Green <| 17 <| "Paris" <| Empty
          , 3 <| "Screw" <| Blue <| 17 <| "Oslo" <| Empty
          , 4 <| "Screw" <| Red <| 14 <| "London" <| Empty
          , 5 <| "Cam" <| Blue <| 12 <| "Paris" <| Empty
          , 6 <| "Cog" <| Red <| 19 <| "London" <| Empty
          ]
      )
    & insertMany @"suppliers-parts" @Tables
      ( map
          (asMap @SPHeading)
          [ 1 <| 1 <| 300 <| Empty
          , 1 <| 2 <| 200 <| Empty
          , 1 <| 3 <| 400 <| Empty
          , 1 <| 4 <| 200 <| Empty
          , 1 <| 5 <| 100 <| Empty
          , 1 <| 6 <| 100 <| Empty
          , 2 <| 1 <| 300 <| Empty
          , 2 <| 2 <| 400 <| Empty
          , 3 <| 2 <| 200 <| Empty
          , 4 <| 2 <| 200 <| Empty
          , 4 <| 4 <| 300 <| Empty
          , 4 <| 5 <| 400 <| Empty
          ]
      )

main :: IO ()
main = do
  let testQuery desc q = putStrLn desc >> (runQuery db q & mapM_ print)
  testQuery "S" s
  testQuery "P" p
  testQuery "EXTEND s ADD (3 * STATUS as TRIPLE)" extendEx
  testQuery "SUMMARIZE SP PER ( S { S# } ) ADD ( COUNT ( ) AS P_COUNT )" summarizeEx
  testQuery "SUMMARIZE S PER ( S { CITY } ) ADD ( AVG ( STATUS ) AS AVG_STATUS )" summarizeEx2
  testQuery "S { S# }" projectEx
  testQuery "SP GROUP ({ P#, QTY } AS PQ)" groupEx
  testQuery "SPQ UNGROUP (PQ)" ungroupEx
  testQuery "S RENAME (S# AS id)" renameEx
  testQuery "S JOIN P" joinEx
  Data.Binary.encodeFile "test.databass" db
  db' <- Data.Binary.decodeFile "test.databass"
  print (db == db')
