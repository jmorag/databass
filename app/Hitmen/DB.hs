{-# LANGUAGE OverloadedStrings #-}

module Hitmen.DB where

import Data.Time
import Data.Type.Map
import Data.Type.Set (Sort, type (:++))
import Databass
import Relude

type family Timed heading where
  Timed heading = heading :++ '["created_at" ::: UTCTime, "updated_at" ::: UTCTime]

type Handler =
  '[ "id" ::: Int
   , "codename" ::: Text
   , "created_at" ::: UTCTime
   , "updated_at" ::: UTCTime
   ]

type Hitman =
  '[ "id" ::: Int
   , "codename" ::: Text
   , "handler_id" ::: Int
   , "created_at" ::: UTCTime
   , "updated_at" ::: UTCTime
   ]

type PursuingMark =
  '[ "hitman_id" ::: Int
   , "mark_id" ::: Int
   , "created_at" ::: UTCTime
   , "updated_at" ::: UTCTime
   ]

type ErasedMark =
  '[ "hitman_id" ::: Int
   , "mark_id" ::: Int
   , "awarded_bounty" ::: Int
   , "created_at" ::: UTCTime
   , "updated_at" ::: UTCTime
   ]

type Mark =
  '[ "id" ::: Int
   , "list_bounty" ::: Int
   , "first_name" ::: Text
   , "last_name" ::: Text
   , "description" ::: Maybe Text
   , "created_at" ::: UTCTime
   , "updated_at" ::: UTCTime
   ]

type Schema =
  Sort
    '[ "handlers" ::: T (Sort Handler) '["id"]
     , "hitmen" ::: T (Sort Hitman) '["id"]
     , "pursuing_marks" ::: T (Sort PursuingMark) '["hitman_id", "mark_id"]
     , "erased_marks" ::: T (Sort ErasedMark) '["hitman_id", "mark_id"]
     , "marks" ::: T (Sort Mark) '["id"]
     ]

makeDB :: IO (MapDB Schema)
makeDB = do
  now <- getCurrentTime
  pure $
    initDB @Schema
      & insertMany @"handlers" @Schema
        ( map
            (asMap @Handler)
            [ 1 <| "Olive" <| now <| now <| Empty
            , 2 <| "Pallas" <| now <| now <| Empty
            ]
        )
      & insertMany @"hitmen" @Schema
        ( map
            (asMap @Hitman)
            [ 1 <| "Callaird" <| 1 <| now <| now <| Empty
            , 2 <| "Bomois" <| 1 <| now <| now <| Empty
            , 3 <| "Dune" <| 2 <| now <| now <| Empty
            ]
        )
      & insertMany @"marks" @Schema
        ( map
            (asMap @Mark)
            [ 1 <| 25000 <| "John" <| "Tosti" <| Nothing <| now <| now <| Empty
            , 2 <| 50000 <| "Macie" <| "Jordan" <| Nothing <| now <| now <| Empty
            , 3 <| 33000 <| "Sal" <| "Aspot" <| Nothing <| now <| now <| Empty
            , 4 <| 10000 <| "Lars" <| "Andersen" <| Nothing <| now <| now <| Empty
            ]
        )
      & insertMany @"pursuing_marks" @Schema
        ( map
            (asMap @PursuingMark)
            [ 1 <| 2 <| read "2018-07-01 00:00:00 UTC" <| now <| Empty
            , 2 <| 2 <| read "2018-07-02 00:00:00 UTC" <| now <| Empty
            , 2 <| 4 <| read "2019-05-05 00:00:00 UTC" <| now <| Empty
            , 3 <| 3 <| read "2018-05-13 00:00:00 UTC" <| now <| Empty
            , 3 <| 2 <| read "2019-02-15 00:00:00 UTC" <| now <| Empty
            ]
        )
      & insertMany @"erased_marks" @Schema
        ( map
            (asMap @ErasedMark)
            [ 1 <| 2 <| 30000 <| read "2018-09-03 00:00:00 UTC" <| now <| Empty
            , 1 <| 1 <| 55000 <| read "2019-02-02 00:00:00 UTC" <| now <| Empty
            , 3 <| 3 <| 27000 <| read "2018-06-30 00:00:00 UTC" <| now <| Empty
            ]
        )
