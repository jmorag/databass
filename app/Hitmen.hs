{-# LANGUAGE OverloadedLabels #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

import qualified Control.Foldl as L
import Control.Lens
import qualified Data.List
import Data.Time
import Data.Type.Map
import Data.Type.Set (Sort)
import Databass
import Hitmen.DB

getAllHitmen :: Query _ Schema
getAllHitmen = table @"hitmen"

getAllHitmenActiveMarks :: Query _ Schema
getAllHitmenActiveMarks =
  ( (table @"pursuing_marks" & project @'["hitman_id", "mark_id"])
      \\ (table @"erased_marks" & project @'["hitman_id", "mark_id"])
  )
    & project @'["hitman_id"]
    & rename @"hitman_id" @"id"
    & join (table @"hitmen")

erasedSince :: UTCTime -> Query (Sort Mark) Schema
erasedSince t =
  table @"erased_marks"
    & restrict (\mark -> mark ^. #created_at >= t)
    & project @'["mark_id"]
    & rename @"mark_id" @"id"
    & join (table @"marks")

erasedSinceBy :: UTCTime -> Int -> Query (Sort Mark) Schema
erasedSinceBy t hitmanId =
  table @"erased_marks"
    & restrict (\mark -> mark ^. #created_at >= t && mark ^. #hitman_id == hitmanId)
    & project @'["mark_id"]
    & rename @"mark_id" @"id"
    & join (table @"marks")

totalBounties :: Query _ Schema
totalBounties =
  table @"erased_marks"
    & summarize @"total_bounty"
      (project @'["id"] (table @"hitmen") & rename @"id" @"hitman_id")
      (L.premap (view #awarded_bounty) L.sum)

totalBounty :: Int -> Query _ Schema
totalBounty hitmanId = totalBounties & restrict (\m -> m ^. #hitman_id == hitmanId)

latestKills :: Query _ Schema
latestKills =
  table @"erased_marks"
    & summarize' @"latest_kill" @'["hitman_id"]
      (L.premap (view #created_at) L.maximum)

singlePursuer :: Query _ Schema
singlePursuer =
  table @"pursuing_marks"
    & group @"pursuers" @'["created_at", "hitman_id", "updated_at"]
    & restrict (\t -> length (t ^. #pursuers) == 1)
    & project @'["mark_id"]
    & join (table @"marks" & rename @"id" @"mark_id")

opportunity :: Query _ Schema
opportunity =
  (table @"erased_marks" & rename @"hitman_id" @"erased_hitman_id" & project @'["erased_hitman_id", "mark_id"])
    & join (table @"pursuing_marks" & rename @"hitman_id" @"pursuing_hitman_id" & project @'["mark_id", "pursuing_hitman_id"])
    & restrict (\m -> m ^. #erased_hitman_id /= m ^. #pursuing_hitman_id)
    & project @'["mark_id"]
    & join (table @"marks" & rename @"id" @"mark_id")

main :: IO ()
main = do
  db <- makeDB
  let query :: (Eq (Tuple t), Show (Tuple t)) => String -> Query t Schema -> IO ()
      query desc q = putStrLn desc >> (runQuery db q & Data.List.nub & mapM_ print)
  query "get all hitmen" getAllHitmen
  query "get all hitmen pursuing active marks" getAllHitmenActiveMarks
  query "get all marks that have been erased since a given date (2019-01-01)" (erasedSince (UTCTime (read "2019-01-01") 0))
  query "get all marks that have been erased since a given date (2018-08-01)" (erasedSince (UTCTime (read "2018-08-01") 0))
  query "get all marks that have been erased since a given date (2018-08-01) by Callaird" (erasedSinceBy (UTCTime (read "2018-08-01") 0) 1)
  query "get the total bounty awarded to each hitman" totalBounties
  query "get the total bounty awarded to a particular hitman" (totalBounty 1)
  query "get each hitman's latest kill" latestKills
  query "get all the active marks that have only a single pursuer" singlePursuer
  query "get all the marks of opportunity" opportunity
