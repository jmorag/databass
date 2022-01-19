module Databass.MapDB where

import qualified Control.Foldl as L
import Control.Lens hiding (Empty)
import qualified Data.IntMap.Strict as IM
import qualified Data.List as List
import Data.List.NonEmpty (groupBy)
import qualified Data.Map.Strict as M
import Data.Type.Map hiding ((:\))
import Databass.QueryLanguage
import Relude hiding (Map, get, put)

runQuery :: forall tables t. Query t tables -> MapDB tables -> [Tuple t]
runQuery q mem = case q of
  TableIdentity (_ :: Proxy map) name (MkTable :: Table heading k v) ->
    (enumerate @map) (lookp name mem) & map \(k, v) -> (k :: Tuple k) `union` (v :: Tuple v)
  Rename v1 v2 q' -> map (quicksort . renameTuple v1 v2) $ runQuery q' mem
  Restrict pred q' -> filter pred (runQuery q' mem)
  Project q' -> map submap (runQuery q' mem)
  -- TODO: This is the most naive possible nested loop O(n*m) join algorithm
  -- See https://en.wikipedia.org/wiki/Category:Join_algorithms for more ideas
  Join (_ :: Proxy t_l_rest) (_ :: Proxy t_r_rest) q1 q2 -> do
    l :: Tuple t_l <- runQuery q1 mem
    r :: Tuple t_r <- runQuery q2 mem
    let (l_common, l_rest) = split @(Intersection t_l t_r) @t_l_rest l
        (r_common, r_rest) = split @(Intersection t_l t_r) @t_r_rest r
    guard (l_common == r_common)
    pure (quicksort $ append l_common (append l_rest r_rest))
  QueryUnion q1 q2 -> runQuery q1 mem `List.union` runQuery q2 mem
  Intersection q1 q2 -> runQuery q1 mem `List.intersect` runQuery q2 mem
  Difference q1 q2 -> runQuery q1 mem List.\\ runQuery q2 mem
  Extend var f q -> runQuery q mem & map \tuple -> quicksort (Ext var (f tuple) tuple)
  Summarize var projection folder q ->
    let proj = List.nub $ runQuery projection mem
        tuples = runQuery q mem
     in go proj tuples
    where
      go [] _ = []
      go (p : ps) tuples =
        let (these, rest) = List.partition (\tuple -> p == submap tuple) tuples
         in quicksort (Ext var (L.fold folder these) p) : go ps rest
  Group var (_ :: Proxy grouped) (_ :: Proxy rest) q ->
    let splits = runQuery q mem & map (split @grouped @rest) & sortWith snd
        groups = groupBy ((==) `on` snd) splits
     in groups & map \((grouped, rest) :| gs) ->
          quicksort (Ext var (grouped : fmap fst gs) rest)
  Ungroup (_ :: Var l) (_ :: Proxy nested) (_ :: Proxy rest) q ->
    runQuery q mem & concatMap \tuple ->
      let (Ext _ nested Empty, rest) = split @'[l ::: [Tuple nested]] @rest tuple
       in nested & map \group -> quicksort (append group rest)

createTable ::
  ( IsHeading heading k v
  , Ord (Tuple k)
  , Member name tables ~ 'False
  ) =>
  Var name ->
  Proxy tables ->
  Proxy k ->
  Proxy v ->
  Table heading k v ->
  MapDB tables ->
  MapDB ((name ::: Table heading k v) ': tables)
createTable var _ (_ :: Proxy k) (_ :: Proxy v) (MkTable :: Table heading k v) = Ext var (Databass.QueryLanguage.fromList @(TableMap (Table heading k v)) @k @v [])

removeTable ::
  forall name tables remaining.
  (Member name tables ~ 'True, (tables :\ name) ~ remaining, Submap (MapDB' remaining) (MapDB' tables)) =>
  Var name ->
  Proxy tables ->
  Proxy remaining ->
  MapDB tables ->
  MapDB remaining
removeTable _ _ _ = submap @(MapDB' remaining)

insert ::
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Var name ->
  Proxy k ->
  Proxy tables ->
  Tuple heading ->
  MapDB tables ->
  MapDB tables
insert var (_ :: Proxy k) _ tuple =
  let (key :: Tuple k, val) = split tuple
   in over (colLens' var) (Databass.QueryLanguage.insertTuple key val)

insertIncreasing ::
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  , Enum (Tuple k)
  , Bounded (Tuple k)
  ) =>
  Var name ->
  Proxy k ->
  Proxy tables ->
  Tuple v ->
  MapDB tables ->
  MapDB tables
insertIncreasing var (_ :: Proxy k) _ val db =
  let key :: Tuple k = case lookupMax @_ @k (db ^. colLens' var) of
        Nothing -> minBound
        Just (key', _) -> succ key'
   in over (colLens' var) (Databass.QueryLanguage.insertTuple key val) db

updateByKey ::
  ( IsHeading heading k v
  , Ord (Tuple k)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  ) =>
  Var name ->
  Proxy tables ->
  Proxy heading ->
  Tuple k ->
  (Tuple v -> Tuple v) ->
  MapDB tables ->
  MapDB tables
updateByKey var _ _ key fn = over (colLens' var) (adjust fn key)

updateTable ::
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Var name ->
  Proxy tables ->
  Proxy k ->
  Proxy heading ->
  (Tuple heading -> Bool) ->
  (Tuple heading -> Tuple heading) ->
  MapDB tables ->
  MapDB tables
updateTable var _ (_ :: Proxy k) (_ :: Proxy heading) where_ fn mdb =
  let (elems :: [Tuple heading]) = map (uncurry (union @k)) $ enumerate (mdb ^. colLens' var)
      (elems' :: [Tuple heading]) = over (traversed . filtered where_) fn elems
   in set (colLens' var) (Databass.QueryLanguage.fromList @_ @k (map split elems')) mdb

deleteByKey ::
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMap (Table heading k v)
  , IsMember name (TableMap (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMap (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Var name ->
  Proxy tables ->
  Proxy v ->
  Tuple k ->
  MapDB tables ->
  MapDB tables
deleteByKey var _ _ k = over (colLens' var) (delete k)

class InitDB tables where
  initDB :: Proxy tables -> MapDB tables

instance InitDB '[] where
  initDB Proxy = Empty

instance
  (IsHeading heading k v, Ord (Tuple k), Member name tables ~ 'False, InitDB tables) =>
  InitDB (name ::: Table heading k v ': tables)
  where
  initDB (_ :: Proxy (name ::: Table heading k v ': ts)) =
    createTable (Var @name) (Proxy @ts) (Proxy @k) (Proxy @v) MkTable (initDB (Proxy @ts))
