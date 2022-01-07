module Databass.MapDB where

import qualified Control.Foldl as L
import Control.Lens hiding (Empty)
import Data.List (partition)
import Data.List.NonEmpty (groupBy)
import qualified Data.Map.Strict as M
import Data.Type.Map hiding ((:\))
import Databass.QueryLanguage
import Relude hiding (Map, get, put)

runQuery :: forall tables t. Query t tables -> MapDB tables -> [Tuple t]
runQuery q mem = case q of
  TableIdentity name (MkTable :: Table heading k v) ->
    M.toList (lookp name mem) & map \(k, v) -> (k :: Tuple k) `union` (v :: Tuple v)
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
  Extend var f q -> runQuery q mem & map \tuple -> quicksort (Ext var (f tuple) tuple)
  Summarize var projection folder q ->
    let proj = runQuery projection mem
        tuples = runQuery q mem
     in go proj tuples
    where
      go [] _ = []
      go (p : ps) tuples =
        let (these, rest) = partition (\tuple -> p == submap tuple) tuples
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
  (IsHeading heading k v, Ord (Tuple k), Member name tables ~ 'False) =>
  Var name ->
  Proxy tables ->
  Table heading k v ->
  MapDB tables ->
  MapDB ((name ::: Table heading k v) ': tables)
createTable var Proxy MkTable = Ext var mempty

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
  , (MapDB' tables :! name) ~ TableMem (Table heading k v)
  , IsMember name (TableMem (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMem (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Var name ->
  Proxy tables ->
  Tuple heading ->
  MapDB tables ->
  MapDB tables
insert var _ tuple = let (key, val) = split tuple in over (colLens' var) (M.insert key val)

updateByKey ::
  ( (MapDB' tables :! name) ~ TableMem (Table heading k v)
  , Ord (Tuple k)
  , IsMember name (TableMem (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMem (Table heading k v)) (MapDB' tables) (MapDB' tables)
  ) =>
  Var name ->
  Proxy tables ->
  Tuple k ->
  (Tuple v -> Tuple v) ->
  MapDB tables ->
  MapDB tables
updateByKey var _ key fn = over (colLens' var) (M.adjust fn key)

updateTable ::
  forall name heading k v tables.
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMem (Table heading k v)
  , IsMember name (TableMem (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMem (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Var name ->
  Proxy tables ->
  (Tuple heading -> Bool) ->
  (Tuple heading -> Tuple heading) ->
  MapDB tables ->
  MapDB tables
updateTable var _ where_ fn mdb =
  let (elems :: [Tuple heading]) = map (uncurry union) $ M.toList (mdb ^. colLens' var)
      elems' = over (traversed . filtered where_) fn elems
   in set (colLens' var) (M.fromList (map split elems')) mdb

deleteByKey ::
  ( IsHeading heading k v
  , (MapDB' tables :! name) ~ TableMem (Table heading k v)
  , IsMember name (TableMem (Table heading k v)) (MapDB' tables)
  , Updatable name (TableMem (Table heading k v)) (MapDB' tables) (MapDB' tables)
  , Ord (Tuple k)
  ) =>
  Var name ->
  Proxy tables ->
  Tuple k ->
  MapDB tables ->
  MapDB tables
deleteByKey var _ k = over (colLens' var) (M.delete k)

class InitDB tables where
  initDB :: Proxy tables -> MapDB tables

instance InitDB '[] where
  initDB Proxy = Empty

instance
  (IsHeading heading k v, Ord (Tuple k), Member name tables ~ 'False, InitDB tables) =>
  InitDB (name ::: Table heading k v ': tables)
  where
  initDB (_ :: Proxy (name ::: Table heading k v ': ts)) =
    createTable (Var @name) (Proxy @ts) MkTable (initDB (Proxy @ts))
