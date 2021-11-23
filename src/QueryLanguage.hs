module QueryLanguage where

-- data Query a = Select [ColumnName] FromClause (WhereClause a) | Insert a TableName

-- type ColumnName = String

-- type FromClause = [TableName]
-- type TableName = String

-- type WhereClause a = [BinExpr a]

-- data BinExpr a =
--     Eq a a
--   | Neq a a

import qualified Control.Foldl as L
import Data.Binary
import qualified Data.ByteString.Lazy as BL
import Relude
import qualified Prelude as P

data DBVal
  = DBInt Int
  | DBDouble Double
  | DBBytes LByteString
  | DBString String
  | DBArray [DBVal]
  -- put other stuff like date-times, json, xml here
  deriving (Show, Eq, Generic)
instance Binary DBVal

data DBType = TyInt | TyDouble | TyBytes | TyString | TyArray DBType
  deriving (Show, Eq, Generic)

-- Change this to Text or ByteString if desired
type Name = String

type Tuple = Map Name DBVal

data Table = MkTable {heading :: Map Name DBType, key :: [Name]}
  deriving (Show, Eq, Generic)

data DBStatement
  = CreateTable Name Table
  | DeleteTable Name
  | Insert Name Tuple
  | Delete Name (Tuple -> Bool)
  deriving (Generic)

data Query
  = TableIdentity Name
  | Rename Name Name Query
  | -- | Restrict uses any haskell function from tuples to booleans. You can use
    -- a BinExpr type thing instead. Same goes for Extend, Summarize, and Delete
    -- in Database. In fact, if you want to write a Parser that consumes
    -- non-haskell program text, you will need to do this
    Restrict (Tuple -> Bool) Query
  | Project [Name] Query
  | Join Query Query
  | Extend Name (Tuple -> DBVal) Query
  | Summarize Name Query (L.Fold Tuple DBVal) Query
  -- You might need to extend DBVal and DBType to include nested tuples to
  -- represent these
  | Group Name [Name] Query
  | UnGroup Name Query
  deriving (Generic)

-- Outer map is tables, inner maps are from table keys to the rest of the row
type MemDB = Map Name (Map Tuple Tuple)

materializeDB :: [DBStatement] -> Maybe MemDB
materializeDB = P.error "not yet implemented"

runQuery :: Query -> MemDB -> Maybe [Tuple]
runQuery = P.error "not yet implemented"
