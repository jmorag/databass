-- |

module QueryLanguage where

data Query a = Select [ColumnName] FromClause (WhereClause a) | Insert a TableName

type ColumnName = String

type FromClause = [TableName]
type TableName = String

type WhereClause a = [BinExpr a]

data BinExpr a =
    Eq a a
  | Neq a a
