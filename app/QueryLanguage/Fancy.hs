{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
module QueryLanguage.Fancy where

import Data.Singletons.Base.TH
import GHC.TypeLits
import Prelude.Singletons
import Relude hiding (Map, show)
import Prelude (show)

data ColumnConstraint table
  = PrimaryKey
  | ForeignKey table
  | Unique
  | NoConstraint

newtype
  Column
    (label :: Symbol)
    (constraint :: ColumnConstraint table)
    (a :: Type)
  = Column a

data Row a where
  RNil :: Row '[]
  (:::) :: Column label constraint value -> Row xs -> Row (Column label constraint value ': xs)

instance Show (Row '[]) where
  show _ = "RNil"

instance (Show value, Show (Row rest), KnownSymbol label) => Show (Row (Column label constraint value ': rest)) where
  show (Column value ::: rest) =
    "(" <> symbolVal (Proxy @label) <> ": " <> show value <> ") "
      <> show rest

infixr 5 :::

type TestRow = Row '[Column "id" 'PrimaryKey Int, Column "name" 'NoConstraint String]

testRow :: TestRow
testRow = Column 1 ::: Column "Joseph" ::: RNil

-- Arguments are flipped but passing flip at the type level is SUPER ANNOYING
type family LookupLabel row (label :: Symbol) where
  LookupLabel (Row (Column label _ t ': _)) label = t
  LookupLabel (Row (Column label' _ t ': cols)) label = LookupLabel (Row cols) label
  LookupLabel (Row '[]) label = TypeError ( 'Text "Could not find field " ':<>: 'Text label)

genDefunSymbols [''LookupLabel]

data Query (labels :: [Symbol]) row a where
  Select :: Query labels row (Map (Apply LookupLabelSym0 row) labels)
