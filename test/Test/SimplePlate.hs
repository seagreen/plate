-- | A minimal description of Plate.
--
-- For a production implementation see the 'Plate' module.
module Test.SimplePlate where

import Protolude hiding (exp)

--------------------------------------------------
-- * Plate Abstract Data Types
--------------------------------------------------

data Plate
  = PDictionary (Map Plate Plate)
  | PSequence   [Plate]
  | PInteger    Integer
  deriving (Eq, Ord, Show)

type PString = [Integer]

--------------------------------------------------
-- * Plate Schemas
--------------------------------------------------

data Expression
  = Variable    Text
  | Abstraction Text Expression
  | Application Expression Expression
  | Builtin     Schema
  deriving (Eq, Ord, Show)

data Schema
  = SumType     (Map Text Expression)
  | ProductType (Map Text Expression)
  | SDictionary Expression Expression
  | SSequence   Expression
  | SInteger
  deriving (Eq, Ord, Show)
