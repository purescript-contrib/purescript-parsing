module Parsing.Pos where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

-- | `Position` represents the position of the parser in the input.
-- |
-- | - `index` is the position since the start of the input. Starts at 0.
-- | - `line` is the current line in the input. Starts at 1.
-- | - `column` is the column of the next character in the current line that
-- |   will be parsed. Starts at 1.
newtype Position = Position
  { index :: Int
  , line :: Int
  , column :: Int
  }

derive instance Generic Position _
instance Show Position where
  show x = genericShow x

instance Eq Position where
  eq (Position l) (Position r) = l.index == r.index

instance Ord Position where
  compare (Position l) (Position r) = compare l.index r.index

-- | The `Position` before any input has been parsed.
initialPos :: Position
initialPos = Position { index: 0, line: 1, column: 1 }
