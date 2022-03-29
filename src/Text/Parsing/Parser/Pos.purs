module Parsing.Pos where

import Prelude

import Data.Generic.Rep (class Generic)

-- | `Position` represents the position of the parser in the input.
-- |
-- | - `line` is the current line in the input
-- | - `column` is the column of the next character in the current line that will be parsed
newtype Position = Position
  { line :: Int
  , column :: Int
  }

derive instance genericPosition :: Generic Position _

instance showPosition :: Show Position where
  show (Position { line: line, column: column }) =
    "(Position { line: " <> show line <> ", column: " <> show column <> " })"

derive instance eqPosition :: Eq Position
derive instance ordPosition :: Ord Position

-- | The `Position` before any input has been parsed.
initialPos :: Position
initialPos = Position { line: 1, column: 1 }
