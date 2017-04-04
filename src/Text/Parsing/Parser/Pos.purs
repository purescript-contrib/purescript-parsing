module Text.Parsing.Parser.Pos where

import Prelude
import Data.Foldable (foldl)
import Data.Newtype (wrap)
import Data.String (split)

-- | `Position` represents the position of the parser in the input.
-- |
-- | - `line` is the current line in the input
-- | - `column` is the column of the next character in the current line that will be parsed
newtype Position = Position
  { line :: Int
  , column :: Int
  }

instance showPosition :: Show Position where
  show (Position { line: line, column: column }) =
    "(Position { line: " <> show line <> ", column: " <> show column <> " })"

derive instance eqPosition :: Eq Position
derive instance ordPosition :: Ord Position

-- | The `Position` before any input has been parsed.
initialPos :: Position
initialPos = Position { line: 1, column: 1 }

-- | Updates a `Position` by adding the columns and lines in `String`.
updatePosString :: Position -> String -> Position
updatePosString pos' str = foldl updatePosChar pos' (split (wrap "") str)
  where
  updatePosChar (Position pos) c = case c of
    "\n" -> Position { line: pos.line + 1, column: 1 }
    "\r" -> Position { line: pos.line + 1, column: 1 }
    "\t" -> Position { line: pos.line,     column: pos.column + 8 - ((pos.column - 1) `mod` 8) }
    _    -> Position { line: pos.line,     column: pos.column + 1 }
