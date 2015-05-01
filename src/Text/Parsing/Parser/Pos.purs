module Text.Parsing.Parser.Pos where

import Data.String (split)
import Data.Foldable (foldl)

-- | `Position` represents the position of the parser in the input.
-- | - `line` is the current line in the input
-- | - `column` is the column of the next character in the current line that will be parsed
data Position = Position
  { line :: Number
  , column :: Number
  }

instance showPosition :: Show Position where
  show (Position { line: line, column: column }) =
    "Position { line: " ++ show line ++ ", column: " ++ show column ++ " }"

instance eqPosition :: Eq Position where
  (==) (Position { line: l1, column: c1 }) (Position { line: l2, column: c2 }) =
    l1 == l2 && c1 == c2
  (/=) p1 p2 = not (p1 == p2)

-- | The `Position` before any input has been parsed.
initialPos :: Position
initialPos = Position { line: 1, column: 1 }

-- | Updates a `Position` by adding the columns and lines in `String`.
updatePosString :: Position -> String -> Position
updatePosString pos str = foldl updatePosChar pos (split "" str)
  where
  updatePosChar (Position pos) c = case c of
    "\n" -> Position { line: pos.line + 1, column: 1 }
    "\r" -> Position { line: pos.line + 1, column: 1 }
    "\t" -> Position { line: pos.line,     column: pos.column + 8 - (pos.column - 1) % 8 }
    _    -> Position { line: pos.line,     column: pos.column + 1 }
