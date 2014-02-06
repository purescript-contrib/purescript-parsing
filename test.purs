module Main where

import Prelude
import Either
import Eff
import Parsing

parser :: Parser String String
parser = do
  string "a" <|> string "b"
  string "c" <|> string "d"

main = do
  case runParser parser "ad" of
    Left (ParseError err) -> Trace.print err.message
    Right (ParseResult res) -> Trace.print res.result
