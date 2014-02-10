module Main where

import Prelude
import Either
import Eff
import Parsing

parser :: Parser String String
parser = do
  s1 <- string "a" <|> string "b"
  s2 <- string s1
  return $ s1 ++ s2

parens :: forall a. ({} -> Parser String a) -> Parser String a
parens p = do
  string "("
  a <- p {}
  string ")"
  return a

nested :: {} -> Parser String Number
nested _ = (do 
  string "a"
  return 0) <|> ((+) 1) <$> parens nested

main = do
  case runParser (nested {}) "(((a)))" of
    Left (ParseError err) -> Trace.print err.message
    Right (ParseResult res) -> Trace.print res.result
