module Main where

import Prelude
import Either
import Eff
import Parsing
import Trace
import Arrays
import Maybe

parens :: forall a. ({} -> Parser String a) -> Parser String a
parens = between (string "(") (string ")")

nested :: {} -> Parser String Number
nested _ = (do 
  string "a"
  return 0) <|> ((+) 1) <$> parens nested

parseTest :: forall s a eff. (Show a) => Parser s a -> s -> Eff (trace :: Trace | eff) {}
parseTest p input = case runParser p input of
  ParseResult { result = Left (ParseError err) } -> Trace.print err.message
  ParseResult { result = Right result } -> Trace.print result

opTest = chainl char (do string "+"
                         return (++)) ""

main = do
  parseTest (nested {}) "(((a)))"
  parseTest (many (string "a")) "aaa"
  parseTest (parens (const $ do
    string "a"
    optionMaybe $ string "b")) "(ab)"
  parseTest (string "a" `sepBy1` string ",") "a,a,a"
  parseTest (do
    as <- string "a" `endBy1` string ","
    eof
    return as) "a,a,a,"  
  parseTest opTest "a+b+c"
