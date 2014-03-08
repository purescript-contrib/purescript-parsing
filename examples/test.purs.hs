module Main where

import Prelude
import Data.Array
import Data.Either
import Data.Maybe
import Control.Monad.Eff
import Debug.Trace
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String

parens :: forall a. ({} -> Parser String a) -> Parser String a
parens = between (string "(") (string ")")

nested :: {} -> Parser String Number
nested _ = (do 
  string "a"
  return 0) <|> ((+) 1) <$> parens nested

parseTest :: forall s a eff. (Show a) => Parser s a -> s -> Eff (trace :: Trace | eff) {}
parseTest p input = case runParser p input of
  ParseResult { result = Left (ParseError err) } -> print err.message
  ParseResult { result = Right result } -> print result

opTest = chainl char (do string "+"
                         return (++)) ""

digit = (string "0" >>= \_ -> return 0) 
        <|> (string "1" >>= \_ -> return 1) 
        <|> (string "2" >>= \_ -> return 2) 
        <|> (string "3" >>= \_ -> return 3) 
        <|> (string "4" >>= \_ -> return 4) 
        <|> (string "5" >>= \_ -> return 5) 
        <|> (string "6" >>= \_ -> return 6) 
        <|> (string "7" >>= \_ -> return 7) 
        <|> (string "8" >>= \_ -> return 8) 
        <|> (string "9" >>= \_ -> return 9) 

exprTest = buildExprParser [[Infix (string "/" >>= \_ -> return (/)) AssocRight]
                           ,[Infix (string "*" >>= \_ -> return (*)) AssocRight]
                           ,[Infix (string "-" >>= \_ -> return (-)) AssocRight]
                           ,[Infix (string "+" >>= \_ -> return (+)) AssocRight]] digit

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
  parseTest exprTest "1*2+3/4-5"
