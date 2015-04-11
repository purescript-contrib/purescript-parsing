module Main where

import Control.Alt ((<|>))
import Control.Alternative (some, many)
import Control.Lazy (fix)
import Control.Monad.Eff (Eff())
import Data.Either (Either(..))
import Debug.Trace (Trace(), trace)
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token

parens :: forall m a. (Monad m) => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

nested :: forall m. (Functor m, Monad m) => ParserT String m Number
nested = fix $ \p -> (do
  string "a"
  return 0) <|> ((+) 1) <$> parens p

parseTest :: forall s a eff. (Show a) => Parser s a -> s -> Eff (trace :: Trace | eff) Unit
parseTest p input = case runParser input p of
  Left (ParseError err) -> trace $ "> " ++ err
  Right result -> trace $ "> " ++ (show result)

opTest :: Parser String String
opTest = chainl char (do string "+"
                         return (++)) ""

digit :: Parser String Number
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

exprTest :: Parser String Number
exprTest = buildExprParser [[Infix (string "/" >>= \_ -> return (/)) AssocRight]
                           ,[Infix (string "*" >>= \_ -> return (*)) AssocRight]
                           ,[Infix (string "-" >>= \_ -> return (-)) AssocRight]
                           ,[Infix (string "+" >>= \_ -> return (+)) AssocRight]] digit

manySatisfyTest :: Parser String [String]
manySatisfyTest = do
    r <- some $ satisfy (\s -> s /= "?")
    string "?"
    return r

data TestToken = A | B

instance showTestTokens :: Show TestToken where
  show A = "A"
  show B = "B"

instance testTokensEq :: Eq TestToken where
  (==) A A = true
  (==) B B = true
  (==) _ _ = false
  (/=) a b = not $ a == b

isA :: TestToken -> Boolean
isA A = true
isA _ = false

main = do
  parseTest nested "(((a)))"
  parseTest (many (string "a")) "aaa"
  parseTest (parens (do
    string "a"
    optionMaybe $ string "b")) "(ab)"
  parseTest (string "a" `sepBy1` string ",") "a,a,a"
  parseTest (do
    as <- string "a" `endBy1` string ","
    eof
    return as) "a,a,a,"
  parseTest opTest "a+b+c"
  parseTest exprTest "1*2+3/4-5"
  parseTest manySatisfyTest "ab?"

  trace "should be A"
  parseTest token [A, B]
  trace "should be B"
  parseTest token [B, A]

  trace "should be A"
  parseTest (when isA) [A, B]
  trace "should fail"
  parseTest (when isA) [B, B]

  trace "should be A"
  parseTest (match A) [A]
  trace "should be B"
  parseTest (match B) [B]
  trace "should be A"
  parseTest (match A) [A, B]
  trace "should fail"
  parseTest (match B) [A, B]
