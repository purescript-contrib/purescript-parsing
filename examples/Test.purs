module Main where

import Data.Array
import Data.Either
import Data.Identity
import Data.Maybe

import Control.Alt
import Control.Alternative
import Control.Apply ((*>))
import Control.Monad.Eff
import Control.Lazy

import Debug.Trace

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token
import Text.Parsing.Parser.Pos

parens :: forall m a. (Monad m) => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

nested :: forall m. (Functor m, Monad m) => ParserT String m Number
nested = fix1 $ \p -> (do
  string "a"
  return 0) <|> ((+) 1) <$> parens p

parseTest :: forall s a eff. (Show a) => Parser s a -> s -> Eff (trace :: Trace | eff) Unit
parseTest p input = case runParser input p of
  Left (ParseError err) -> print err.message
  Right result -> print result

parseErrorTestPosition :: forall s a eff. (Show a) => Parser s a -> s -> Position -> Eff (trace :: Trace | eff) Unit
parseErrorTestPosition p input expected = case runParser input p of
  Right _ -> print "error: ParseError expected!"
  Left (ParseError { position: pos }) -> case expected == pos of
    true  -> print $ "ok, as expected: " ++ show pos
    false -> print $ "error: got " ++ show pos ++ " instead of " ++ show expected



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

  let tokpos = const initialPos
  print "should be A"
  parseTest (token tokpos) [A, B]
  print "should be B"
  parseTest (token tokpos) [B, A]

  print "should be A"
  parseTest (when tokpos isA) [A, B]
  print "should fail"
  parseTest (when tokpos isA) [B, B]

  print "should be A"
  parseTest (match tokpos A) [A]
  print "should be B"
  parseTest (match tokpos B) [B]
  print "should be A"
  parseTest (match tokpos A) [A, B]
  print "should fail"
  parseTest (match tokpos B) [A, B]

  parseErrorTestPosition (string "abc") "bcd" (Position { column: 1, line: 1 })
  parseErrorTestPosition (string "abc" *> eof) "abcdefg" (Position { column: 4, line: 1 })
  parseErrorTestPosition (string "a\nb\nc\n" *> eof) "a\nb\nc\nd\n" (Position { column: 1, line: 4 })
  parseErrorTestPosition (string "\ta" *> eof) "\tab" (Position { column: 10, line: 1 })
