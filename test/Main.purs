module Test.Main where

import Prelude

import Data.Array (some)
import Data.Either
import Data.Identity
import Data.Maybe
import Data.Char (toString)
import Data.String (fromCharArray)
import Data.List (List(..), many, toList)
import Data.Functor (($>))

import Control.Alt
import Control.Alternative
import Control.Apply ((*>))
import Control.Lazy

import Control.Monad.Eff
import Control.Monad.Eff.Console

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Expr
import Text.Parsing.Parser.String
import Text.Parsing.Parser.Token
import Text.Parsing.Parser.Pos

import Test.Assert

parens :: forall m a. (Monad m) => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

nested :: forall m. (Functor m, Monad m) => ParserT String m Int
nested = fix \p -> (do
  string "a"
  return 0) <|> ((+) 1) <$> parens p

parseTest :: forall s a eff. (Show a, Eq a) => s -> a -> Parser s a -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseTest input expected p = case runParser input p of
  Right actual -> assert (expected == actual)
  Left err -> print ("error: " ++ show err)

parseErrorTestPosition :: forall s a eff. (Show a) => Parser s a -> s -> Position -> Eff (console :: CONSOLE, assert :: ASSERT | eff) Unit
parseErrorTestPosition p input expected = case runParser input p of
  Right _ -> print "error: ParseError expected!"
  Left (ParseError { position: pos }) -> assert (expected == pos)

opTest :: Parser String String
opTest = chainl (toString <$> anyChar) (char '+' $> append) ""

digit :: Parser String Int
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

exprTest :: Parser String Int
exprTest = buildExprParser [ [ Infix (string "/" >>= \_ -> return (/)) AssocRight ]
                           , [ Infix (string "*" >>= \_ -> return (*)) AssocRight ]
                           , [ Infix (string "-" >>= \_ -> return (-)) AssocRight ]
                           , [ Infix (string "+" >>= \_ -> return (+)) AssocRight ]
                           ] digit

manySatisfyTest :: Parser String String
manySatisfyTest = do
  r <- some $ satisfy (\s -> s /= '?')
  char '?'
  return (fromCharArray r)

data TestToken = A | B

instance showTestTokens :: Show TestToken where
  show A = "A"
  show B = "B"

instance testTokensEq :: Eq TestToken where
  eq A A = true
  eq B B = true
  eq _ _ = false

isA :: TestToken -> Boolean
isA A = true
isA _ = false

main = do

  parseTest "\n" Nil $ many $ char '\n' *> char '\n'

  parseTest "(((a)))" 3 nested
  parseTest "aaa" (Cons "a" (Cons "a" (Cons "a" Nil))) $ many (string "a")
  parseTest "(ab)" (Just "b") $ parens do
    string "a"
    optionMaybe $ string "b"
  parseTest "a,a,a" (Cons "a" (Cons "a" (Cons "a" Nil))) $ string "a" `sepBy1` string ","
  parseTest "a,a,a," (Cons "a" (Cons "a" (Cons "a" Nil))) $ do
    as <- string "a" `endBy1` string ","
    eof
    return as
  parseTest "a+b+c" "abc" opTest
  parseTest "1*2+3/4-5" (-3) exprTest
  parseTest "ab?" "ab" manySatisfyTest

  let tokpos = const initialPos
  parseTest (toList [A, B]) A (token tokpos)
  parseTest (toList [B, A]) B (token tokpos)

  parseTest (toList [A, B]) A (when tokpos isA)

  parseTest (toList [A]) A (match tokpos A) 
  parseTest (toList [B]) B (match tokpos B) 
  parseTest (toList [A, B]) A (match tokpos A) 

  parseErrorTestPosition (string "abc") "bcd" (Position { column: 1, line: 1 })
  parseErrorTestPosition (string "abc" *> eof) "abcdefg" (Position { column: 4, line: 1 })
  parseErrorTestPosition (string "a\nb\nc\n" *> eof) "a\nb\nc\nd\n" (Position { column: 1, line: 4 })
  parseErrorTestPosition (string "\ta" *> eof) "\tab" (Position { column: 10, line: 1 })
