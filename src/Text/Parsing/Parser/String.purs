module Text.Parsing.Parser.String where

import Prelude

import Data.String
import Data.Either
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Data.Char
import Data.List (List(..), (:), many, some)

import Control.Alt
import Control.Alternative
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.State.Class

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Pos

eof :: forall m. (Monad m) => ParserT String m Unit
eof = ParserT $ \(PState { input: s, position: pos }) ->
  return $ case s of
    "" -> { consumed: false, input: s, result: Right unit, position: pos }
    _  -> parseFailed s pos "Expected EOF"

string :: forall m. (Monad m) => String -> ParserT String m String
string str = ParserT $ \(PState { input: s, position: pos })  ->
  return $ case indexOf str s of
    Just 0 -> { consumed: true, input: drop (length str) s, result: Right str, position: updatePosString pos str }
    _ -> parseFailed s pos ("Expected " ++ str)

char :: forall m. (Monad m) => ParserT String m String
char = ParserT $ \(PState { input: s, position: pos }) ->
  return $ case charAt 0 s of
    Nothing -> parseFailed s pos "Unexpected EOF"
    Just c  -> { consumed: true, input: drop 1 s, result: Right (toString c), position: updatePosString pos (toString c) }

satisfy :: forall m. (Monad m) => (String -> Boolean) -> ParserT String m String
satisfy f = try do
  c <- char
  if f c then return c
         else fail "Character did not satisfy predicate"

whiteSpace :: forall m. (Monad m) => ParserT String m String
whiteSpace = do
  list <- many $ string "\n" <|> string "\r" <|> string " " <|> string "\t"
  return $ foldMap id list

skipSpaces :: forall m. (Monad m) => ParserT String m Unit
skipSpaces = do
  whiteSpace
  return unit

oneOf :: forall s m a. (Monad m) => Array String -> ParserT String m String
oneOf ss = satisfy (flip elem ss)

noneOf :: forall s m a. (Monad m) => Array String -> ParserT String m String
noneOf ss = satisfy (flip notElem ss)
