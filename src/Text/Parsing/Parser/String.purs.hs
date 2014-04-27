module Text.Parsing.Parser.String where

import Prelude

import Data.String

import Control.Monad.State.Class
import Control.Monad.Error.Class

import Data.Foldable
import Data.Monoid

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators

eof :: forall m. (Monad m) => ParserT String m {}
eof = do
  s <- get
  case s of
    "" -> return {}
    _ -> fail "Expected EOF"

string :: forall m. (Monad m) => String -> ParserT String m String
string s = do
  s' <- get
  case indexOf s s' of
    0 -> do
      put (Consumed true)
      put (drop (length s) s')
      return s
    _ -> fail $ "Expected \"" ++ s ++ "\""

char :: forall m. (Monad m) => ParserT String m String
char = do
  s <- get
  case s of
    "" -> fail "Unexpected EOF"
    _ -> do
      put (Consumed true)
      put (drop 1 s)
      return (take 1 s)

satisfy :: forall m. (Monad m) => (String -> Boolean) -> ParserT String m String
satisfy f = do
  p <- char
  r <- if not $ f p then fail "Character did not satisfy prediate" else return p
  return r

whiteSpace :: forall m. (Monad m) => ParserT String m String
whiteSpace = do
  list <- many $ string "\n" <|> string "\r" <|> string " " <|> string "\t"
  return $ foldMap id list

