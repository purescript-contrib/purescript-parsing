module Text.Parsing.Parser.String where

import Prelude

import Data.String

import Control.Monad.State.Class
import Control.Monad.Error.Class

import Text.Parsing.Parser

eof :: forall m. (Monad m) => ParserT String m {}
eof = do
  s <- get
  case s of
    "" -> return {}
    _ -> fail "Expected EOF"

string :: forall m. (Monad m) => String -> ParserT String m String
string s = do
  s' <- get
  case indexOfS s' s of
    0 -> do
      put (Consumed true)
      put (substring (lengthS s) (lengthS s') s')
      return s
    _ -> fail $ "Expected \"" ++ s ++ "\""

char :: forall m. (Monad m) => ParserT String m String
char = do
  s <- get
  case s of
    "" -> fail "Unexpected EOF"
    _ -> do
      put (Consumed true)
      put (substring 1 (lengthS s) s)
      return (substr 0 1 s)

