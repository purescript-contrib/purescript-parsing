module Text.Parsing.Parser.String where

import Prelude
import Data.String
import Text.Parsing.Parser

eof :: Parser String {}
eof = Parser $ \s -> case s of
  "" -> successResult s false {}
  _ -> failureResult s false $ parseError "Expected EOF"

string :: String -> Parser String String
string s = Parser $ \s' -> case indexOfS s' s of
  0 -> successResult (substring (lengthS s) (lengthS s') s') true s
  _ -> failureResult s' false $ parseError $ "Expected \"" ++ s ++ "\""

char :: Parser String String
char = Parser $ \s -> case s of
  "" -> failureResult s false $ parseError "Unexpected EOF"
  _ -> successResult (substring 1 (lengthS s) s) true (substr 0 1 s)

