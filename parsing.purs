module Parsing where

import Prelude
import Either
import String

data ParseError = ParseError
  { message :: String
  }

parseError :: String -> ParseError
parseError message = ParseError 
  { message: message
  }

data ParseResult s a = ParseResult
  { leftover :: s
  , result :: a
  }

parseResult :: forall s a. s -> a -> ParseResult s a
parseResult leftover result = ParseResult
  { leftover: leftover
  , result: result
  }

data Parser s a = Parser (s -> Either ParseError (ParseResult s a))

runParser :: forall s a. Parser s a -> s -> Either ParseError (ParseResult s a)
runParser (Parser p) s = p s

fail :: String -> forall s a. Parser s a
fail message = Parser $ \s -> Left (parseError message)

string :: String -> Parser String String
string s = Parser $ \s' -> case indexOfS s' s of
  0 -> Right $ parseResult (substr 0 (lengthS s) s') s
  _ -> Left $ parseError $ "Expected \"" ++ s ++ "\""

instance Prelude.Monad (Parser s) where
  ret a = Parser $ \s -> Right (parseResult s a)
  (>>=) (Parser p) f = Parser $ \s -> case p s of
    Left err -> Left err
    Right (ParseResult { leftover = s, result = a }) -> runParser (f a) s

instance Prelude.Alternative (Parser s) where
  empty = fail "No alternative"
  (<|>) p1 p2 = Parser $ \s -> case runParser p1 s of
    Left _ -> runParser p2 s
    Right res -> Right res
