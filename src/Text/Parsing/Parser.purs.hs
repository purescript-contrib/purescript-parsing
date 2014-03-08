module Text.Parsing.Parser where

import Prelude
import Control.Monad
import Data.Array
import Data.Either
import Data.Maybe

data ParseError = ParseError
  { message :: String
  }

parseError :: String -> ParseError
parseError message = ParseError 
  { message: message
  }

data ParseResult s a = ParseResult
  { leftover :: s
  , consumed :: Boolean
  , result :: Either ParseError a
  }

parseResult :: forall s a. s -> Boolean -> Either ParseError a -> ParseResult s a
parseResult leftover consumed result = ParseResult
  { leftover: leftover
  , consumed: consumed
  , result: result
  }

successResult :: forall s a. s -> Boolean -> a -> ParseResult s a
successResult leftover consumed result = parseResult leftover consumed (Right result)

failureResult :: forall s a. s -> Boolean -> ParseError -> ParseResult s a
failureResult leftover consumed err = parseResult leftover consumed (Left err)

instance functorParseResult :: Prelude.Functor (ParseResult s) where
  (<$>) f (ParseResult o) = parseResult o.leftover o.consumed (f <$> o.result)

data Parser s a = Parser (s -> ParseResult s a)

runParser :: forall s a. Parser s a -> s -> ParseResult s a
runParser (Parser p) s = p s

instance monadParser :: Prelude.Monad (Parser s) where
  return a = Parser $ \s -> successResult s false a
  (>>=) p f = Parser $ \s -> case runParser p s of
    ParseResult ({ leftover = s', consumed = consumed, result = Left err }) -> failureResult s' consumed err
    ParseResult ({ leftover = s', consumed = consumed, result = Right a }) -> runParser (f a) s'

instance monadAlternative :: Prelude.Alternative (Parser s) where
  empty = fail "No alternative"
  (<|>) p1 p2 = Parser $ \s -> case runParser p1 s of
    ParseResult ({ leftover = s', consumed = false, result = Left _ }) -> runParser p2 s
    res -> res

fail :: forall s a. String -> Parser s a
fail message = Parser $ \s -> failureResult s false (parseError message)


  
