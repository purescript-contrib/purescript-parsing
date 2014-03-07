module Text.Parsing.Parser where

import Prelude
import Data.Array
import Data.Either
import Data.Maybe
import Data.String

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
    ParseResult ({ leftover = s', consumed = consumed, result = Right a }) -> runParser (f a) s' -- TODO

instance monadAlternative :: Prelude.Alternative (Parser s) where
  empty = fail "No alternative"
  (<|>) p1 p2 = Parser $ \s -> case runParser p1 s of
    ParseResult ({ leftover = s', consumed = false, result = Left _ }) -> runParser p2 s
    res -> res

-- Polymorphic Parser Combinators

fail :: forall s a. String -> Parser s a
fail message = Parser $ \s -> failureResult s false (parseError message)

(<?>) :: forall s a. Parser s a -> String -> Parser s a
(<?>) p msg = p <|> fail msg

many :: forall s a. Parser s a -> Parser s [a]
many p = many1 p <|> return []

many1 :: forall s a. Parser s a -> Parser s [a]
many1 p = do a <- p
             as <- many p
             return (a : as)

between :: forall s a open close. Parser s open -> Parser s close -> ({} -> Parser s a) -> Parser s a
between open close p = do
  open
  a <- p {}
  close 
  return a

option :: forall s a. a -> Parser s a -> Parser s a
option a p = p <|> return a

optional :: forall s a. Parser s a -> Parser s {}
optional p = (do p
                 return {}) <|> return {}

optionMaybe :: forall s a. Parser s a -> Parser s (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

try :: forall s a. Parser s a -> Parser s a
try p = Parser $ \s -> case runParser p s of
  ParseResult ({ consumed = true, result = Left err }) -> failureResult s false err
  res -> res

sepBy :: forall s a sep. Parser s a -> Parser s sep -> Parser s [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: forall s a sep. Parser s a -> Parser s sep -> Parser s [a]
sepBy1 p sep = do
  a <- p
  as <- many $ do
    sep
    p
  return (a : as)

sepEndBy :: forall s a sep. Parser s a -> Parser s sep -> Parser s [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []

sepEndBy1 :: forall s a sep. Parser s a -> Parser s sep -> Parser s [a]
sepEndBy1 p sep = do
  a <- p
  (do sep
      as <- sepEndBy p sep
      return (a : as)) <|> return [a]

endBy1 :: forall s a sep. Parser s a -> Parser s sep -> Parser s [a]
endBy1 p sep = many1 $ do 
  a <- p
  sep
  return a

endBy :: forall s a sep. Parser s a -> Parser s sep -> Parser s [a]
endBy p sep = many $ do
  a <- p
  sep
  return a

chainr :: forall s a. Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainr p f a = chainr1 p f <|> return a

chainl :: forall s a. Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl p f a = chainl1 p f <|> return a

chainl1 :: forall s a. Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainl1 p f = do
  a <- p
  chainl1' p f a

chainl1' :: forall s a. Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainl1' p f a = (do f' <- f
                     a' <- p
                     chainl1' p f (f' a a')) <|> return a

chainr1 :: forall s a. Parser s a -> Parser s (a -> a -> a) -> Parser s a
chainr1 p f = do
  a <- p
  chainr1' p f a

chainr1' :: forall s a. Parser s a -> Parser s (a -> a -> a) -> a -> Parser s a
chainr1' p f a = (do f' <- f
                     a' <- chainr1 p f
                     return $ f' a a') <|> return a

-- String Parsers

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
