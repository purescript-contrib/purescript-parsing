module Text.Parsing.Parser.Combinators where

import Prelude
import Data.Maybe
import Data.Array
import Data.Either
import Text.Parsing.Parser

many :: forall s a. Parser s a -> Parser s [a]
many p = many1 p <|> return []

many1 :: forall s a. Parser s a -> Parser s [a]
many1 p = do a <- p
             as <- many p
             return (a : as)

(<?>) :: forall s a. Parser s a -> String -> Parser s a
(<?>) p msg = p <|> fail msg

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

choice :: forall s a. [Parser s a] -> Parser s a
choice []   = fail "Nothing to parse"
choice [x]  = x
choice (x:xs) = x <|> choice xs

