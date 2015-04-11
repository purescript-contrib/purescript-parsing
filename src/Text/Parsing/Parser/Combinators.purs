module Text.Parsing.Parser.Combinators where

import Control.Alt ((<|>))
import Control.Alternative (some, many)
import Control.Apply ((*>), (<*))
import Control.Lazy (Lazy)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser

(<?>) :: forall m s a. (Monad m) => ParserT s m a -> String -> ParserT s m a
(<?>) p msg = p <|> fail ("Expected " ++ msg)

between :: forall m s a open close. (Monad m) => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
between open close p = open *> p <* close

option :: forall m s a. (Monad m) => a -> ParserT s m a -> ParserT s m a
option a p = p <|> return a

optional :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m Unit
optional p = (p *> return unit) <|> return unit

optionMaybe :: forall m s a. (Functor m, Monad m) => ParserT s m a -> ParserT s m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

try :: forall m s a. (Functor m) => ParserT s m a -> ParserT s m a
try p = ParserT \s -> try' s <$> unParserT p s
  where
  try' s o@{ result = Left _ } = { input: s, result: o.result, consumed: false }
  try' _ o = o

sepBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
sepBy1 p sep = (:) <$> p <*> (many $ sep *> p)

sepEndBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []

sepEndBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
sepEndBy1 p sep = do
  a <- p
  (sep *> ((a :) <$> sepEndBy p sep)) <|> return [a]

endBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
endBy1 p sep = some $ p <* sep

endBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
endBy p sep = many $ p <* sep

chainr :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr p f a = chainr1 p f <|> return a

chainl :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl p f a = chainl1 p f <|> return a

chainl1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainl1 p f = p >>= chainl1' p f

chainl1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl1' p f a = (do f' <- f
                     a' <- p
                     chainl1' p f (f' a a')) <|> return a

chainr1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainr1 p f = p >>= chainr1' p f

chainr1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr1' p f a = (do f' <- f
                     a' <- chainr1 p f
                     return $ f' a a') <|> return a

choice :: forall m s a. (Monad m) => [ParserT s m a] -> ParserT s m a
choice []   = fail "Nothing to parse"
choice [x]  = x
choice (x:xs) = x <|> choice xs

skipMany :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
skipMany p = skipMany1 p <|> return unit

skipMany1 :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
skipMany1 p = p *> skipMany p *> return unit

lookAhead :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m a
lookAhead (ParserT p) = ParserT \s -> _ { input = s, consumed = false } <$> p s

notFollowedBy :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> return unit

manyTill :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m [a]
manyTill p end = scan
  where
  scan = (end *> return [])
     <|> (do x <- p
             xs <- scan
             return (x:xs))

many1Till :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m [a]
many1Till p end = (:) <$> p <*> manyTill p end
