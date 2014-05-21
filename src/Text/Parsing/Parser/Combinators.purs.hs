module Text.Parsing.Parser.Combinators where

import Prelude

import Data.Maybe
import Data.Array
import Data.Tuple
import Data.Either

import Control.Monad

import Control.Monad.Error.Trans
import Control.Monad.Error.Class
import Control.Monad.State.Trans
import Control.Monad.State.Class

import Text.Parsing.Parser

fix :: forall m s a. (ParserT m s a -> ParserT m s a) -> ParserT m s a
fix f = ParserT (StateT (\s -> runStateT (unParserT (f (fix f))) s))

fix2 :: forall m s a b. (Tuple (ParserT m s a) (ParserT m s b) -> Tuple (ParserT m s a) (ParserT m s b)) -> Tuple (ParserT m s a) (ParserT m s b)
fix2 f = Tuple
           (ParserT (StateT (\s -> runStateT (unParserT (fst (f (fix2 f)))) s)))
           (ParserT (StateT (\s -> runStateT (unParserT (snd (f (fix2 f)))) s)))

many :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m [a]
many p = many1 p <|> return []

many1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m [a]
many1 p = do a <- p
             as <- many p
             return (a : as)

(<?>) :: forall m s a. (Monad m) => ParserT s m a -> String -> ParserT s m a
(<?>) p msg = p <|> fail ("Expected " ++ msg)

between :: forall m s a open close. (Monad m) => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
between open close p = do
  open
  a <- p
  close 
  return a

option :: forall m s a. (Monad m) => a -> ParserT s m a -> ParserT s m a
option a p = p <|> return a

optional :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m {}
optional p = (do p
                 return {}) <|> return {}

optionMaybe :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

try :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m a
try p = catchError p $ \e -> do
  Consumed consumed <- get
  when consumed $ put (Consumed false)
  throwError (e :: ParseError)

sepBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
sepBy1 p sep = do
  a <- p
  as <- many $ do
    sep
    p
  return (a : as)

sepEndBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []

sepEndBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
sepEndBy1 p sep = do
  a <- p
  (do sep
      as <- sepEndBy p sep
      return (a : as)) <|> return [a]

endBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
endBy1 p sep = many1 $ do 
  a <- p
  sep
  return a

endBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
endBy p sep = many $ do
  a <- p
  sep
  return a

chainr :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr p f a = chainr1 p f <|> return a

chainl :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl p f a = chainl1 p f <|> return a

chainl1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainl1 p f = do
  a <- p
  chainl1' p f a

chainl1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl1' p f a = (do f' <- f
                     a' <- p
                     chainl1' p f (f' a a')) <|> return a

chainr1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainr1 p f = do
  a <- p
  chainr1' p f a

chainr1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr1' p f a = (do f' <- f
                     a' <- chainr1 p f
                     return $ f' a a') <|> return a

choice :: forall m s a. (Monad m) => [ParserT s m a] -> ParserT s m a
choice []   = fail "Nothing to parse"
choice [x]  = x
choice (x:xs) = x <|> choice xs

