module Text.Parsing.Parser where

import Prelude

import Data.Either
import Data.Maybe
import Data.Monoid

import Control.Monad
import Control.Monad.Identity

import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Trans

data ParseError = ParseError
  { message :: String
  }

instance errorParseError :: Error ParseError where
  noMsg = ParseError { message: "" }
  strMsg msg = ParseError { message: msg }

data Consumed = Consumed Boolean

runConsumed :: Consumed -> Boolean
runConsumed (Consumed c) = c

data ParserT s m a = ParserT (StateT s (StateT Consumed (ErrorT ParseError m)) a)

unParserT :: forall m s a. ParserT s m a -> StateT s (StateT Consumed (ErrorT ParseError m)) a
unParserT (ParserT p) = p

runParserT :: forall m s a. (Monad m) => s -> ParserT s m a -> m (Either ParseError a)
runParserT s = runErrorT <<< flip evalStateT (Consumed false) <<< flip evalStateT s <<< unParserT

type Parser s a = ParserT s Identity a

runParser :: forall s a. s -> Parser s a -> Either ParseError a
runParser s = runIdentity <<< runParserT s

instance monadParserT :: (Monad m) => Monad (ParserT s m) where
  return a = ParserT (return a)
  (>>=) p f = ParserT (unParserT p >>= (unParserT <<< f))

instance alternativeParserT :: (Monad m) => Alternative (ParserT s m) where
  empty = ParserT empty
  (<|>) p1 p2 = ParserT (unParserT p1 <|> unParserT p2)

instance monadTransParserT :: MonadTrans (ParserT s) where
  lift m = ParserT (lift (lift (lift m)))

instance monadErrorParserT :: (Monad m) => MonadError ParseError (ParserT s m) where
  throwError e = ParserT (throwError e)
  catchError p f = ParserT (catchError (unParserT p) (unParserT <<< f))

instance monadStateParserT :: (Monad m) => MonadState s (ParserT s m) where
  state f = ParserT (state f)

instance monadStateConsumerParserT :: (Monad m) => MonadState Consumed (ParserT s m) where
  state f = ParserT (state f)

fail :: forall m s a. (Monad m) => String -> ParserT s m a
fail message = throwError (ParseError { message: message })

