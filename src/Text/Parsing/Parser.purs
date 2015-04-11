module Text.Parsing.Parser where

import Control.Alt (Alt)
import Control.Alternative (Alternative)
import Control.Lazy (Lazy)
import Control.Monad.Error (Error)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans (MonadTrans)
import Control.MonadPlus (MonadPlus)
import Control.Plus (Plus)
import Data.Either (Either(..))
import Data.Identity (Identity(..), runIdentity)
import Data.Tuple (Tuple(..))

newtype ParserT s m a = ParserT (s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean })

unParserT :: forall m s a. ParserT s m a -> s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean }
unParserT (ParserT p) = p

runParserT :: forall m s a. (Monad m) => s -> ParserT s m a -> m (Either ParseError a)
runParserT s p = _.result <$> unParserT p s

type Parser s a = ParserT s Identity a

runParser :: forall s a. s -> Parser s a -> Either ParseError a
runParser s = runIdentity <<< runParserT s

instance functorParserT :: (Functor m) => Functor (ParserT s m) where
  (<$>) f p = ParserT \s -> f' <$> unParserT p s
    where
    f' o = { input: o.input, result: f <$> o.result, consumed: o.consumed }

instance applyParserT :: (Monad m) => Apply (ParserT s m) where
  (<*>) = ap

instance applicativeParserT :: (Monad m) => Applicative (ParserT s m) where
  pure a = ParserT \s -> pure { input: s, result: Right a, consumed: false }

instance altParserT :: (Monad m) => Alt (ParserT s m) where
  (<|>) p1 p2 = ParserT \s -> unParserT p1 s >>= \o ->
    case o.result of
      Left _ | not o.consumed -> unParserT p2 s
      _ -> return o

instance plusParserT :: (Monad m) => Plus (ParserT s m) where
  empty = fail "No alternative"

instance alternativeParserT :: (Monad m) => Alternative (ParserT s m)

instance bindParserT :: (Monad m) => Bind (ParserT s m) where
  (>>=) p f = ParserT \s -> unParserT p s >>= \o ->
    case o.result of
      Left err -> return { input: o.input, result: Left err, consumed: o.consumed }
      Right a -> updateConsumedFlag o.consumed <$> unParserT (f a) o.input
    where
    updateConsumedFlag c o = { input: o.input, consumed: c || o.consumed, result: o.result }

instance monadParserT :: (Monad m) => Monad (ParserT s m)

instance monadPlusParserT :: (Monad m) => MonadPlus (ParserT s m)

instance monadTransParserT :: MonadTrans (ParserT s) where
  lift m = ParserT \s -> (\a -> { input: s, consumed: false, result: Right a }) <$> m

instance monadStateParserT :: (Monad m) => MonadState s (ParserT s m) where
  state f = ParserT \s ->
    return $ case f s of
      Tuple a s' -> { input: s', consumed: false, result: Right a }

instance lazyParserT :: Lazy (ParserT s m a) where
  defer f = ParserT \s -> unParserT (f unit) s

consume :: forall s m. (Monad m) => ParserT s m Unit
consume = ParserT \s -> return { consumed: true, input: s, result: Right unit }

newtype ParseError = ParseError String

instance errorParseError :: Error ParseError where
  noMsg = ParseError ""
  strMsg msg = ParseError msg

instance showParseError :: Show ParseError where
  show (ParseError msg) = "ParseError " ++ show msg

fail :: forall m s a. (Monad m) => String -> ParserT s m a
fail message = ParserT \s -> return { input: s, consumed: false, result: Left (ParseError message) }
