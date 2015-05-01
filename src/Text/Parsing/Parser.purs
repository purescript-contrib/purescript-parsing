module Text.Parsing.Parser where

import Data.Either
import Data.Identity
import Data.Maybe
import Data.Monoid
import Data.Tuple

import Control.Alt
import Control.Alternative
import Control.Lazy
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Class
import Control.Monad.State.Trans
import Control.Monad.Error
import Control.Monad.Error.Class
import Control.Monad.Error.Trans
import Control.MonadPlus
import Control.Plus

import Text.Parsing.Parser.Pos

data ParseError = ParseError
  { message :: String
  , position :: Position
  }

instance errorParseError :: Error ParseError where
  noMsg = ParseError { message: "", position: initialPos }
  strMsg msg = ParseError { message: msg, position: initialPos }

instance showParseError :: Show ParseError where
  show (ParseError msg) = "ParseError { message: " ++ msg.message ++ ", position: " ++ show msg.position ++ " }"

-- | `PState` contains the remaining input and current position.
data PState s = PState
  { input :: s
  , position :: Position
  }

newtype ParserT s m a = ParserT (PState s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position })

unParserT :: forall m s a. ParserT s m a -> PState s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
unParserT (ParserT p) = p

runParserT :: forall m s a. (Monad m) => PState s -> ParserT s m a -> m (Either ParseError a)
runParserT s p = do
  o <- unParserT p s
  return o.result

type Parser s a = ParserT s Identity a

runParser :: forall s a. s -> Parser s a -> Either ParseError a
runParser s = runIdentity <<< runParserT (PState { input: s, position: initialPos })

instance functorParserT :: (Functor m) => Functor (ParserT s m) where
  (<$>) f p = ParserT $ \s -> f' <$> unParserT p s
    where
    f' o = { input: o.input, result: f <$> o.result, consumed: o.consumed, position: o.position }

instance applyParserT :: (Monad m) => Apply (ParserT s m) where
  (<*>) = ap

instance applicativeParserT :: (Monad m) => Applicative (ParserT s m) where
  pure a = ParserT $ \(PState { input: s, position: pos }) -> pure { input: s, result: Right a, consumed: false, position: pos }

instance altParserT :: (Monad m) => Alt (ParserT s m) where
  (<|>) p1 p2 = ParserT $ \s -> unParserT p1 s >>= \o ->
    case o.result of
      Left _ | not o.consumed -> unParserT p2 s
      _ -> return o

instance plusParserT :: (Monad m) => Plus (ParserT s m) where
  empty = fail "No alternative"

instance alternativeParserT :: (Monad m) => Alternative (ParserT s m)

instance bindParserT :: (Monad m) => Bind (ParserT s m) where
  (>>=) p f = ParserT $ \s -> unParserT p s >>= \o ->
    case o.result of
      Left err -> return { input: o.input, result: Left err, consumed: o.consumed, position: o.position }
      Right a -> updateConsumedFlag o.consumed <$> unParserT (f a) (PState { input: o.input, position: o.position })
    where
    updateConsumedFlag c o = { input: o.input, consumed: c || o.consumed, result: o.result, position: o.position }

instance monadParserT :: (Monad m) => Monad (ParserT s m)

instance monadPlusParserT :: (Monad m) => MonadPlus (ParserT s m)

instance monadTransParserT :: MonadTrans (ParserT s) where
  lift m = ParserT $ \(PState { input: s, position: pos }) -> (\a -> { input: s, consumed: false, result: Right a, position: pos }) <$> m

instance monadStateParserT :: (Monad m) => MonadState s (ParserT s m) where
  state f = ParserT $ \(PState { input: s, position: pos }) ->
    return $ case f s of
      Tuple a s' -> { input: s', consumed: false, result: Right a, position: pos }

instance lazy1ParserT :: Lazy1 (ParserT s m) where
  defer1 f = ParserT $ \s -> unParserT (f unit) s

consume :: forall s m. (Monad m) => ParserT s m Unit
consume = ParserT $ \(PState { input: s, position: pos }) -> return { consumed: true, input: s, result: Right unit, position: pos }

fail :: forall m s a. (Monad m) => String -> ParserT s m a
fail message = ParserT $ \(PState { input: s, position: pos }) -> return $ parseFailed s pos message

-- | Creates a failed parser state for the remaining input `s` and current position
-- | with an error message.
-- | Most of the time, `fail` should be used instead.
parseFailed :: forall s a. s -> Position -> String -> { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
parseFailed s pos message = { input: s, consumed: false, result: Left (ParseError { message: message, position: pos }), position: pos }

