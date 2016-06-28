module Text.Parsing.Parser where

import Prelude

import Control.Lazy (class Lazy)
import Control.Monad.State.Class (class MonadState)
import Control.Monad.Trans (class MonadTrans)
import Control.MonadPlus (class MonadPlus, class MonadZero, class Alternative)
import Control.Plus (class Plus, class Alt)
import Data.Either (Either(..))
import Data.Identity (Identity, runIdentity)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser.Pos (Position, initialPos)

-- | A parsing error, consisting of a message and position information.
data ParseError = ParseError String Position

derive instance eqParseError :: Eq ParseError

-- | The result of a single parse
data Result s a = Result  s                     -- the new input
                          (Either ParseError a) -- the result
                          Boolean               -- consumed?
                          Position              -- the new position

instance showParseError :: Show ParseError where
  show (ParseError msg pos) = "ParseError " <> show msg <> " " <> show pos

-- | `PState` contains the remaining input and current position.
data PState s = PState s Position

-- | The Parser monad transformer.
-- |
-- | The first type argument is the stream type. Typically, this is either `String`, or some sort of token stream.
newtype ParserT s m a = ParserT (PState s -> m (Result s a))

-- | Apply a parser by providing an initial state.
unParserT :: forall m s a. ParserT s m a -> PState s -> m (Result s a)
unParserT (ParserT p) = p

-- | Apply a parser, keeping only the parsed result.
runParserT :: forall m s a. Monad m => PState s -> ParserT s m a -> m (Either ParseError a)
runParserT s p = do
  (Result _ result _ _) <- unParserT p s
  pure result

-- | The `Parser` monad is a synonym for the parser monad transformer applied to the `Identity` monad.
type Parser s a = ParserT s Identity a

-- | Apply a parser, keeping only the parsed result.
runParser :: forall s a. s -> Parser s a -> Either ParseError a
runParser s = runIdentity <<< runParserT (PState s initialPos)

instance functorParserT :: (Functor m) => Functor (ParserT s m) where
  map f p = ParserT $ \s -> f' <$> unParserT p s
    where
    f' (Result input result consumed pos) = Result input (f <$> result) consumed pos

instance applyParserT :: Monad m => Apply (ParserT s m) where
  apply = ap

instance applicativeParserT :: Monad m => Applicative (ParserT s m) where
  pure a = ParserT $ \(PState s pos) -> do
    pure (Result s (Right a) false pos)

instance altParserT :: Monad m => Alt (ParserT s m) where
  alt p1 p2 = ParserT $ \s -> do
    (o@(Result input result consumed pos)) <- unParserT p1 s
    case result of
      Left _ | not consumed -> unParserT p2 s
      otherwise             -> pure o

instance plusParserT :: Monad m => Plus (ParserT s m) where
  empty = fail "No alternative"

instance alternativeParserT :: Monad m => Alternative (ParserT s m)

instance bindParserT :: Monad m => Bind (ParserT s m) where
  bind p f = ParserT $ \s -> do
    (Result input result consumed pos) <- unParserT p s
    case result of
      Left err  -> pure (Result input (Left err) consumed pos)
      Right a -> do
        (Result input' result' consumed' pos') <- unParserT (f a) (PState input pos)
        pure (Result input' result' (consumed || consumed') pos')

instance monadParserT :: Monad m => Monad (ParserT s m)

instance monadZeroParserT :: Monad m => MonadZero (ParserT s m)

instance monadPlusParserT :: Monad m => MonadPlus (ParserT s m)

instance monadTransParserT :: MonadTrans (ParserT s) where
  lift m = ParserT $ \(PState s pos) -> (\a -> Result s (Right a) false pos) <$> m

instance monadStateParserT :: Monad m => MonadState s (ParserT s m) where
  state f = ParserT $ \(PState s pos) ->
    pure $ case f s of
      Tuple a s' -> Result s' (Right a) false pos

instance lazyParserT :: Lazy (ParserT s m a) where
  defer f = ParserT $ \s -> unParserT (f unit) s

-- | Set the consumed flag.
consume :: forall s m. Monad m => ParserT s m Unit
consume = ParserT $ \(PState s pos) -> pure (Result s (Right unit) true pos)

-- | Fail with a message.
fail :: forall m s a. Monad m => String -> ParserT s m a
fail message = ParserT $ \(PState s pos) -> pure $ parseFailed s pos message

-- | Creates a failed parser state for the remaining input `s` and current position
-- | with an error message.
-- |
-- | Most of the time, `fail` should be used instead.
parseFailed :: forall s a. s -> Position -> String -> Result s a
parseFailed s pos message = Result s (Left (ParseError message pos)) false pos
