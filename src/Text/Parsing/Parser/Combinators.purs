-- | Combinators for creating parsers.
-- |
-- | ### Notes
-- |
-- | A few of the known combinators from Parsec are missing in this module. That
-- | is because they have already been defined in other libraries.
-- |
-- | ```purescript
-- | Text.Parsec.many  = Data.(Array|List).many
-- | Text.Parsec.(<|>) = Control.Alt.alt (<|>)
-- | ```
-- |
-- | Because Strings are not Char Arrays in PureScript `many` and `some` on Char Parsers need to
-- | be used in conjunction with `Data.String.CodeUnits.fromCharArray` to achieve "Parsec-like" results.
-- |
-- | ```purescript
-- | Text.Parsec.many  (char 'x') <=> fromCharArray <$> Data.Array.many (char 'x')
-- | ```
-- |
-- | Note that `Data.(Array|List).(many|some)` are not stack safe. If you need to parse
-- | large numbers of items then consider using `Data.List.(manyRec|someRec)` instead.

module Text.Parsing.Parser.Combinators where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.State (StateT(..), runStateT)
import Control.Plus (empty, (<|>))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), many, (:))
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (ParseError(..), ParseState(..), ParserT(..), fail)

-- | Provide an error message in the case of failure.
withErrorMessage :: forall m s a. Monad m => ParserT s m a -> String -> ParserT s m a
withErrorMessage p msg = p <|> fail ("Expected " <> msg)

infixl 3 withErrorMessage as <?>

-- | Provide an error message in the case of failure, but lazily. This is handy
-- | in cases where constructing the error message is expensive, so it's
-- | preferable to defer it until an error actually happens.
-- |
-- |```purs
-- |parseBang :: Parser Char
-- |parseBang = char '!' <??> \_ -> "Expected a bang"
-- |```
withLazyErrorMessage :: forall m s a. Monad m => ParserT s m a -> (Unit -> String) -> ParserT s m a
withLazyErrorMessage p msg = p <|> defer \_ -> fail ("Expected " <> msg unit)

infixl 3 withLazyErrorMessage as <~?>

-- | Flipped `(<?>)`.
asErrorMessage :: forall m s a. Monad m => String -> ParserT s m a -> ParserT s m a
asErrorMessage = flip (<?>)

infixl 3 asErrorMessage as <??>

-- | Wrap a parser with opening and closing markers.
-- |
-- | For example:
-- |
-- | ```purescript
-- | parens = between (string "(") (string ")")
-- | ```
between :: forall m s a open close. Monad m => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
between open close p = open *> p <* close

-- | Provide a default result in the case where a parser fails without consuming input.
option :: forall m s a. Monad m => a -> ParserT s m a -> ParserT s m a
option a p = p <|> pure a

-- | Optionally parse something, failing quietly.
optional :: forall m s a. Monad m => ParserT s m a -> ParserT s m Unit
optional p = void p <|> pure unit

-- | pure `Nothing` in the case where a parser fails without consuming input.
optionMaybe :: forall m s a. Monad m => ParserT s m a -> ParserT s m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | In case of failure, reset the stream to the unconsumed state.
try :: forall m s a. Monad m => ParserT s m a -> ParserT s m a
try p = (ParserT <<< ExceptT <<< StateT) \(s@(ParseState _ _ consumed)) -> do
  Tuple e s'@(ParseState input position _) <- runStateT (runExceptT (unwrap p)) s
  case e of
    Left _ -> pure (Tuple e (ParseState input position consumed))
    _ -> pure (Tuple e s')

-- | Like `try`, but will reannotate the error location to the `try` point.
tryRethrow :: forall m s a. Monad m => ParserT s m a -> ParserT s m a
tryRethrow p = (ParserT <<< ExceptT <<< StateT) \(s@(ParseState _ position consumed)) -> do
  Tuple e s'@(ParseState input' position' _) <- runStateT (runExceptT (unwrap p)) s
  case e of
    Left (ParseError err _) -> pure (Tuple (Left (ParseError err position)) (ParseState input' position' consumed))
    _ -> pure (Tuple e s')

-- | Parse a phrase, without modifying the consumed state or stream position.
lookAhead :: forall s a m. Monad m => ParserT s m a -> ParserT s m a
lookAhead p = (ParserT <<< ExceptT <<< StateT) \s -> do
  Tuple e _ <- runStateT (runExceptT (unwrap p)) s
  pure (Tuple e s)

-- | Match one or more times.
many1 :: forall m s a. Monad m => ParserT s m a -> ParserT s m (NonEmptyList a)
many1 p = NEL.cons' <$> p <*> many p

-- | Parse phrases delimited by a separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | digit `sepBy` string ","
-- | ```
sepBy :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepBy p sep = map NEL.toList (sepBy1 p sep) <|> pure Nil

-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepBy1 p sep = do
  a <- p
  as <- many $ sep *> p
  pure (NEL.cons' a as)

-- | Parse phrases delimited and optionally terminated by a separator.
sepEndBy :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndBy p sep = map NEL.toList (sepEndBy1 p sep) <|> pure Nil

-- | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
sepEndBy1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepEndBy1 p sep = do
  a <- p
  ( do
      _ <- sep
      as <- sepEndBy p sep
      pure (NEL.cons' a as)
  ) <|> pure (NEL.singleton a)

-- | Parse phrases delimited and terminated by a separator, requiring at least one match.
endBy1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
endBy1 p sep = many1 $ p <* sep

-- | Parse phrases delimited and terminated by a separator.
endBy :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
endBy p sep = many $ p <* sep

-- | Parse phrases delimited by a right-associative operator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | chainr digit (string "+" *> add) 0
-- | ```
chainr :: forall m s a. Monad m => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr p f a = chainr1 p f <|> pure a

-- | Parse phrases delimited by a left-associative operator.
chainl :: forall m s a. Monad m => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl p f a = chainl1 p f <|> pure a

-- | Parse phrases delimited by a left-associative operator, requiring at least one match.
chainl1 :: forall m s a. Monad m => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainl1 p f = do
  a <- p
  chainl1' p f a

chainl1' :: forall m s a. Monad m => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl1' p f a =
  ( do
      f' <- f
      a' <- p
      chainl1' p f (f' a a')
  ) <|> pure a

-- | Parse phrases delimited by a right-associative operator, requiring at least one match.
chainr1 :: forall m s a. Monad m => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainr1 p f = do
  a <- p
  chainr1' p f a

chainr1' :: forall m s a. Monad m => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr1' p f a =
  ( do
      f' <- f
      a' <- chainr1 p f
      pure $ f' a a'
  ) <|> pure a

-- | Parse one of a set of alternatives.
choice :: forall f m s a. Foldable f => Monad m => f (ParserT s m a) -> ParserT s m a
choice = foldl (<|>) empty

-- | Skip many instances of a phrase.
skipMany :: forall s a m. Monad m => ParserT s m a -> ParserT s m Unit
skipMany p = skipMany1 p <|> pure unit

-- | Skip at least one instance of a phrase.
skipMany1 :: forall s a m. Monad m => ParserT s m a -> ParserT s m Unit
skipMany1 p = do
  _ <- p
  _ <- skipMany p
  pure unit

-- | Fail if the specified parser matches.
notFollowedBy :: forall s a m. Monad m => ParserT s m a -> ParserT s m Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> pure unit

-- | Parse several phrases until the specified terminator matches.
manyTill :: forall s a m e. Monad m => ParserT s m a -> ParserT s m e -> ParserT s m (List a)
manyTill p end = scan
  where
  scan = (end $> Nil) <|> do
    x <- p
    xs <- scan
    pure (x : xs)

-- | Parse several phrases until the specified terminator matches, requiring at least one match.
many1Till :: forall s a m e. Monad m => ParserT s m a -> ParserT s m e -> ParserT s m (NonEmptyList a)
many1Till p end = do
  x <- p
  xs <- manyTill p end
  pure (NEL.cons' x xs)
