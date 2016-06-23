-- | Combinators for creating parsers.
-- |
-- | ### Notes:
-- | A few of the known combinators from Parsec are missing in this module. That
-- | is because they have already been defined in other libraries.
-- |
-- | ```purescript
-- | Text.Parsec.many  = Data.(Array|List).many
-- | Text.Parsec.many1 = Data.(Array|List).some
-- | Text.Parsec.(<|>) = Control.Alt.alt (<|>)
-- | ```
-- |
-- | Because Strings are not Char Arrays in PureScript `many` and `some` on Char Parsers need to
-- | be used in conjunction with `Data.String.fromCharArray` to achieve "Parsec-like" results.
-- |
-- | ```purescript
-- | Text.Parsec.many  (char 'x') <=> fromCharArray <$> Data.Array.many (char 'x')
-- | ```
-- |
-- | ===

module Text.Parsing.Parser.Combinators where

import Prelude (class Functor, class Monad, Unit, ($), (*>), (<>), (<$>), bind, flip, pure, unit)

import Control.Plus (empty, (<|>))
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldl)
import Data.List (List(..), (:), many, some, singleton)
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser (PState(..), ParserT(..), fail, unParserT)

-- | Provide an error message in the case of failure.
withErrorMessage :: forall m s a. Monad m => ParserT s m a -> String -> ParserT s m a
withErrorMessage p msg = p <|> fail ("Expected " <> msg)

infix 3 withErrorMessage as <?>

-- | Flipped `(<?>)`.
asErrorMessage :: forall m s a. Monad m => String -> ParserT s m a -> ParserT s m a
asErrorMessage = flip (<?>)

infix 3 asErrorMessage as <??>

-- | Wrap a parser with opening and closing markers.
-- |
-- | For example:
-- |
-- | ```purescript
-- | parens = between (string "(") (string ")")
-- | ```
between :: forall m s a open close. Monad m => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
between open close p = do
  open
  a <- p
  close
  pure a

-- | Provide a default result in the case where a parser fails without consuming input.
option :: forall m s a. Monad m => a -> ParserT s m a -> ParserT s m a
option a p = p <|> pure a

-- | Optionally parse something, failing quietly.
optional :: forall m s a. Monad m => ParserT s m a -> ParserT s m Unit
optional p = (do p
                 pure unit) <|> pure unit

-- | pure `Nothing` in the case where a parser fails without consuming input.
optionMaybe :: forall m s a. Monad m => ParserT s m a -> ParserT s m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | In case of failure, reset the stream to the unconsumed state.
try :: forall m s a. (Functor m) => ParserT s m a -> ParserT s m a
try p = ParserT $ \(PState s pos) -> try' s pos <$> unParserT p (PState s pos)
  where
  try' s pos o@{ result: Left _ } = { input: s, result: o.result, consumed: false, position: pos }
  try' _ _   o = o

-- | Parse phrases delimited by a separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | digit `sepBy` string ","
-- | ```
sepBy :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepBy p sep = sepBy1 p sep <|> pure Nil

-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepBy1 p sep = do
  a <- p
  as <- many $ do
    sep
    p
  pure (a : as)

-- | Parse phrases delimited and optionally terminated by a separator.
sepEndBy :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndBy p sep = sepEndBy1 p sep <|> pure Nil

-- | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
sepEndBy1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndBy1 p sep = do
  a <- p
  (do sep
      as <- sepEndBy p sep
      pure (a : as)) <|> pure (singleton a)

-- | Parse phrases delimited and terminated by a separator, requiring at least one match.
endBy1 :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
endBy1 p sep = some $ do
  a <- p
  sep
  pure a

-- | Parse phrases delimited and terminated by a separator.
endBy :: forall m s a sep. Monad m => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
endBy p sep = many $ do
  a <- p
  sep
  pure a

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
chainl1' p f a = (do f' <- f
                     a' <- p
                     chainl1' p f (f' a a')) <|> pure a

-- | Parse phrases delimited by a right-associative operator, requiring at least one match.
chainr1 :: forall m s a. Monad m => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainr1 p f = do
  a <- p
  chainr1' p f a

chainr1' :: forall m s a. Monad m => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr1' p f a = (do f' <- f
                     a' <- chainr1 p f
                     pure $ f' a a') <|> pure a

-- | Parse one of a set of alternatives.
choice :: forall f m s a. (Foldable f, Monad m) => f (ParserT s m a) -> ParserT s m a
choice = foldl (<|>) empty

-- | Skip many instances of a phrase.
skipMany :: forall s a m. Monad m => ParserT s m a -> ParserT s m Unit
skipMany p = skipMany1 p <|> pure unit

-- | Skip at least one instance of a phrase.
skipMany1 :: forall s a m. Monad m => ParserT s m a -> ParserT s m Unit
skipMany1 p = do
  x <- p
  xs <- skipMany p
  pure unit

-- | Parse a phrase, without modifying the consumed state or stream position.
lookAhead :: forall s a m. Monad m => ParserT s m a -> ParserT s m a
lookAhead (ParserT p) = ParserT \(PState s pos) -> do
  state <- p (PState s pos)
  pure state{input = s, consumed = false, position = pos}

-- | Fail if the specified parser matches.
notFollowedBy :: forall s a m. Monad m => ParserT s m a -> ParserT s m Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> pure unit

-- | Parse several phrases until the specified terminator matches.
manyTill :: forall s a m e. Monad m => ParserT s m a -> ParserT s m e -> ParserT s m (List a)
manyTill p end = scan
  where
    scan = (do
              end
              pure Nil)
       <|> (do
              x <- p
              xs <- scan
              pure (x:xs))

-- | Parse several phrases until the specified terminator matches, requiring at least one match.
many1Till :: forall s a m e. Monad m => ParserT s m a -> ParserT s m e -> ParserT s m (List a)
many1Till p end = do
  x <- p
  xs <- manyTill p end
  pure (x:xs)
