-- | ## Combinators in other packages
-- |
-- | Many variations of well-known monadic and applicative combinators used for parsing are
-- | defined in other PureScript packages. It’s awkward to re-export
-- | them because their names overlap so much, so we list them here.
-- |
-- | ### Data.Array
-- | * [Data.Array.many](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array#v:many)
-- | * [Data.Array.some](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array#v:some)
-- |
-- | ### Data.Array.NonEmpty
-- | * [Data.Array.NonEmpty.some](https://pursuit.purescript.org/packages/purescript-arrays/docs/Data.Array.NonEmpty#v:some)
-- |
-- | ### Data.List
-- | * [Data.List.many](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List#v:many)
-- | * [Data.List.some](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List#v:some)
-- |
-- | ### Data.List.NonEmpty
-- | * See the __many1__ combinator below.
-- |
-- | ### Data.List.Lazy
-- | * [Data.List.Lazy.many](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.Lazy#v:many)
-- | * [Data.List.Lazy.some](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.Lazy#v:some)
-- | * [Data.List.Lazy.replicateM](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List.Lazy#v:replicateM)
-- |
-- | ## Combinators in this package
-- |
-- | A parser combinator is a function which takes some
-- | parsers as arguments and returns a new parser.
-- |
-- | The __many__ combinator applied to parser `p :: Parser s a` will return
-- | a parser `many p :: Parser s (Array a)` which will repeat the
-- | parser `p` as many times as possible. If `p` never consumes input when it
-- | fails then `many p` will always succeed
-- | but may return an empty array.
-- |
-- | The __replicateA n__ combinator applied to parser `p :: Parser s a` will
-- | return a parser `replicateA n p :: Parser s (Array a)` which will
-- | repeat parser `p` exactly `n` times. `replicateA n p` will only succeed
-- | if it can match parser `p` exactly `n` consecutive times.
-- |
-- | ## Stack-safety
-- |
-- | The `ParserT` monad is always stack-safe, so `MonadRec` combinators are
-- | not necessary.
module Parsing.Combinators
  ( (<?>)
  , (<??>)
  , (<~?>)
  , asErrorMessage
  , between
  , chainl
  , chainl1
  , chainr
  , chainr1
  , choice
  , endBy
  , endBy1
  , lookAhead
  , many1
  , many1Till
  , many1Till_
  , manyTill
  , manyTill_
  , module Control.Plus
  , module Data.Unfoldable
  , module Data.Unfoldable1
  , notFollowedBy
  , option
  , optionMaybe
  , optional
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , skipMany
  , skipMany1
  , try
  , tryRethrow
  , withErrorMessage
  , withLazyErrorMessage
  ) where

import Prelude

import Control.Lazy (defer)
import Control.Plus (empty, (<|>), alt)
import Data.Foldable (class Foldable, foldr)
import Data.Function.Uncurried (mkFn2, mkFn5, runFn2, runFn5)
import Data.List (List(..), many, (:))
import Data.List.NonEmpty (NonEmptyList, cons')
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (replicateA)
import Data.Unfoldable1 (replicate1A)
import Parsing (ParseError(..), ParseState(..), ParserT(..), fail)

-- | Provide an error message in the case of failure.
withErrorMessage :: forall m s a. ParserT s m a -> String -> ParserT s m a
withErrorMessage p msg = p <|> fail ("Expected " <> msg)

infixl 4 withErrorMessage as <?>

-- | Provide an error message in the case of failure, but lazily. This is handy
-- | in cases where constructing the error message is expensive, so it's
-- | preferable to defer it until an error actually happens.
-- |
-- |```purs
-- |parseBang :: Parser Char
-- |parseBang = char '!' <~?> \_ -> "Expected a bang"
-- |```
withLazyErrorMessage :: forall m s a. ParserT s m a -> (Unit -> String) -> ParserT s m a
withLazyErrorMessage p msg = p <|> defer \_ -> fail ("Expected " <> msg unit)

infixl 4 withLazyErrorMessage as <~?>

-- | Flipped `(<?>)`.
asErrorMessage :: forall m s a. String -> ParserT s m a -> ParserT s m a
asErrorMessage = flip (<?>)

infixr 3 asErrorMessage as <??>

-- | Wrap a parser with opening and closing markers.
-- |
-- | For example:
-- |
-- | ```purescript
-- | parens = between (string "(") (string ")")
-- | ```
between :: forall m s a open close. ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
between open close p = open *> p <* close

-- | Provide a default result in the case where a parser fails without consuming input.
option :: forall m s a. a -> ParserT s m a -> ParserT s m a
option a p = p <|> pure a

-- | Optionally parse something, failing quietly.
-- |
-- | To optionally parse `p` and never fail: `optional (try p)`.
optional :: forall m s a. ParserT s m a -> ParserT s m Unit
optional p = void p <|> pure unit

-- TODO Is this optional parser correct? Isn't this parser supposed to succeed
-- even if p fails? Otherwise what's the point? I think we need try (void p).

-- | pure `Nothing` in the case where a parser fails without consuming input.
optionMaybe :: forall m s a. ParserT s m a -> ParserT s m (Maybe a)
optionMaybe p = option Nothing (Just <$> p)

-- | If the parser fails then backtrack the input stream to the unconsumed state.
-- |
-- | One use for this combinator is to ensure that the right parser of an
-- | alternative will always be tried when the left parser fails.
-- | ```
-- | >>> runParser "ac" ((char 'a' *> char 'b') <|> (char 'a' *> char 'c'))
-- | Left (ParseError "Expected 'b'" (Position { line: 1, column: 2 }))
-- | ```
-- |
-- | ```
-- | >>> runParser "ac" (try (char 'a' *> char 'b') <|> (char 'a' *> char 'c'))
-- | Right 'c'
-- | ```
try :: forall m s a. ParserT s m a -> ParserT s m a
try (ParserT k1) = ParserT
  ( mkFn5 \state1@(ParseState _ _ consumed) more lift throw done ->
      runFn5 k1 state1 more lift
        ( mkFn2 \(ParseState input position _) err ->
            runFn2 throw (ParseState input position consumed) err
        )
        done
  )

-- | If the parser fails then backtrack the input stream to the unconsumed state.
-- |
-- | Like `try`, but will relocate the error to the `try` point.
-- |
-- | ```
-- | >>> runParser "ac" (try (char 'a' *> char 'b'))
-- | Left (ParseError "Expected 'b'" (Position { line: 1, column: 2 }))
-- | ```
-- |
-- | ```
-- | >>> runParser "ac" (tryRethrow (char 'a' *> char 'b'))
-- | Left (ParseError "Expected 'b'" (Position { line: 1, column: 1 }))
-- | ```
tryRethrow :: forall m s a. ParserT s m a -> ParserT s m a
tryRethrow (ParserT k1) = ParserT
  ( mkFn5 \state1@(ParseState _ position consumed) more lift throw done ->
      runFn5 k1 state1 more lift
        ( mkFn2 \(ParseState input' position' _) (ParseError err _) ->
            runFn2 throw (ParseState input' position' consumed) (ParseError err position)
        )
        done
  )

-- | Parse a phrase, without modifying the consumed state or stream position.
lookAhead :: forall s a m. ParserT s m a -> ParserT s m a
lookAhead (ParserT k1) = ParserT
  ( mkFn5 \state1 more lift throw done ->
      runFn5 k1 state1 more lift
        (mkFn2 \_ err -> runFn2 throw state1 err)
        (mkFn2 \_ res -> runFn2 done state1 res)
  )

-- | Match one or more times.
many1 :: forall m s a. ParserT s m a -> ParserT s m (NonEmptyList a)
many1 p = NEL.cons' <$> p <*> many p

-- | Parse phrases delimited by a separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | digit `sepBy` string ","
-- | ```
sepBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepBy p sep = map NEL.toList (sepBy1 p sep) <|> pure Nil

-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepBy1 p sep = do
  a <- p
  as <- many $ sep *> p
  pure (NEL.cons' a as)

-- | Parse phrases delimited and optionally terminated by a separator.
sepEndBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndBy p sep = map NEL.toList (sepEndBy1 p sep) <|> pure Nil

-- | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
sepEndBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepEndBy1 p sep = do
  a <- p
  ( do
      _ <- sep
      as <- sepEndBy p sep
      pure (NEL.cons' a as)
  ) <|> pure (NEL.singleton a)

-- | Parse phrases delimited and terminated by a separator, requiring at least one match.
endBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
endBy1 p sep = many1 $ p <* sep

-- | Parse phrases delimited and terminated by a separator.
endBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
endBy p sep = many $ p <* sep

-- | Parse phrases delimited by a right-associative operator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | chainr digit (string "+" $> add) 0
-- | ```
chainr :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr p f a = chainr1 p f <|> pure a

-- | Parse phrases delimited by a left-associative operator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | chainr digit (string "+" $> add) 0
-- | ```
chainl :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl p f a = chainl1 p f <|> pure a

-- | Parse phrases delimited by a left-associative operator, requiring at least one match.
chainl1 :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainl1 p f = do
  a <- p
  chainl1' a
  where
  chainl1' a =
    ( do
        f' <- f
        a' <- p
        chainl1' (f' a a')
    ) <|> pure a

-- | Parse phrases delimited by a right-associative operator, requiring at least one match.
chainr1 :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainr1 p f = do
  a <- p
  chainr1' a
  where
  chainr1' a =
    ( do
        f' <- f
        a' <- chainr1 p f
        pure $ f' a a'
    ) <|> pure a

-- | Parse one of a set of alternatives.
choice :: forall f m s a. Foldable f => f (ParserT s m a) -> ParserT s m a
choice = fromMaybe empty <<< foldr go Nothing
  where
  go p1 = case _ of
    Nothing -> Just p1
    Just p2 -> Just (p1 <|> p2)

-- | Skip many instances of a phrase.
skipMany :: forall s a m. ParserT s m a -> ParserT s m Unit
skipMany p = skipMany1 p <|> pure unit

-- | Skip at least one instance of a phrase.
skipMany1 :: forall s a m. ParserT s m a -> ParserT s m Unit
skipMany1 p = do
  _ <- p
  _ <- skipMany p
  pure unit

-- | Fail if the parser succeeds.
-- |
-- | Will never consume input.
notFollowedBy :: forall s a m. ParserT s m a -> ParserT s m Unit
notFollowedBy p = try $ (try p *> fail "Negated parser succeeded") <|> pure unit

-- | Parse many phrases until the terminator phrase matches.
manyTill :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (List a)
manyTill p end = scan
  where
  scan = (end $> Nil) <|> do
    x <- p
    xs <- scan
    pure (x : xs)

-- | Parse at least one phrase until the terminator phrase matches.
many1Till :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (NonEmptyList a)
many1Till p end = do
  x <- p
  xs <- manyTill p end
  pure (NEL.cons' x xs)

-- | Parse many phrases until the terminator phrase matches.
-- | Returns the list of phrases and the terminator phrase.
-- |
-- | ## Non-greedy repetition
-- |
-- | Use the __manyTill_ __ combinator
-- | to do non-greedy repetition of a pattern `p`, like we would in Regex
-- | by writing `p*?`.
-- | To repeat pattern `p` non-greedily, write
-- | `manyTill_ p q` where `q` is the entire rest of the parser.
-- |
-- | For example, this parse fails because `many` repeats the pattern `letter`
-- | greedily.
-- |
-- | ```
-- | runParser "aab" do
-- |   a <- many letter
-- |   b <- char 'b'
-- |   pure (Tuple a b)
-- | ```
-- | ```
-- | (ParseError "Expected 'b'" (Position { line: 1, column: 4 }))
-- | ```
-- |
-- | To repeat pattern `letter` non-greedily, use `manyTill_`.
-- |
-- | ```
-- | runParser "aab" do
-- |   Tuple a b <- manyTill_ letter do
-- |     char 'b'
-- |   pure (Tuple a b)
-- | ```
-- | ```
-- | (Tuple ('a' : 'a' : Nil) 'b')
-- | ```
manyTill_ :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (List a) e)
manyTill_ p end = scan
  where
  scan =
    do
      t <- end
      pure $ Tuple Nil t
      <|>
        do
          x <- p
          Tuple xs t <- scan
          pure $ Tuple (x : xs) t

-- | Parse many phrases until the terminator phrase matches, requiring at least one match.
-- | Returns the list of phrases and the terminator phrase.
many1Till_ :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (NonEmptyList a) e)
many1Till_ p end = do
  x <- p
  Tuple xs t <- manyTill_ p end
  pure $ Tuple (cons' x xs) t
