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
-- | * [Data.List.someRec](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List#v:someRec)
-- | * [Data.List.manyRec](https://pursuit.purescript.org/packages/purescript-lists/docs/Data.List#v:manyRec)
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
module Text.Parsing.Parser.Combinators
  ( (<?>)
  , (<??>)
  , (<~?>)
  , asErrorMessage
  , between
  , chainl
  , chainl1
  , chainl1Rec
  , chainlRec
  , chainr
  , chainr1
  , chainr1Rec
  , chainrRec
  , choice
  , endBy
  , endBy1
  , endBy1Rec
  , endByRec
  , lookAhead
  , many1
  , many1Rec
  , many1Till
  , many1TillRec
  , many1TillRec_
  , many1Till_
  , manyTill
  , manyTillRec
  , manyTillRec_
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
  , sepBy1Rec
  , sepByRec
  , sepEndBy
  , sepEndBy1
  , sepEndBy1Rec
  , sepEndByRec
  , skipMany
  , skipMany1
  , skipMany1Rec
  , skipManyRec
  , try
  , tryRethrow
  , withErrorMessage
  , withLazyErrorMessage
  ) where

import Prelude

import Control.Lazy (defer)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Plus (empty, (<|>), alt)
import Data.Foldable (class Foldable, foldl, foldr)
import Data.Function.Uncurried (mkFn2, mkFn5, runFn2, runFn5)
import Data.List (List(..), many, manyRec, reverse, (:))
import Data.List.NonEmpty (NonEmptyList, cons')
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Unfoldable (replicateA)
import Data.Unfoldable1 (replicate1A)
import Text.Parsing.Parser (ParseError(..), ParseState(..), ParserT(..), fail)

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

-- | Match one or more times.
-- |
-- | Stack-safe version of `many1` at the expense of a `MonadRec` constraint.
many1Rec :: forall m s a. ParserT s m a -> ParserT s m (NonEmptyList a)
many1Rec p = NEL.cons' <$> p <*> manyRec p

-- | Parse phrases delimited by a separator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | digit `sepBy` string ","
-- | ```
sepBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepBy p sep = map NEL.toList (sepBy1 p sep) <|> pure Nil

-- | Parse phrases delimited by a separator.
-- |
-- | Stack-safe version of `sepBy` at the expense of a `MonadRec` constraint.
sepByRec :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepByRec p sep = map NEL.toList (sepBy1Rec p sep) <|> pure Nil

-- | Parse phrases delimited by a separator, requiring at least one match.
sepBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepBy1 p sep = do
  a <- p
  as <- many $ sep *> p
  pure (NEL.cons' a as)

-- | Parse phrases delimited by a separator, requiring at least one match.
-- |
-- | Stack-safe version of `sepBy1` at the expense of a `MonadRec` constraint.
sepBy1Rec :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepBy1Rec p sep = do
  a <- p
  as <- manyRec $ sep *> p
  pure (NEL.cons' a as)

-- | Parse phrases delimited and optionally terminated by a separator.
sepEndBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndBy p sep = map NEL.toList (sepEndBy1 p sep) <|> pure Nil

-- | Parse phrases delimited and optionally terminated by a separator.
-- |
-- | Stack-safe version of `sepEndBy` at the expense of a `MonadRec` constraint.
sepEndByRec :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
sepEndByRec p sep = map NEL.toList (sepEndBy1Rec p sep) <|> pure Nil

-- | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
sepEndBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepEndBy1 p sep = do
  a <- p
  ( do
      _ <- sep
      as <- sepEndBy p sep
      pure (NEL.cons' a as)
  ) <|> pure (NEL.singleton a)

-- | Parse phrases delimited and optionally terminated by a separator, requiring at least one match.
-- |
-- | Stack-safe version of `sepEndBy1` at the expense of a `MonadRec` constraint.
sepEndBy1Rec :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
sepEndBy1Rec p sep = do
  a <- p
  (NEL.cons' a <$> tailRecM go Nil) <|> pure (NEL.singleton a)
  where
  go :: List a -> ParserT s m (Step (List a) (List a))
  go acc = nextOne <|> done
    where
    nextOne = do
      -- First make sure there's a separator.
      _ <- sep
      -- Then try the phrase and loop if it's there, or bail if it's not there.
      (p <#> \a -> Loop $ a : acc) <|> done

    done = defer \_ -> pure $ Done $ reverse acc

-- | Parse phrases delimited and terminated by a separator, requiring at least one match.
endBy1 :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
endBy1 p sep = many1 $ p <* sep

-- | Parse phrases delimited and terminated by a separator, requiring at least one match.
-- |
-- | Stack-safe version of `endBy1` at the expense of a `MonadRec` constraint.
endBy1Rec :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (NonEmptyList a)
endBy1Rec p sep = many1Rec $ p <* sep

-- | Parse phrases delimited and terminated by a separator.
endBy :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
endBy p sep = many $ p <* sep

-- | Parse phrases delimited and terminated by a separator.
-- |
-- | Stack-safe version of `endBy` at the expense of a `MonadRec` constraint.
endByRec :: forall m s a sep. ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
endByRec p sep = manyRec $ p <* sep

-- | Parse phrases delimited by a right-associative operator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | chainr digit (string "+" $> add) 0
-- | ```
chainr :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainr p f a = chainr1 p f <|> pure a

-- | Parse phrases delimited by a right-associative operator.
-- |
-- | Stack-safe version of `chainr` at the expense of a `MonadRec` constraint.
chainrRec :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainrRec p f a = chainr1Rec p f <|> pure a

-- | Parse phrases delimited by a left-associative operator.
-- |
-- | For example:
-- |
-- | ```purescript
-- | chainr digit (string "+" $> add) 0
-- | ```
chainl :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainl p f a = chainl1 p f <|> pure a

-- | Parse phrases delimited by a left-associative operator.
-- |
-- | Stack-safe version of `chainl` at the expense of a `MonadRec` constraint.
chainlRec :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
chainlRec p f a = chainl1Rec p f <|> pure a

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

-- | Parse phrases delimited by a left-associative operator, requiring at least one match.
-- |
-- | Stack-safe version of `chainl1` at the expense of a `MonadRec` constraint.
chainl1Rec :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainl1Rec p f = do
  a <- p
  tailRecM go a
  where
  go :: a -> ParserT s m (Step a a)
  go a =
    ( do
        op <- f
        a' <- p
        pure $ Loop $ op a a'
    )
      <|> pure (Done a)

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

-- | Parse phrases delimited by a right-associative operator, requiring at least one match.
-- |
-- | Stack-safe version of `chainr1` at the expense of a `MonadRec` constraint.
chainr1Rec :: forall m s a. ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
chainr1Rec p f = do
  a <- p
  tailRecM go { last: a, init: Nil }
  where
  -- This looks scary at first glance, so I'm leaving a comment in a vain
  -- attempt to explain how it works.
  --
  -- The loop state is a record {init, last}, where `last` is the last (i.e.
  -- rightmost) `a` value that has been parsed so far, and `init` is a list of
  -- (value + operator) pairs that have been parsed before that.
  --
  -- The very first value is parsed at top level, and it becomes the initial
  -- value of `last`, while the initial value of `init` is just `Nil`,
  -- indicating that no pairs of (value + operator) have been parsed yet.
  --
  -- At every step, we parse an operator and a value, and then the newly parsed
  -- value becomes `last` (because, well, it's been parsed last), and the pair
  -- of (previous last + operator) is prepended to `init`.
  --
  -- After we can no longer parse a pair of (value + operation), we're done. At
  -- that point, we have a list of (value + operation) pairs in reverse order
  -- (since we prepend each pair as we go) and the very last value. All that's
  -- left is combine them all via `foldl`.
  go
    :: { init :: List (a /\ (a -> a -> a)), last :: a }
    -> ParserT s m
         ( Step
             { init :: List (a /\ (a -> a -> a)), last :: a }
             a
         )
  go { last, init } =
    ( do
        op <- f
        a <- p
        pure $ Loop { last: a, init: (last /\ op) : init }
    )
      <|> pure (Done $ foldl apply last init)

  apply :: a -> (a /\ (a -> a -> a)) -> a
  apply y (x /\ op) = x `op` y

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

-- | Skip many instances of a phrase.
-- |
-- | Stack-safe version of `skipMany` at the expense of a `MonadRec` constraint.
skipManyRec :: forall s a m. ParserT s m a -> ParserT s m Unit
skipManyRec p = skipMany1Rec p <|> pure unit

-- | Skip at least one instance of a phrase.
skipMany1 :: forall s a m. ParserT s m a -> ParserT s m Unit
skipMany1 p = do
  _ <- p
  _ <- skipMany p
  pure unit

-- | Skip at least one instance of a phrase.
-- |
-- | Stack-safe version of `skipMany1` at the expense of a `MonadRec` constraint.
skipMany1Rec :: forall s a m. ParserT s m a -> ParserT s m Unit
skipMany1Rec p = p *> tailRecM go unit
  where
  go _ = (p $> Loop unit) <|> pure (Done unit)

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

-- | Parse many phrases until the terminator phrase matches.
-- |
-- | Stack-safe version of `manyTill` at the expense of a `MonadRec` constraint.
manyTillRec :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (List a)
manyTillRec p end = tailRecM go Nil
  where
  go :: List a -> ParserT s m (Step (List a) (List a))
  go acc =
    (end <#> \_ -> Done $ reverse acc)
      <|> (p <#> \x -> Loop $ x : acc)

-- | Parse at least one phrase until the terminator phrase matches.
many1Till :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (NonEmptyList a)
many1Till p end = do
  x <- p
  xs <- manyTill p end
  pure (NEL.cons' x xs)

-- | Parse at least one phrase until the terminator phrase matches.
-- |
-- | Stack-safe version of `many1Till` at the expense of a `MonadRec` constraint.
many1TillRec :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (NonEmptyList a)
many1TillRec p end = NEL.cons' <$> p <*> manyTillRec p end

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

-- | Parse many phrases until the terminator phrase matches.
-- | Returns the list of phrases and the terminator phrase.
-- |
-- | Stack-safe version of `manyTill_` at the expense of a `MonadRec` constraint.
manyTillRec_ :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (List a) e)
manyTillRec_ p end = tailRecM go Nil
  where
  go :: List a -> ParserT s m (Step (List a) (Tuple (List a) e))
  go xs =
    do
      t <- end
      pure (Done (Tuple (reverse xs) t))
      <|>
        do
          x <- p
          pure (Loop (x : xs))

-- | Parse many phrases until the terminator phrase matches, requiring at least one match.
-- | Returns the list of phrases and the terminator phrase.
-- |
-- | Stack-safe version of `many1Till_` at the expense of a `MonadRec` constraint.
many1TillRec_ :: forall s a m e. ParserT s m a -> ParserT s m e -> ParserT s m (Tuple (NonEmptyList a) e)
many1TillRec_ p end = do
  x <- p
  Tuple xs t <- manyTillRec_ p end
  pure $ Tuple (cons' x xs) t
