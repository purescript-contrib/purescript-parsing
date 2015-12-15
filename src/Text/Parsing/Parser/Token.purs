-- | Functions for working with streams of tokens.

module Text.Parsing.Parser.Token where

import Prelude

import Data.Either
import Data.List (List(..))

import Control.Monad.State.Class hiding (get)
import Control.MonadPlus

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Pos

-- | Create a parser which returns the first token in the stream.
token :: forall m a. (Monad m) => (a -> Position) -> ParserT (List a) m a
token tokpos = ParserT $ \(PState { input: toks, position: pos }) ->
  return $ case toks of
    Cons x xs -> { consumed: true, input: xs, result: Right x, position: tokpos x }
    _ -> parseFailed toks pos "expected token, met EOF"

-- | Create a parser which matches any token satisfying the predicate.
when :: forall m a. (Monad m) => (a -> Position) -> (a -> Boolean) -> ParserT (List a) m a
when tokpos f = try $ do
  a <- token tokpos
  guard $ f a
  return a

-- | Match the specified token at the head of the stream.
match :: forall a m. (Monad m, Eq a) => (a -> Position) -> a -> ParserT (List a) m a
match tokpos tok = when tokpos (== tok)
