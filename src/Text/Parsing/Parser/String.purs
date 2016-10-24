-- | Primitive parsers for working with an input stream of type `String`.

module Text.Parsing.Parser.String where

import Data.String as S
import Control.Monad.State (modify, gets)
import Data.Array (many)
import Data.Foldable (elem, notElem)
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.String (Pattern, fromCharArray, length, singleton)
import Text.Parsing.Parser (ParseState(..), ParserT, fail)
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.Pos (updatePosString)
import Prelude hiding (between)

-- | This class exists to abstract over streams which support the string-like
-- | operations which this modules needs.
class StringLike s where
  drop :: Int -> s -> s
  indexOf :: Pattern -> s -> Maybe Int
  null :: s -> Boolean
  uncons :: s -> Maybe { head :: Char, tail :: s }

instance stringLikeString :: StringLike String where
  uncons = S.uncons
  drop = S.drop
  indexOf = S.indexOf
  null = S.null

-- | Match end-of-file.
eof :: forall s m. (StringLike s, Monad m) => ParserT s m Unit
eof = do
  input <- gets \(ParseState { input }) -> input
  unless (null input) (fail "Expected EOF")

-- | Match the specified string.
string :: forall s m. (StringLike s, Monad m) => String -> ParserT s m String
string str = do
  input <- gets \(ParseState { input }) -> input
  case indexOf (wrap str) input of
    Just 0 -> do
      modify \(ParseState { position }) ->
        ParseState { position: updatePosString position str
                   , consumed: true
                   , input: drop (length str) input
                   }
      pure str
    _ -> fail ("Expected " <> show str)

-- | Match any character.
anyChar :: forall s m. (StringLike s, Monad m) => ParserT s m Char
anyChar = do
  input <- gets \(ParseState { input }) -> input
  case uncons input of
    Nothing -> fail "Unexpected EOF"
    Just { head, tail } -> do
      modify \(ParseState { position }) ->
        ParseState { position: updatePosString position (singleton head)
                   , consumed: true
                   , input: tail
                   }
      pure head

-- | Match a character satisfying the specified predicate.
satisfy :: forall s m. (StringLike s, Monad m) => (Char -> Boolean) -> ParserT s m Char
satisfy f = try do
  c <- anyChar
  if f c then pure c
         else fail $ "Character '" <> singleton c <> "' did not satisfy predicate"

-- | Match the specified character
char :: forall s m. (StringLike s, Monad m) => Char -> ParserT s m Char
char c = satisfy (_ == c) <?> ("Expected " <> show c)

-- | Match a whitespace character.
whiteSpace :: forall s m. (StringLike s, Monad m) => ParserT s m String
whiteSpace = do
  cs <- many $ satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
  pure $ fromCharArray cs

-- | Skip whitespace characters.
skipSpaces :: forall s m. (StringLike s, Monad m) => ParserT s m Unit
skipSpaces = do
  whiteSpace
  pure unit

-- | Match one of the characters in the array.
oneOf :: forall s m. (StringLike s, Monad m) => Array Char -> ParserT s m Char
oneOf ss = satisfy (flip elem ss) <?> ("Expected one of " <> show ss)

-- | Match any character not in the array.
noneOf :: forall s m. (StringLike s, Monad m) => Array Char -> ParserT s m Char
noneOf ss = satisfy (flip notElem ss) <?> ("Expected none of " <> show ss)
