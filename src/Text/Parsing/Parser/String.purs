-- | Primitive parsers for working with an input stream of type `String`.

module Text.Parsing.Parser.String where

import Prelude hiding (between)

import Data.Array (many)
import Data.Either (Either(..))
import Data.Foldable (elem, notElem)
import Data.Maybe (Maybe(..))
import Data.String (charAt, drop, fromCharArray, indexOf, length, singleton)
import Text.Parsing.Parser (PState(..), ParserT(..), Result(..), fail, parseFailed)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.Pos (updatePosString)

-- | Match end-of-file.
eof :: forall m. (Monad m) => ParserT String m Unit
eof = ParserT $ \(PState s pos) ->
  pure $ case s of
    "" -> Result s (Right unit) false pos
    _  -> parseFailed s pos "Expected EOF"

-- | Match the specified string.
string :: forall m. (Monad m) => String -> ParserT String m String
string str = ParserT $ \(PState s pos)  ->
  pure $ case indexOf str s of
    Just 0 -> Result (drop (length str) s) (Right str) true (updatePosString pos str)
    _ -> parseFailed s pos ("Expected " <> str)

-- | Match any character.
anyChar :: forall m. (Monad m) => ParserT String m Char
anyChar = ParserT $ \(PState s pos) ->
  pure $ case charAt 0 s of
    Nothing -> parseFailed s pos "Unexpected EOF"
    Just c  -> Result (drop 1 s) (Right c) true (updatePosString pos (singleton c))

-- | Match a character satisfying the specified predicate.
satisfy :: forall m. (Monad m) => (Char -> Boolean) -> ParserT String m Char
satisfy f = try do
  c <- anyChar
  if f c then pure c
         else fail $ "Character '" <> singleton c <> "' did not satisfy predicate"

-- | Match the specified character
char :: forall m. (Monad m) => Char -> ParserT String m Char
char c = satisfy (_ == c)

-- | Match a whitespace character.
whiteSpace :: forall m. (Monad m) => ParserT String m String
whiteSpace = do
  cs <- many $ satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
  pure $ fromCharArray cs

-- | Skip whitespace characters.
skipSpaces :: forall m. (Monad m) => ParserT String m Unit
skipSpaces = do
  whiteSpace
  pure unit

-- | Match one of the characters in the array.
oneOf :: forall m. (Monad m) => Array Char -> ParserT String m Char
oneOf ss = satisfy (flip elem ss)

-- | Match any character not in the array.
noneOf :: forall m. (Monad m) => Array Char -> ParserT String m Char
noneOf ss = satisfy (flip notElem ss)
