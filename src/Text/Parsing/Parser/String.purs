-- | Primitive parsers for working with an input stream of type `String`.

module Text.Parsing.Parser.String where

import Prelude

import Data.String
import Data.Either
import Data.Foldable
import Data.Maybe
import Data.Char (toString)
import Data.Array (many)

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators
import Text.Parsing.Parser.Pos

-- | Match end-of-file.
eof :: forall m. (Monad m) => ParserT String m Unit
eof = ParserT $ \(PState { input: s, position: pos }) ->
  return $ case s of
    "" -> { consumed: false, input: s, result: Right unit, position: pos }
    _  -> parseFailed s pos "Expected EOF"

-- | Match the specified string.
string :: forall m. (Monad m) => String -> ParserT String m String
string str = ParserT $ \(PState { input: s, position: pos })  ->
  return $ case indexOf str s of
    Just 0 -> { consumed: true, input: drop (length str) s, result: Right str, position: updatePosString pos str }
    _ -> parseFailed s pos ("Expected " ++ str)

-- | Match any character.
anyChar :: forall m. (Monad m) => ParserT String m Char
anyChar = ParserT $ \(PState { input: s, position: pos }) ->
  return $ case charAt 0 s of
    Nothing -> parseFailed s pos "Unexpected EOF"
    Just c  -> { consumed: true, input: drop 1 s, result: Right c, position: updatePosString pos (toString c) }

-- | Match a character satisfying the specified predicate.
satisfy :: forall m. (Monad m) => (Char -> Boolean) -> ParserT String m Char
satisfy f = try do
  c <- anyChar
  if f c then return c
         else fail $ "Character '" ++ fromChar c ++ "' did not satisfy predicate"

-- | Match the specified character
char :: forall m. (Monad m) => Char -> ParserT String m Char
char c = satisfy (== c)

-- | Match a whitespace character.
whiteSpace :: forall m. (Monad m) => ParserT String m String
whiteSpace = do
  cs <- many $ satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'
  return $ fromCharArray cs

-- | Skip whitespace characters.
skipSpaces :: forall m. (Monad m) => ParserT String m Unit
skipSpaces = do
  whiteSpace
  return unit

-- | Match one of the characters in the array.
oneOf :: forall m. (Monad m) => Array Char -> ParserT String m Char
oneOf ss = satisfy (flip elem ss)

-- | Match any character not in the array.
noneOf :: forall m. (Monad m) => Array Char -> ParserT String m Char
noneOf ss = satisfy (flip notElem ss)

