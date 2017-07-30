-- | Primitive parsers for working with an input stream of type `String`.

module Text.Parsing.Parser.String where

import Data.Array (many)
import Data.Char.Unicode (isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit, isSpace, isUpper)
import Prelude hiding (between)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Stream (class Stream, satisfy)

-- | Match a whitespace characters but returns them using Array.
whiteSpace :: forall s m. Stream s m Char => Monad m => ParserT s m (Array Char)
whiteSpace = many space

-- | Skip whitespace characters.
skipSpaces :: forall s m. Stream s m Char => Monad m => ParserT s m Unit
skipSpaces = void whiteSpace

-- | Parse a digit.  Matches any char that satisfies `Data.Char.Unicode.isDigit`.
digit :: forall s m . Stream s m Char => Monad m => ParserT s m Char
digit = satisfy isDigit <?> "digit"

-- | Parse a hex digit.  Matches any char that satisfies `Data.Char.Unicode.isHexDigit`.
hexDigit :: forall s m . Stream s m Char => Monad m => ParserT s m Char
hexDigit = satisfy isHexDigit <?> "hex digit"

-- | Parse an octal digit.  Matches any char that satisfies `Data.Char.Unicode.isOctDigit`.
octDigit :: forall s m . Stream s m Char => Monad m => ParserT s m Char
octDigit = satisfy isOctDigit <?> "oct digit"

-- | Parse an uppercase letter.  Matches any char that satisfies `Data.Char.Unicode.isUpper`.
upper :: forall s m . Stream s m Char => Monad m => ParserT s m Char
upper = satisfy isUpper <?> "uppercase letter"

-- | Parse a space character.  Matches any char that satisfies `Data.Char.Unicode.isSpace`.
space :: forall s m . Stream s m Char => Monad m => ParserT s m Char
space = satisfy isSpace <?> "space"

-- | Parse an alphabetical character.  Matches any char that satisfies `Data.Char.Unicode.isAlpha`.
letter :: forall s m . Stream s m Char => Monad m => ParserT s m Char
letter = satisfy isAlpha <?> "letter"

-- | Parse an alphabetical or numerical character.
-- | Matches any char that satisfies `Data.Char.Unicode.isAlphaNum`.
alphaNum :: forall s m . Stream s m Char => Monad m => ParserT s m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"
