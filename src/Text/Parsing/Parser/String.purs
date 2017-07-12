-- | Primitive parsers for working with an input stream of type `String`.

module Text.Parsing.Parser.String where

import Data.Array (many)
import Data.Char.Unicode (isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit, isSpace, isUpper)
import Prelude hiding (between)
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators ((<?>))
import Text.Parsing.Parser.Stream (class StreamLike, satisfy)

-- | Match a whitespace characters but returns them using Array.
whiteSpace :: forall f m. StreamLike f Char => Monad m => ParserT f m (Array Char)
whiteSpace = many space

-- | Skip whitespace characters.
skipSpaces :: forall f m. StreamLike f Char => Monad m => ParserT f m Unit
skipSpaces = void whiteSpace

-- | Parse a digit.  Matches any char that satisfies `Data.Char.Unicode.isDigit`.
digit :: forall f m . StreamLike f Char => Monad m => ParserT f m Char
digit = satisfy isDigit <?> "digit"

-- | Parse a hex digit.  Matches any char that satisfies `Data.Char.Unicode.isHexDigit`.
hexDigit :: forall f m . StreamLike f Char => Monad m => ParserT f m Char
hexDigit = satisfy isHexDigit <?> "hex digit"

-- | Parse an octal digit.  Matches any char that satisfies `Data.Char.Unicode.isOctDigit`.
octDigit :: forall f m . StreamLike f Char => Monad m => ParserT f m Char
octDigit = satisfy isOctDigit <?> "oct digit"

-- | Parse an uppercase letter.  Matches any char that satisfies `Data.Char.Unicode.isUpper`.
upper :: forall f m . StreamLike f Char => Monad m => ParserT f m Char
upper = satisfy isUpper <?> "uppercase letter"

-- | Parse a space character.  Matches any char that satisfies `Data.Char.Unicode.isSpace`.
space :: forall f m . StreamLike f Char => Monad m => ParserT f m Char
space = satisfy isSpace <?> "space"

-- | Parse an alphabetical character.  Matches any char that satisfies `Data.Char.Unicode.isAlpha`.
letter :: forall f m . StreamLike f Char => Monad m => ParserT f m Char
letter = satisfy isAlpha <?> "letter"

-- | Parse an alphabetical or numerical character.
-- | Matches any char that satisfies `Data.Char.Unicode.isAlphaNum`.
alphaNum :: forall f m . StreamLike f Char => Monad m => ParserT f m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"
