-- | Basic `String` parsers derived from primitive `String` parsers.
-- |
-- | Note: In the future, the
-- | __noneOf__, __noneOfCodePoints__, __oneOf__, __oneOfCodePoints__, __skipSpaces__, __whiteSpace__
-- | should be moved into this module and removed from the
-- | __Parsing.String__ module, because they are not primitive parsers.
module Parsing.String.Basic
  ( digit
  , hexDigit
  , octDigit
  , letter
  , space
  , lower
  , upper
  , alphaNum
  , intDecimal
  , number
  , module Parsing.String
  ) where

import Prelude

import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isDecDigit, isHexDigit, isLower, isOctDigit, isSpace, isUpper)
import Data.Int as Data.Int
import Data.Maybe (Maybe(..))
import Data.Number (infinity, nan)
import Data.Number as Data.Number
import Data.String (CodePoint)
import Data.String.CodePoints (codePointFromChar)
import Data.Tuple (Tuple(..))
import Parsing (ParserT, fail)
import Parsing.Combinators (choice, skipMany, (<?>))
import Parsing.String (noneOf, noneOfCodePoints, oneOf, oneOfCodePoints, skipSpaces, whiteSpace)
import Parsing.String as Parser.String

-- | Parse a digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isDecDigit`.
digit :: forall m. ParserT String m Char
digit = satisfyCP isDecDigit <?> "digit"

-- | Parse a hex digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isHexDigit`.
hexDigit :: forall m. ParserT String m Char
hexDigit = satisfyCP isHexDigit <?> "hex digit"

-- | Parse an octal digit.  Matches any char that satisfies `Data.CodePoint.Unicode.isOctDigit`.
octDigit :: forall m. ParserT String m Char
octDigit = satisfyCP isOctDigit <?> "oct digit"

-- | Parse a lowercase letter.  Matches any char that satisfies `Data.CodePoint.Unicode.isLower`.
lower :: forall m. ParserT String m Char
lower = satisfyCP isLower <?> "lowercase letter"

-- | Parse an uppercase letter.  Matches any char that satisfies `Data.CodePoint.Unicode.isUpper`.
upper :: forall m. ParserT String m Char
upper = satisfyCP isUpper <?> "uppercase letter"

-- | Parse a space character.  Matches any char that satisfies `Data.CodePoint.Unicode.isSpace`.
space :: forall m. ParserT String m Char
space = satisfyCP isSpace <?> "space"

-- | Parse an alphabetical character.  Matches any char that satisfies `Data.CodePoint.Unicode.isAlpha`.
letter :: forall m. ParserT String m Char
letter = satisfyCP isAlpha <?> "letter"

-- | Parse an alphabetical or numerical character.
-- | Matches any char that satisfies `Data.CodePoint.Unicode.isAlphaNum`.
alphaNum :: forall m. ParserT String m Char
alphaNum = satisfyCP isAlphaNum <?> "letter or digit"

-- | Parser based on the __Data.Number.fromString__ function.
-- |
-- | This should be the inverse of `show :: String -> Number`.
-- |
-- | Examples of strings which can be parsed by this parser:
-- | * `"3"`
-- | * `"3.0"`
-- | * `"0.3"`
-- | * `"-0.3"`
-- | * `"+0.3"`
-- | * `"-3e-1"`
-- | * `"-3.0E-1.0"`
-- | * `"NaN"`
-- | * `"-Infinity"`
number :: forall m. ParserT String m Number
-- TODO because the JavaScript parseFloat function will successfully parse
-- a Number up until it doesn't understand something and then return
-- the partially parsed Number, this parser will sometimes consume more
-- String that it actually parses. Example "1..3" will parse as 1.0.
-- So this needs improvement.
number =
  choice
    [ Parser.String.string "Infinity" *> pure infinity
    , Parser.String.string "+Infinity" *> pure infinity
    , Parser.String.string "-Infinity" *> pure (negate infinity)
    , Parser.String.string "NaN" *> pure nan
    , do
        Tuple section _ <- Parser.String.match do
          _ <- Parser.String.oneOf [ '+', '-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
          skipMany $ Parser.String.oneOf [ 'e', 'E', '+', '-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
        -- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/parseFloat
        case Data.Number.fromString section of
          Nothing -> fail $ "Could not parse Number " <> section
          Just x -> pure x
    ]

-- | Parser based on the __Data.Int.fromString__ function.
-- |
-- | This should be the inverse of `show :: String -> Int`.
-- |
-- | Examples of strings which can be parsed by this parser:
-- | * `"3"`
-- | * `"-3"`
-- | * `"+300"`
intDecimal :: forall m. ParserT String m Int
intDecimal = do
  Tuple section _ <- Parser.String.match do
    _ <- Parser.String.oneOf [ '+', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
    skipMany $ Parser.String.oneOf [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
  case Data.Int.fromString section of
    Nothing -> fail $ "Could not parse Int " <> section
    Just x -> pure x

-- | Helper function
satisfyCP :: forall m. (CodePoint -> Boolean) -> ParserT String m Char
satisfyCP p = Parser.String.satisfy (p <<< codePointFromChar)
