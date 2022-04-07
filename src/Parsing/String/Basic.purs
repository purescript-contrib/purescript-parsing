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
  , whiteSpace
  , skipSpaces
  , oneOf
  , oneOfCodePoints
  , noneOf
  , noneOfCodePoints
  ) where

import Prelude

import Data.Array (elem, notElem)
import Data.CodePoint.Unicode (isAlpha, isAlphaNum, isDecDigit, isHexDigit, isLower, isOctDigit, isSpace, isUpper)
import Data.Either (Either(..))
import Data.Int as Data.Int
import Data.Maybe (Maybe(..))
import Data.Number (infinity, nan)
import Data.Number as Data.Number
import Data.String (CodePoint, singleton, takeWhile)
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), fst)
import Parsing (ParserT, fail)
import Parsing.Combinators (choice, skipMany, (<?>), (<~?>))
import Parsing.String (consumeWith, match, satisfy, satisfyCodePoint)
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
          _ <- oneOf [ '+', '-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
          skipMany $ oneOf [ 'e', 'E', '+', '-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
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
    _ <- oneOf [ '+', '-', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
    skipMany $ oneOf [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' ]
  case Data.Int.fromString section of
    Nothing -> fail $ "Could not parse Int " <> section
    Just x -> pure x

-- | Helper function
satisfyCP :: forall m. (CodePoint -> Boolean) -> ParserT String m Char
satisfyCP p = Parser.String.satisfy (p <<< codePointFromChar)

-- | Match zero or more whitespace characters satisfying
-- | `Data.CodePoint.Unicode.isSpace`. Always succeeds.
whiteSpace :: forall m. ParserT String m String
whiteSpace = fst <$> match skipSpaces

-- | Skip whitespace characters and throw them away. Always succeeds.
skipSpaces :: forall m. ParserT String m Unit
skipSpaces = consumeWith \input -> do
  let consumed = takeWhile isSpace input
  let remainder = SCU.drop (SCU.length consumed) input
  Right { value: unit, consumed, remainder }

-- | Match one of the BMP `Char`s in the array.
oneOf :: forall m. Array Char -> ParserT String m Char
oneOf ss = satisfy (flip elem ss) <~?> \_ -> "one of " <> show ss

-- | Match any BMP `Char` not in the array.
noneOf :: forall m. Array Char -> ParserT String m Char
noneOf ss = satisfy (flip notElem ss) <~?> \_ -> "none of " <> show ss

-- | Match one of the Unicode characters in the array.
oneOfCodePoints :: forall m. Array CodePoint -> ParserT String m CodePoint
oneOfCodePoints ss = satisfyCodePoint (flip elem ss) <~?> \_ -> "one of " <> show (singleton <$> ss)

-- | Match any Unicode character not in the array.
noneOfCodePoints :: forall m. Array CodePoint -> ParserT String m CodePoint
noneOfCodePoints ss = satisfyCodePoint (flip notElem ss) <~?> \_ -> "none of " <> show (singleton <$> ss)
