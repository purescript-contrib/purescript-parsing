-- | Functions for working with streams of tokens.

module Text.Parsing.Parser.Token
  ( token
  , when
  , match
  , LanguageDef
  , GenLanguageDef(LanguageDef)
  , unGenLanguageDef
  , TokenParser
  , GenTokenParser
  , makeTokenParser
  -- should these be exported?  Maybe they should go in a different module?
  , digit
  , hexDigit
  , octDigit
  , upper
  , space
  , letter
  , alphaNum
  )
    where

import Prelude hiding (when, between)

import Control.Lazy (fix)
import Control.MonadPlus (guard, (<|>))

import Data.Array as Array
import Data.Char (fromCharCode, toCharCode)
import Data.Char.Unicode (digitToInt, isAlpha, isAlphaNum, isDigit, isHexDigit, isOctDigit, isSpace, isUpper)
import Data.Char.Unicode as Unicode
import Data.Either (Either(..))
import Data.Foldable (foldl, foldr)
import Data.Identity (Identity)
import Data.Int (toNumber)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..), maybe)
import Data.String (toCharArray, null, toLower, fromCharArray, singleton, uncons)
import Data.Tuple (Tuple(..))

import Math (pow)

import Text.Parsing.Parser (PState(..), ParserT(..), Result(..), fail, parseFailed)
import Text.Parsing.Parser.Combinators (skipMany1, try, skipMany, notFollowedBy, option, choice, between, sepBy1, sepBy, (<?>), (<??>))
import Text.Parsing.Parser.Pos (Position)
import Text.Parsing.Parser.String (satisfy, oneOf, noneOf, string, char)

-- | Create a parser which Returns the first token in the stream.
token :: forall m a. Monad m => (a -> Position) -> ParserT (List a) m a
token tokpos = ParserT $ \(PState toks pos) ->
  pure $ case toks of
    Cons x xs -> Result xs (Right x) true (tokpos x)
    _ -> parseFailed toks pos "expected token, met EOF"

-- | Create a parser which matches any token satisfying the predicate.
when :: forall m a. Monad m => (a -> Position) -> (a -> Boolean) -> ParserT (List a) m a
when tokpos f = try $ do
  a <- token tokpos
  guard $ f a
  pure a

-- | Match the specified token at the head of the stream.
match :: forall a m. (Monad m, Eq a) => (a -> Position) -> a -> ParserT (List a) m a
match tokpos tok = when tokpos (_ == tok)

type LanguageDef = GenLanguageDef String Identity

-- | The `GenLanguageDef` type is a record that contains all parameterizable
-- | features of the "Text.Parsec.Token" module. The module `Text.Parsec.Languager`
-- | contains some default definitions.
newtype GenLanguageDef s m
    = LanguageDef {
    -- | Describes the start of a block comment. Use the empty string if the
    -- | language doesn't support block comments. For example `/*`.
    commentStart   :: String,
    -- | Describes the end of a block comment. Use the empty string if the
    -- | language doesn't support block comments. For example `*/`.
    commentEnd     :: String,
    -- | Describes the start of a line comment. Use the empty string if the
    -- | language doesn't support line comments. For example `//`.
    commentLine    :: String,
    -- | Set to `true` if the language supports nested block comments.
    nestedComments :: Boolean,
    -- | This parser should accept any start characters of identifiers. For
    -- | example `letter <|> char '_'`.
    identStart     :: ParserT s m Char,
    -- | This parser should accept any legal tail characters of identifiers.
    -- | For example `alphaNum <|> char '_'`.
    identLetter    :: ParserT s m Char,
    -- | This parser should accept any start characters of operators. For
    -- | example `oneOf [':', '+', '=']`.
    opStart        :: ParserT s m Char,
    -- | This parser should accept any legal tail characters of operators.
    -- | Note that this parser should even be defined if the language doesn't
    -- | support user-defined operators, or otherwise the `reservedOp`
    -- | parser won't work correctly.
    opLetter       :: ParserT s m Char,
    -- | The list of reserved identifiers.
    reservedNames  :: Array String,
    -- | The list of reserved operators.
    reservedOpNames:: Array String,
    -- | Set to `true` if the language is case sensitive.
    caseSensitive  :: Boolean
    }

unGenLanguageDef :: forall s m . GenLanguageDef s m -> { caseSensitive :: Boolean
                                                       , reservedOpNames :: Array String
                                                       , reservedNames :: Array String
                                                       , opLetter :: ParserT s m Char
                                                       , opStart :: ParserT s m Char
                                                       , identLetter :: ParserT s m Char
                                                       , identStart :: ParserT s m Char
                                                       , nestedComments :: Boolean
                                                       , commentLine :: String
                                                       , commentEnd :: String
                                                       , commentStart :: String
                                                       }
unGenLanguageDef (LanguageDef langDef) = langDef

-----------------------------------------------------------
-- A first class module: TokenParser
-----------------------------------------------------------

type TokenParser = GenTokenParser String Identity

-- | The type of the record that holds lexical parsers that work on
-- | `s` streams over a monad `m`.
type GenTokenParser s m
    = {
        -- | This lexeme parser parses a legal identifier. Returns the identifier
        -- | string. This parser will fail on identifiers that are reserved
        -- | words. Legal identifier (start) characters and reserved words are
        -- | defined in the `LanguageDef` that is passed to
        -- | `makeTokenParser`. An `identifier` is treated as
        -- | a single token using `try`.
        identifier       :: ParserT s m String,
        -- | The lexeme parser `reserved name` parses `symbol
        -- | name`, but it also checks that the `name` is not a prefix of a
        -- | valid identifier. A `reserved` word is treated as a single token
        -- | using `try`.
        reserved         :: String -> ParserT s m Unit,
        -- | This lexeme parser parses a legal operator. Returns the name of the
        -- | operator. This parser will fail on any operators that are reserved
        -- | operators. Legal operator (start) characters and reserved operators
        -- | are defined in the `LanguageDef` that is passed to
        -- | `makeTokenParser`. An `operator` is treated as a
        -- | single token using `try`.
        operator         :: ParserT s m String,
        -- |The lexeme parser `reservedOp name` parses `symbol
        -- | name`, but it also checks that the `name` is not a prefix of a
        -- | valid operator. A `reservedOp` is treated as a single token using
        -- | `try`.
        reservedOp       :: String -> ParserT s m Unit,
        -- | This lexeme parser parses a single literal character. Returns the
        -- | literal character value. This parsers deals correctly with escape
        -- | sequences. The literal character is parsed according to the grammar
        -- | rules defined in the Haskell report (which matches most programming
        -- | languages quite closely).
        charLiteral      :: ParserT s m Char,
        -- | This lexeme parser parses a literal string. Returns the literal
        -- | string value. This parsers deals correctly with escape sequences and
        -- | gaps. The literal string is parsed according to the grammar rules
        -- | defined in the Haskell report (which matches most programming
        -- | languages quite closely).
        stringLiteral    :: ParserT s m String,
        -- | This lexeme parser parses a natural number (a positive whole
        -- | number). Returns the value of the number. The number can be
        -- | specified in `decimal`, `hexadecimal` or
        -- | `octal`. The number is parsed according to the grammar
        -- | rules in the Haskell report.
        natural          :: ParserT s m Int,
        -- | This lexeme parser parses an integer (a whole number). This parser
        -- | is like `natural` except that it can be prefixed with
        -- | sign (i.e. `-` or `+`). Returns the value of the number. The
        -- | number can be specified in `decimal`, `hexadecimal`
        -- | or `octal`. The number is parsed according
        -- | to the grammar rules in the Haskell report.
        integer          :: ParserT s m Int,
        -- | This lexeme parser parses a floating point value. Returns the value
        -- | of the number. The number is parsed according to the grammar rules
        -- | defined in the Haskell report.
        float            :: ParserT s m Number,
        -- | This lexeme parser parses either `natural` or a `float`.
        -- | Returns the value of the number. This parsers deals with
        -- | any overlap in the grammar rules for naturals and floats. The number
        -- | is parsed according to the grammar rules defined in the Haskell report.
        naturalOrFloat   :: ParserT s m (Either Int Number),
        -- | Parses a positive whole number in the decimal system. Returns the
        -- | value of the number.
        decimal          :: ParserT s m Int,
        -- | Parses a positive whole number in the hexadecimal system. The number
        -- | should be prefixed with `0x` or `0X`. Returns the value of the
        -- | number.
        hexadecimal      :: ParserT s m Int,
        -- | Parses a positive whole number in the octal system. The number
        -- | should be prefixed with `0o` or `0O`. Returns the value of the
        -- | number.
        octal            :: ParserT s m Int,
        -- | Lexeme parser `symbol s` parses `string` `s` and skips
        -- | trailing white space.
        symbol           :: String -> ParserT s m String,
        -- | `lexeme p` first applies parser `p` and than the `whiteSpace`
        -- | parser, returning the value of `p`. Every lexical
        -- | token (lexeme) is defined using `lexeme`, this way every parse
        -- | starts at a point without white space. Parsers that use `lexeme` are
        -- | called *lexeme* parsers in this document.
        -- |
        -- | The only point where the `whiteSpace` parser should be
        -- | called explicitly is the start of the main parser in order to skip
        -- | any leading white space.
        -- |
        -- | ```purescript
        -- | mainParser = do
        -- |   whiteSpace
        -- |   ds <- many (lexeme digit)
        -- |   eof
        -- |   pure (sum ds)
        -- | ```
        lexeme           :: forall a. ParserT s m a -> ParserT s m a,
        -- | Parses any white space. White space consists of *zero* or more
        -- | occurrences of a `space`, a line comment or a block (multi
        -- | line) comment. Block comments may be nested. How comments are
        -- | started and ended is defined in the `LanguageDef`
        -- | that is passed to `makeTokenParser`.
        whiteSpace       :: ParserT s m Unit,
        -- | Lexeme parser `parens p` parses `p` enclosed in parenthesis,
        -- | returning the value of `p`.
        parens           :: forall a. ParserT s m a -> ParserT s m a,
        -- | Lexeme parser `braces p` parses `p` enclosed in braces (`{` and
        -- | `}`), returning the value of `p`.
        braces           :: forall a. ParserT s m a -> ParserT s m a,
        -- | Lexeme parser `angles p` parses `p` enclosed in angle brackets (`<`
        -- | and `>`), returning the value of `p`.
        angles           :: forall a. ParserT s m a -> ParserT s m a,
        -- | Lexeme parser `brackets p` parses `p` enclosed in brackets (`[`
        -- | and `]`), returning the value of `p`.
        brackets         :: forall a. ParserT s m a -> ParserT s m a,
        -- | Lexeme parser `semi` parses the character `;` and skips any
        -- | trailing white space. Returns the string `;`.
        semi             :: ParserT s m String,
        -- | Lexeme parser `comma` parses the character `,` and skips any
        -- | trailing white space. Returns the string `,`.
        comma            :: ParserT s m String,
        -- | Lexeme parser `colon` parses the character `:` and skips any
        -- | trailing white space. Returns the string `:`.
        colon            :: ParserT s m String,
        -- | Lexeme parser `dot` parses the character `.` and skips any
        -- | trailing white space. Returns the string `.`.
        dot              :: ParserT s m String,
        -- | Lexeme parser `semiSep p` parses *zero* or more occurrences of `p`
        -- | separated by `semi`. Returns a list of values pureed by
        -- | `p`.
        semiSep          :: forall a . ParserT s m a -> ParserT s m (List a),
        -- | Lexeme parser `semiSep1 p` parses *one* or more occurrences of `p`
        -- | separated by `semi`. Returns a list of values pureed by `p`.
        semiSep1         :: forall a . ParserT s m a -> ParserT s m (List a),
        -- | Lexeme parser `commaSep p` parses *zero* or more occurrences of
        -- | `p` separated by `comma`. Returns a list of values pureed
        -- | by `p`.
        commaSep         :: forall a . ParserT s m a -> ParserT s m (List a),
        -- | Lexeme parser `commaSep1 p` parses *one* or more occurrences of
        -- | `p` separated by `comma`. Returns a list of values pureed
        -- | by `p`.
        commaSep1        :: forall a . ParserT s m a -> ParserT s m (List a)
    }

-----------------------------------------------------------
-- Given a LanguageDef, create a token parser.
-----------------------------------------------------------

-- | The expression `makeTokenParser language` creates a `GenTokenParser`
-- | record that contains lexical parsers that are
-- | defined using the definitions in the `language` record.
-- |
-- | The use of this function is quite stylized - one imports the
-- | appropiate language definition and selects the lexical parsers that
-- | are needed from the resulting `GenTokenParser`.
-- |
-- | ```purescript
-- | module Main where
-- |
-- | import Text.Parsing.Parser.Language (haskellDef)
-- | import Text.Parsing.Parser.Token (makeTokenParser)
-- |
-- | -- The parser
-- | expr = parens expr
-- |    <|> identifier
-- |    <|> ...
-- |
-- |
-- | -- The lexer
-- | tokenParser = makeTokenParser haskellDef
-- | parens      = tokenParser.parens
-- | braces      = tokenParser.braces
-- | identifier  = tokenParser.identifier
-- | reserved    = tokenParser.reserved
-- | ...
-- | ```
makeTokenParser :: forall m . Monad m => GenLanguageDef String m -> GenTokenParser String m
makeTokenParser (LanguageDef languageDef)
    = { identifier: identifier
      , reserved: reserved
      , operator: operator
      , reservedOp: reservedOp

      , charLiteral: charLiteral
      , stringLiteral: stringLiteral
      , natural: natural
      , integer: integer
      , float: float
      , naturalOrFloat: naturalOrFloat
      , decimal: decimal
      , hexadecimal: hexadecimal
      , octal: octal

      , symbol: symbol
      , lexeme: lexeme
      , whiteSpace: whiteSpace' (LanguageDef languageDef)

      , parens: parens
      , braces: braces
      , angles: angles
      , brackets: brackets
      , semi: semi
      , comma: comma
      , colon: colon
      , dot: dot
      , semiSep: semiSep
      , semiSep1: semiSep1
      , commaSep: commaSep
      , commaSep1: commaSep1
      }
  where
    -----------------------------------------------------------
    -- Bracketing
    -----------------------------------------------------------
    parens :: forall a . ParserT String m a -> ParserT String m a
    parens p = between (symbol "(") (symbol ")") p

    braces :: forall a . ParserT String m a -> ParserT String m a
    braces p = between (symbol "{") (symbol "}") p

    angles :: forall a . ParserT String m a -> ParserT String m a
    angles p = between (symbol "<") (symbol ">") p

    brackets :: forall a . ParserT String m a -> ParserT String m a
    brackets p = between (symbol "[") (symbol "]") p

    semi :: ParserT String m String
    semi = symbol ";"

    comma :: ParserT String m String
    comma = symbol ","

    dot :: ParserT String m String
    dot = symbol "."

    colon :: ParserT String m String
    colon = symbol ":"

    commaSep :: forall a . ParserT String m a -> ParserT String m (List a)
    commaSep p = sepBy p comma

    semiSep :: forall a . ParserT String m a -> ParserT String m (List a)
    semiSep p = sepBy p semi

    commaSep1 :: forall a . ParserT String m a -> ParserT String m (List a)
    commaSep1 p = sepBy1 p comma

    semiSep1 :: forall a . ParserT String m a -> ParserT String m (List a)
    semiSep1 p = sepBy1 p semi

    -----------------------------------------------------------
    -- Chars & Strings
    -----------------------------------------------------------
    charLiteral :: ParserT String m Char
    charLiteral = lexeme go <?> "character"
      where
        go :: ParserT String m Char
        go = between (char '\'') (char '\'' <?> "end of character") characterChar

    characterChar :: ParserT String m Char
    characterChar = charLetter <|> charEscape <?> "literal character"

    charEscape :: ParserT String m Char
    charEscape = char '\\' *> escapeCode

    charLetter :: ParserT String m Char
    charLetter = satisfy \c -> (c /= '\'') && (c /= '\\') && (c > '\026')

    stringLiteral :: ParserT String m String
    stringLiteral = lexeme (go <?> "literal string")
      where
        go :: ParserT String m String
        go = do
            maybeChars <- between (char '"') (char '"' <?> "end of string") (List.many stringChar)
            pure $ fromCharArray $ List.toUnfoldable $ foldr folder Nil maybeChars

        folder :: Maybe Char -> List Char -> List Char
        folder Nothing chars = chars
        folder (Just c) chars = Cons c chars


    stringChar :: ParserT String m (Maybe Char)
    stringChar = (Just <$> stringLetter)
             <|> stringEscape
             <?> "string character"

    stringLetter :: ParserT String m Char
    stringLetter = satisfy (\c -> (c /= '"') && (c /= '\\') && (c > '\026'))

    stringEscape :: ParserT String m (Maybe Char)
    stringEscape = do
        char '\\'
        (escapeGap $> Nothing) <|> (escapeEmpty $> Nothing) <|> (Just <$> escapeCode)

    escapeEmpty :: ParserT String m Char
    escapeEmpty = char '&'

    escapeGap :: ParserT String m Char
    escapeGap = Array.some space *> char '\\' <?> "end of string gap"

    -- -- escape codes
    escapeCode :: ParserT String m Char
    escapeCode = charEsc <|> charNum <|> charAscii <|> charControl
             <?> "escape code"

    charControl :: ParserT String m Char
    charControl = do
        char '^'
        code <- upper
        pure <<< fromCharCode $ toCharCode code - toCharCode 'A' + 1

    charNum :: ParserT String m Char
    charNum = do
        code <- decimal
           <|> ( char 'o' *> number 8 octDigit )
           <|> ( char 'x' *> number 16 hexDigit )
        if code > 0x10FFFF
           then fail "invalid escape sequence"
           else pure $ fromCharCode code

    charEsc :: ParserT String m Char
    charEsc = choice (map parseEsc escMap)
      where
        parseEsc :: Tuple Char Char -> ParserT String m Char
        parseEsc (Tuple c code) = char c $> code

    charAscii :: ParserT String m Char
    charAscii = choice (map parseAscii asciiMap)
      where
        parseAscii :: Tuple String Char -> ParserT String m Char
        parseAscii (Tuple asc code) = try $ string asc $> code

    -- escape code tables
    escMap :: Array (Tuple Char Char)
    escMap = Array.zip [  'a',  'b',  'f',  'n',  'r',  't',  'v', '\\', '\"', '\'' ]
                       [ '\a', '\b', '\f', '\n', '\r', '\t', '\v', '\\', '\"', '\'' ]

    asciiMap :: Array (Tuple String Char)
    asciiMap = Array.zip (ascii3codes <> ascii2codes) (ascii3 <> ascii2)

    ascii2codes :: Array String
    ascii2codes = [ "BS", "HT", "LF", "VT", "FF", "CR", "SO", "SI", "EM", "FS", "GS", "RS", "US", "SP" ]

    ascii3codes :: Array String
    ascii3codes = [ "NUL", "SOH", "STX", "ETX", "EOT", "ENQ", "ACK", "BEL"
                  , "DLE", "DC1", "DC2", "DC3", "DC4", "NAK", "SYN", "ETB"
                  , "CAN", "SUB", "ESC", "DEL"
                  ]

    ascii2 :: Array Char
    ascii2 = [ '\BS', '\HT', '\LF', '\VT', '\FF', '\CR', '\SO', '\SI'
             , '\EM', '\FS', '\GS', '\RS', '\US', '\SP'
             ]

    ascii3 :: Array Char
    ascii3 = [ '\NUL', '\SOH', '\STX', '\ETX', '\EOT', '\ENQ', '\ACK'
             , '\BEL', '\DLE', '\DC1', '\DC2', '\DC3', '\DC4', '\NAK'
             , '\SYN', '\ETB', '\CAN', '\SUB', '\ESC', '\DEL'
             ]

    -----------------------------------------------------------
    -- Numbers
    -----------------------------------------------------------

    naturalOrFloat :: ParserT String m (Either Int Number)
    naturalOrFloat = lexeme (natFloat) <?> "number"

    float :: ParserT String m Number
    float = lexeme floating <?> "float"

    integer :: ParserT String m Int
    integer = lexeme int <?> "integer"

    natural :: ParserT String m Int
    natural = lexeme nat <?> "natural"

    -- floats
    floating :: ParserT String m Number
    floating = decimal >>= fractExponent

    natFloat :: ParserT String m (Either Int Number)
    natFloat = char '0' *> zeroNumFloat
           <|> decimalFloat

    zeroNumFloat :: ParserT String m (Either Int Number)
    zeroNumFloat = Left <$> (hexadecimal <|> octal)
               <|> decimalFloat
               <|> fractFloat 0
               <|> pure (Left 0)

    decimalFloat :: ParserT String m (Either Int Number)
    decimalFloat = do
        n <- decimal
        option (Left n) (fractFloat n)

    fractFloat :: forall a . Int -> ParserT String m (Either a Number)
    fractFloat n = Right <$> fractExponent n

    fractExponent :: Int -> ParserT String m Number
    fractExponent n = fractExponent' <|> justExponent
      where
        fractExponent' :: ParserT String m Number
        fractExponent' = do
            fract <- fraction
            expo  <- option 1.0 exponent'
            pure $ (toNumber n + fract) * expo

        justExponent :: ParserT String m Number
        justExponent = do
            expo <- exponent'
            pure $ (toNumber n * expo)

    fraction :: ParserT String m Number
    fraction = "fraction" <??> do
        char '.'
        digits <- Array.some digit <?> "fraction"
        maybe (fail "not digit") pure $ foldr op (Just 0.0) digits
      where
        op :: Char -> Maybe Number -> Maybe Number
        op _ Nothing  = Nothing
        op d (Just f) = do
            int' <- digitToInt d
            pure $ ( f + toNumber int' ) / 10.0

    exponent' :: ParserT String m Number
    exponent' = "exponent" <??> do
        oneOf ['e', 'E']
        f <- sign
        e <- decimal <?> "exponent"
        pure $ power (f e)
      where
        power :: Int -> Number
        power e | e < 0      = 1.0 / power (-e)
                | otherwise  = 10.0 `pow` toNumber e

    -- integers and naturals
    int :: ParserT String m Int
    int = do
        f <- lexeme sign
        n <- nat
        pure $ f n

    sign :: forall a . (Ring a) => ParserT String m (a -> a)
    sign = (char '-' $> negate)
       <|> (char '+' $> id)
       <|> pure id

    nat :: ParserT String m Int
    nat = zeroNumber <|> decimal

    zeroNumber :: ParserT String m Int
    zeroNumber = char '0' *>
                    ( hexadecimal <|> octal <|> decimal <|> pure 0 ) <?> ""

    decimal :: ParserT String m Int
    decimal = number 10 digit

    hexadecimal :: ParserT String m Int
    hexadecimal = oneOf ['x', 'X'] *> number 16 hexDigit

    octal :: ParserT String m Int
    octal = oneOf ['o', 'O'] *> number 8 octDigit

    number :: Int -> ParserT String m Char -> ParserT String m Int
    number base baseDigit = do
        digits <- Array.some baseDigit
        maybe (fail "not digits") pure $ foldl folder (Just 0) digits
      where
        folder :: Maybe Int -> Char -> Maybe Int
        folder Nothing _ = Nothing
        folder (Just x) d = ((base * x) + _) <$> digitToInt d

    -----------------------------------------------------------
    -- Operators & reserved ops
    -----------------------------------------------------------

    reservedOp :: String -> ParserT String m Unit
    reservedOp name = lexeme $ try go
      where
        go :: ParserT String m Unit
        go = do
            string name
            notFollowedBy languageDef.opLetter <?> "end of " <> name

    operator :: ParserT String m String
    operator = lexeme $ try go
      where
        go :: ParserT String m String
        go = do
            name <- oper
            if (isReservedOp name)
                then fail ("reserved operator " <> name)
                else pure name

    oper :: ParserT String m String
    oper = go <?> "operator"
      where
        go :: ParserT String m String
        go = do
            c <- languageDef.opStart
            cs <- Array.many languageDef.opLetter
            pure $ singleton c <> fromCharArray cs

    isReservedOp :: String -> Boolean
    isReservedOp name = isReserved (Array.sort languageDef.reservedOpNames) name


    -----------------------------------------------------------
    -- Identifiers & Reserved words
    -----------------------------------------------------------

    reserved :: String -> ParserT String m Unit
    reserved name = lexeme $ try go
      where
        go :: ParserT String m Unit
        go = caseString name *> (notFollowedBy languageDef.identLetter <?> "end of " <> name)

    caseString :: String -> ParserT String m String
    caseString name | languageDef.caseSensitive = string name
                    | otherwise                 = walk name $> name
      where
        walk :: String -> ParserT String m Unit
        walk name' = case uncons name' of
                        Nothing -> pure unit
                        Just { head: c, tail: cs } -> (caseChar c <?> msg) *> walk cs

        caseChar :: Char -> ParserT String m Char
        caseChar c | isAlpha c = char (Unicode.toLower c) <|> char (Unicode.toUpper c)
                   | otherwise = char c

        msg :: String
        msg = show name


    identifier :: ParserT String m String
    identifier = lexeme $ try go
      where
        go :: ParserT String m String
        go = do
            name <- ident
            if (isReservedName (LanguageDef languageDef) name)
               then fail ("reserved word " <> show name)
               else pure name


    ident :: ParserT String m String
    ident = go <?> "identitfier"
      where
        go :: ParserT String m String
        go = do
            c <- languageDef.identStart
            cs <- Array.many languageDef.identLetter
            pure $ singleton c <> fromCharArray cs


    -----------------------------------------------------------
    -- White space & symbols
    -----------------------------------------------------------
    symbol :: String -> ParserT String m String
    symbol name = lexeme (string name)

    lexeme :: forall a . ParserT String m a -> ParserT String m a
    lexeme p = p <* whiteSpace' (LanguageDef languageDef)


-- ================================================================================ --
-- The following functions should really be in the where-clause of makeTokenParser, --
-- but they can't go there because they are mutually recursive.                     --
-- ================================================================================ --

-----------------------------------------------------------
-- Identifiers & Reserved words
-----------------------------------------------------------

isReservedName :: forall m . Monad m => GenLanguageDef String m -> String -> Boolean
isReservedName langDef@(LanguageDef languageDef) name =
    isReserved (theReservedNames langDef) caseName
  where
    caseName | languageDef.caseSensitive  = name
             | otherwise                  = toLower name

isReserved :: Array String -> String -> Boolean
isReserved names name =
    case Array.uncons names of
        Nothing -> false
        Just { head: r, tail: rs } -> case (compare r name) of
                                        LT  -> isReserved rs name
                                        EQ  -> true
                                        GT  -> false

theReservedNames :: forall m . Monad m => GenLanguageDef String m -> Array String
theReservedNames (LanguageDef languageDef)
    | languageDef.caseSensitive = Array.sort languageDef.reservedNames
    | otherwise                 = Array.sort $ map toLower languageDef.reservedNames

-----------------------------------------------------------
-- White space & symbols
-----------------------------------------------------------

whiteSpace' :: forall m . Monad m => GenLanguageDef String m -> ParserT String m Unit
whiteSpace' langDef@(LanguageDef languageDef)
    | null languageDef.commentLine && null languageDef.commentStart =
        skipMany (simpleSpace <?> "")
    | null languageDef.commentLine =
        skipMany (simpleSpace <|> multiLineComment langDef <?> "")
    | null languageDef.commentStart =
        skipMany (simpleSpace <|> oneLineComment langDef <?> "")
    | otherwise =
        skipMany (simpleSpace <|> oneLineComment langDef <|> multiLineComment langDef <?> "")

simpleSpace :: forall m . Monad m => ParserT String m Unit
simpleSpace = skipMany1 (satisfy isSpace)

oneLineComment :: forall m . Monad m => GenLanguageDef String m -> ParserT String m Unit
oneLineComment (LanguageDef languageDef) =
    try (string languageDef.commentLine) *> skipMany (satisfy (_ /= '\n'))

multiLineComment :: forall m . Monad m => GenLanguageDef String m -> ParserT String m Unit
multiLineComment langDef@(LanguageDef languageDef) =
    try (string languageDef.commentStart) *> inComment langDef

inComment :: forall m . Monad m => GenLanguageDef String m -> ParserT String m Unit
inComment langDef@(LanguageDef languageDef) =
    if languageDef.nestedComments then inCommentMulti langDef else inCommentSingle langDef

inCommentMulti :: forall m . Monad m => GenLanguageDef String m -> ParserT String m Unit
inCommentMulti langDef@(LanguageDef languageDef) =
    fix \p -> ( void $ try (string languageDef.commentEnd) )
          <|> ( multiLineComment langDef    *>  p )
          <|> ( skipMany1 (noneOf startEnd) *> p )
          <|> ( oneOf startEnd              *> p )
          <?> "end of comment"
  where
    startEnd :: Array Char
    startEnd   = toCharArray languageDef.commentEnd <> toCharArray languageDef.commentStart

inCommentSingle :: forall m . Monad m => GenLanguageDef String m -> ParserT String m Unit
inCommentSingle (LanguageDef languageDef) =
    fix \p -> ( void $ try (string languageDef.commentEnd) )
          <|> ( skipMany1 (noneOf startEnd) *> p )
          <|> ( oneOf startEnd              *> p )
          <?> "end of comment"
  where
    startEnd :: Array Char
    startEnd = toCharArray languageDef.commentEnd <> toCharArray languageDef.commentStart

-------------------------------------------------------------------------
-- Helper functions that should maybe go in Text.Parsing.Parser.String --
-------------------------------------------------------------------------

-- | Parse a digit.  Matches any char that satisfies `Data.Char.Unicode.isDigit`.
digit :: forall m . Monad m => ParserT String m Char
digit = satisfy isDigit <?> "digit"

-- | Parse a hex digit.  Matches any char that satisfies `Data.Char.Unicode.isHexDigit`.
hexDigit :: forall m . Monad m => ParserT String m Char
hexDigit = satisfy isHexDigit <?> "hex digit"

-- | Parse an octal digit.  Matches any char that satisfies `Data.Char.Unicode.isOctDigit`.
octDigit :: forall m . Monad m => ParserT String m Char
octDigit = satisfy isOctDigit <?> "oct digit"

-- | Parse an uppercase letter.  Matches any char that satisfies `Data.Char.Unicode.isUpper`.
upper :: forall m . Monad m => ParserT String m Char
upper = satisfy isUpper <?> "uppercase letter"

-- | Parse a space character.  Matches any char that satisfies `Data.Char.Unicode.isSpace`.
space :: forall m . Monad m => ParserT String m Char
space = satisfy isSpace <?> "space"

-- | Parse an alphabetical character.  Matches any char that satisfies `Data.Char.Unicode.isAlpha`.
letter :: forall m . Monad m => ParserT String m Char
letter = satisfy isAlpha <?> "letter"

-- | Parse an alphabetical or numerical character.
-- | Matches any char that satisfies `Data.Char.Unicode.isAlphaNum`.
alphaNum :: forall m . Monad m => ParserT String m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"
