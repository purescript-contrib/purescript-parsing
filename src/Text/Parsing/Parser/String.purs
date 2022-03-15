-- | Primitive parsers for working with an input stream of type `String`.
-- |
-- | All of these primitive parsers will consume their input when they succeed.
-- |
-- | All of these primitive parsers will consume no input when they
-- | fail.
-- |
-- | The behavior of these primitive parsers is based on the behavior of the
-- | `Data.String` module in the __strings__ package.
-- | In most JavaScript runtime environments, the `String`
-- | is little-endian [UTF-16](https://en.wikipedia.org/wiki/UTF-16).
-- |
-- | The primitive parsers which return `Char` will only succeed when the character
-- | being parsed is a code point in the
-- | [Basic Multilingual Plane](https://en.wikipedia.org/wiki/Plane_(Unicode)#Basic_Multilingual_Plane)
-- | (the “BMP”). These parsers can be convenient because of the good support
-- | that PureScript has for writing `Char` literals like `'あ'`, `'β'`, `'C'`.
-- |
-- | The other primitive parsers, which return `CodePoint` and `String` types,
-- | can parse the full Unicode character set. All of the primitive parsers
-- | in this module can be used together.
module Text.Parsing.Parser.String
  ( string
  , eof
  , rest
  , anyChar
  , anyCodePoint
  , satisfy
  , satisfyCodePoint
  , char
  , takeN
  , whiteSpace
  , skipSpaces
  , oneOf
  , oneOfCodePoints
  , noneOf
  , noneOfCodePoints
  , match
  , regex
  , RegexFlagsRow
  ) where

import Prelude hiding (between)

import Control.Monad.State (get, put)
import Data.Array (notElem)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Char (fromCharCode)
import Data.CodePoint.Unicode (isSpace)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, Pattern(..), length, null, singleton, splitAt, stripPrefix, uncons)
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..), RegexFlagsRec)
import Data.Tuple (Tuple(..), fst)
import Prim.Row (class Nub, class Union)
import Record (merge)
import Text.Parsing.Parser (ParseState(..), ParserT, consume, fail)
import Text.Parsing.Parser.Combinators (skipMany, tryRethrow, (<?>), (<~?>))
import Text.Parsing.Parser.Pos (Position(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Match “end-of-file,” the end of the input stream.
eof :: forall m. Monad m => ParserT String m Unit
eof = do
  ParseState input _ _ <- get
  if (null input)
  -- We must consume so this combines correctly with notFollowedBy
  then consume
  else (fail "Expected EOF")

-- | Match the entire rest of the input stream. Always succeeds.
rest :: forall m. Monad m => ParserT String m String
rest = do
  ParseState input position _ <- get
  put $ ParseState "" (updatePosString position input) true
  pure input

-- | Match the specified string.
string :: forall m. Monad m => String -> ParserT String m String
string str = do
  ParseState input position _ <- get
  case stripPrefix (Pattern str) input of
    Just remainder -> do
      put $ ParseState remainder (updatePosString position str) true
      pure str
    _ -> fail ("Expected " <> show str)

-- | Match any BMP `Char`.
-- | Parser will fail if the character is not in the Basic Multilingual Plane.
anyChar :: forall m. Monad m => ParserT String m Char
anyChar = tryRethrow do
  cp :: Int <- unCodePoint <$> anyCodePoint
  -- the `fromCharCode` function doesn't check if this is beyond the
  -- BMP, so we check that ourselves.
  -- https://github.com/purescript/purescript-strings/issues/153
  if cp > 65535 -- BMP
  then fail "Not a Char"
  else case fromCharCode cp of
    Nothing -> fail "Not a Char"
    Just c -> pure c

-- | Match any Unicode character.
-- | Always succeeds.
anyCodePoint :: forall m. Monad m => ParserT String m CodePoint
anyCodePoint = do
  ParseState input position _ <- get
  case uncons input of
    Nothing -> fail "Unexpected EOF"
    Just { head, tail } -> do
      put $ ParseState tail (updatePosSingle position head) true
      pure head

-- | Match a BMP `Char` satisfying the predicate.
satisfy :: forall m. Monad m => (Char -> Boolean) -> ParserT String m Char
satisfy f = tryRethrow do
  c <- anyChar
  if f c then pure c
  else fail "Predicate unsatisfied"

-- | Match a Unicode character satisfying the predicate.
satisfyCodePoint :: forall m. Monad m => (CodePoint -> Boolean) -> ParserT String m CodePoint
satisfyCodePoint f = tryRethrow do
  c <- anyCodePoint
  if f c then pure c
  else fail "Predicate unsatisfied"

-- | Match the specified BMP `Char`.
char :: forall m. Monad m => Char -> ParserT String m Char
char c = satisfy (_ == c) <?> show c

-- | Match a `String` exactly *N* characters long.
takeN :: forall m. Monad m => Int -> ParserT String m String
takeN n = do
  ParseState input position _ <- get
  let { before, after } = splitAt n input
  if length before == n then do
    put $ ParseState after (updatePosString position before) true
    pure before
  else fail ("Could not take " <> show n <> " characters")

-- | Match zero or more whitespace characters satisfying
-- | `Data.CodePoint.Unicode.isSpace`. Always succeeds.
whiteSpace :: forall m. Monad m => ParserT String m String
whiteSpace = fst <$> match skipSpaces

-- | Skip whitespace characters and throw them away. Always succeeds.
skipSpaces :: forall m. Monad m => ParserT String m Unit
skipSpaces = skipMany (satisfyCodePoint isSpace)

-- | Match one of the BMP `Char`s in the array.
oneOf :: forall m. Monad m => Array Char -> ParserT String m Char
oneOf ss = satisfy (flip elem ss) <~?> \_ -> "one of " <> show ss

-- | Match any BMP `Char` not in the array.
noneOf :: forall m. Monad m => Array Char -> ParserT String m Char
noneOf ss = satisfy (flip notElem ss) <~?> \_ -> "none of " <> show ss

-- | Match one of the Unicode characters in the array.
oneOfCodePoints :: forall m. Monad m => Array CodePoint -> ParserT String m CodePoint
oneOfCodePoints ss = satisfyCodePoint (flip elem ss) <~?> \_ -> "one of " <> show (singleton <$> ss)

-- | Match any Unicode character not in the array.
noneOfCodePoints :: forall m. Monad m => Array CodePoint -> ParserT String m CodePoint
noneOfCodePoints ss = satisfyCodePoint (flip notElem ss) <~?> \_ -> "none of " <> show (singleton <$> ss)

-- | Updates a `Position` by adding the columns and lines in `String`.
updatePosString :: Position -> String -> Position
updatePosString pos str = case uncons str of
  Nothing -> pos
  Just { head, tail } -> updatePosString (updatePosSingle pos head) tail -- tail recursive

-- | Updates a `Position` by adding the columns and lines in a
-- | single `CodePoint`.
updatePosSingle :: Position -> CodePoint -> Position
updatePosSingle (Position { line, column }) cp = case unCodePoint cp of
  10 -> Position { line: line + 1, column: 1 } -- "\n"
  13 -> Position { line: line + 1, column: 1 } -- "\r"
  9 -> Position { line, column: column + 8 - ((column - 1) `mod` 8) } -- "\t" Who says that one tab is 8 columns?
  _ -> Position { line, column: column + 1 }

-- | Combinator which returns both the result of a parse and the slice of
-- | the input that was consumed while it was being parsed.
-- |
-- | Because `String`s are not `Char` arrays in PureScript, `many` and `some`
-- | on `Char` parsers need to
-- | be used with `Data.String.CodeUnits.fromCharArray` to
-- | construct a `String`.
-- |
-- | ```
-- | fromCharArray <$> Data.Array.many (char 'x')
-- | ```
-- |
-- | It’s more efficient to achieve the same result by using this `match` combinator
-- | instead of `fromCharArray`.
-- |
-- | ```
-- | fst <$> match (Combinators.skipMany (char 'x'))
-- | ```
match :: forall m a. Monad m => ParserT String m a -> ParserT String m (Tuple String a)
match p = do
  ParseState input1 _ _ <- get
  x <- p
  ParseState input2 _ _ <- get
  -- We use the `SCU.length`, which is in units of “code units”
  -- instead of `Data.String.length`. which is in units of “code points”.
  -- This is more efficient, and it will be correct as long as we can assume
  -- the invariant that the `ParseState input` always begins on a code point
  -- boundary.
  pure $ Tuple (SCU.take (SCU.length input1 - SCU.length input2) input1) x

-- | The CodePoint newtype constructor is not exported, so here's a helper.
-- | This will break at runtime if the definition of CodePoint ever changes
-- | to something other than `newtype CodePoint = CodePoint Int`.
unCodePoint :: CodePoint -> Int
unCodePoint = unsafeCoerce

-- | Parser which uses the `Data.String.Regex` module to match the regular
-- | expression pattern passed as the `String`
-- | argument to the parser.
-- |
-- | This parser will try to match the regular expression pattern starting
-- | at the current parser position. On success, it will return the matched
-- | substring.
-- |
-- | If the `Regex` pattern string fails to compile then this parser will fail.
-- | (Note: It’s not possible to use a precompiled `Regex` because this parser
-- | must set flags and make adjustments to the `Regex` pattern string.)
-- |
-- | This parser may be useful for quickly consuming a large section of the
-- | input `String`, because in a JavaScript runtime environment the `RegExp`
-- | runtime is a lot faster than primitive parsers.
-- |
-- | [*MDN Regular Expressions Cheatsheet*](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Cheatsheet)
-- |
-- | #### Flags
-- |
-- | The `Record flags` argument to the parser is for `Regex` flags. Here are
-- | the default flags.
-- |
-- | ```purescript
-- | { dotAll: true
-- |   ignoreCase: false
-- |   unicode: true
-- | }
-- | ```
-- |
-- | To use the defaults, pass
-- | `{}` as the flags argument. For case-insensitive pattern matching, pass
-- | `{ignoreCase: true}` as the flags argument.
-- |
-- | The other `Data.String.Regex.Flags.RegexFlagsRec` fields are mostly
-- | nonsense in the context of parsing
-- | and use of the other flags may cause strange behavior in the parser.
-- |
-- | [*MDN Advanced searching with flags*](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#advanced_searching_with_flags)
-- |
-- | #### Example
-- |
-- | ```
-- | runParser "ababXX" (regex {} "(ab)+")
-- | ```
-- | ```
-- | (Right "abab")
-- | ```
regex
  :: forall m flags f_
   . Monad m
  => Union flags RegexFlagsRow f_
  => Nub f_ RegexFlagsRow
  => Record flags
  -> String
  -> ParserT String m String
regex flags pattern =
  -- Prefix a ^ to ensure the pattern only matches the current position in the parse
  case Regex.regex ("^(" <> pattern <> ")") flags' of
    Left paterr ->
      fail $ "Regex pattern error " <> paterr
    Right regexobj -> do
      ParseState input position _ <- get
      case NonEmptyArray.head <$> Regex.match regexobj input of
        Just (Just matched) -> do
          let remainder = SCU.drop (SCU.length matched) input
          put $ ParseState remainder (updatePosString position matched) true
          pure matched
        _ -> fail $ "No Regex pattern match"
  where
  flags' = RegexFlags
    ( merge flags
        { dotAll: true
        , global: false
        , ignoreCase: false
        , multiline: false
        , sticky: false
        , unicode: true
        } :: RegexFlagsRec
    )

-- | The fields from `Data.String.Regex.Flags.RegexFlagsRec`.
type RegexFlagsRow =
  ( dotAll :: Boolean
  , global :: Boolean
  , ignoreCase :: Boolean
  , multiline :: Boolean
  , sticky :: Boolean
  , unicode :: Boolean
  )