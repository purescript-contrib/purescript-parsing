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
module Parsing.String
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
  , consumeWith
  ) where

import Prelude hiding (between)

import Control.Monad.State (get)
import Data.Array (elem, notElem)
import Data.Array.NonEmpty as NonEmptyArray
import Data.CodePoint.Unicode (isSpace)
import Data.Either (Either(..))
import Data.Enum (fromEnum, toEnum)
import Data.Function.Uncurried (mkFn5, runFn2)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (CodePoint, Pattern(..), codePointAt, length, null, singleton, splitAt, stripPrefix, takeWhile, uncons)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags)
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafePartial)
import Parsing (ParseError(..), ParseState(..), ParserT(..))
import Parsing.Combinators ((<?>), (<~?>))
import Parsing.Pos (Position(..))

-- | Match “end-of-file,” the end of the input stream.
eof :: forall m. ParserT String m Unit
eof = ParserT
  ( mkFn5 \state1@(ParseState input pos _) _ _ throw done ->
      if null input then
        -- We must consume so this combines correctly with notFollowedBy
        runFn2 done (ParseState input pos true) unit
      else
        runFn2 throw state1 (ParseError "Expected EOF" pos)
  )

-- | Match the entire rest of the input stream. Always succeeds.
rest :: forall m. ParserT String m String
rest = consumeWith \consumed ->
  Right { value: consumed, consumed, remainder: "" }

-- | Match the specified string.
string :: forall m. String -> ParserT String m String
string str = consumeWith \input ->
  case stripPrefix (Pattern str) input of
    Just remainder ->
      Right { value: str, consumed: str, remainder }
    _ ->
      Left $ "Expected " <> show str

-- | Match any BMP `Char`.
-- | Parser will fail if the character is not in the Basic Multilingual Plane.
anyChar :: forall m. ParserT String m Char
anyChar = satisfy (const true)

-- | Match any Unicode character.
-- | Always succeeds.
anyCodePoint :: forall m. ParserT String m CodePoint
anyCodePoint = satisfyCodePoint (const true)

-- | Match a BMP `Char` satisfying the predicate.
satisfy :: forall m. (Char -> Boolean) -> ParserT String m Char
satisfy f = ParserT
  ( mkFn5 \state1@(ParseState input pos _) _ _ throw done ->
      case uncons input of
        Nothing ->
          runFn2 throw state1 (ParseError "Unexpected EOF" pos)
        Just { head, tail } -> do
          let cp = fromEnum head
          -- the `fromEnum` function doesn't check if this is beyond the
          -- BMP, so we check that ourselves.
          -- https://github.com/purescript/purescript-strings/issues/153
          if cp < 0 || cp > 65535 then
            runFn2 throw state1 (ParseError "Expected Char" pos)
          else do
            let ch = unsafePartial (fromJust (toEnum cp))
            if f ch then
              runFn2 done (ParseState tail (updatePosSingle pos head tail) true) ch
            else
              runFn2 throw state1 (ParseError "Predicate unsatisfied" pos)
  )

-- | Match a Unicode character satisfying the predicate.
satisfyCodePoint :: forall m. (CodePoint -> Boolean) -> ParserT String m CodePoint
satisfyCodePoint f = ParserT
  ( mkFn5 \state1@(ParseState input pos _) _ _ throw done ->
      case uncons input of
        Nothing ->
          runFn2 throw state1 (ParseError "Unexpected EOF" pos)
        Just { head, tail } ->
          if f head then
            runFn2 done (ParseState tail (updatePosSingle pos head tail) true) head
          else
            runFn2 throw state1 (ParseError "Predicate unsatisfied" pos)
  )

-- | Match the specified BMP `Char`.
char :: forall m. Char -> ParserT String m Char
char c = satisfy (_ == c) <?> show c

-- | Match a `String` exactly *N* characters long.
takeN :: forall m. Int -> ParserT String m String
takeN n = consumeWith \input -> do
  let { before, after } = splitAt n input
  if length before == n then
    Right { value: before, consumed: before, remainder: after }
  else
    Left $ "Could not take " <> show n <> " characters"

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

-- | Updates a `Position` by adding the columns and lines in `String`.
updatePosString :: Position -> String -> String -> Position
updatePosString pos before after = case uncons before of
  Nothing -> pos
  Just { head, tail } -> do
    let
      newPos
        | String.null tail = updatePosSingle pos head after
        | otherwise = updatePosSingle pos head tail
    updatePosString newPos tail after

-- | Updates a `Position` by adding the columns and lines in a
-- | single `CodePoint`.
updatePosSingle :: Position -> CodePoint -> String -> Position
updatePosSingle (Position { line, column }) cp after = case fromEnum cp of
  10 -> Position { line: line + 1, column: 1 } -- "\n"
  13 ->
    case codePointAt 0 after of
      Just nextCp | fromEnum nextCp == 10 -> Position { line, column } -- "\r\n" lookahead
      _ -> Position { line: line + 1, column: 1 } -- "\r"
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
match :: forall m a. ParserT String m a -> ParserT String m (Tuple String a)
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

-- | Compile a regular expression string into a regular expression parser.
-- |
-- | This function will use the `Data.String.Regex.regex` function to compile and return a parser which can be used
-- | in a `ParserT String m` monad.
-- |
-- | This parser will try to match the regular expression pattern starting
-- | at the current parser position. On success, it will return the matched
-- | substring.
-- |
-- | [*MDN Regular Expressions Cheatsheet*](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions/Cheatsheet)
-- |
-- | This function should be called outside the context of a `ParserT String m` monad, because this function might
-- | fail with a `Left` RegExp compilation error message.
-- | If you call this function inside of the `ParserT String m` monad and then `fail` the parse when the compilation fails,
-- | then that could be confusing because a parser failure is supposed to indicate an invalid input string.
-- | If the compilation failure occurs in an `alt` then the compilation failure might not be reported at all and instead
-- | the input string would be parsed incorrectly.
-- |
-- | This parser may be useful for quickly consuming a large section of the
-- | input `String`, because in a JavaScript runtime environment the RegExp
-- | runtime is a lot faster than primitive parsers.
-- |
-- | #### Example
-- |
-- | This example shows how to compile and run the `xMany` parser which will
-- | capture the regular expression pattern `x*`.
-- |
-- | ```purescript
-- | case regex "x*" noFlags of
-- |   Left compileError -> unsafeCrashWith $ "xMany failed to compile: " <> compileError
-- |   Right xMany -> runParser "xxxZ" do
-- |     xMany
-- | ```
-- |
-- | #### Flags
-- |
-- | Set `RegexFlags` with the `Semigroup` instance like this.
-- |
-- | ```purescript
-- | regex "x*" (dotAll <> ignoreCase)
-- | ```
-- |
-- | The `dotAll`, `unicode`, and `ignoreCase` flags might make sense for a `regex` parser. The other flags will
-- | probably cause surprising behavior and you should avoid them.
-- |
-- | [*MDN Advanced searching with flags*](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#advanced_searching_with_flags)
regex :: forall m. String -> RegexFlags -> Either String (ParserT String m String)
regex pattern flags =
  Regex.regex ("^(" <> pattern <> ")") flags <#> \regexobj ->
    consumeWith \input -> do
      case NonEmptyArray.head <$> Regex.match regexobj input of
        Just (Just consumed) -> do
          let remainder = SCU.drop (SCU.length consumed) input
          Right { value: consumed, consumed, remainder }
        _ ->
          Left "No Regex pattern match"

-- | Consume a portion of the input string while yielding a value.
-- |
-- | Takes a consumption function which takes the remaining input `String`
-- | as its argument and returns three fields:
-- |
-- | * `value` is the value to return.
-- | * `consumed` is the input `String` that was consumed. It is used to update the parser position.
-- | * `remainder` is the new remaining input `String`.
consumeWith
  :: forall m a
   . (String -> Either String { value :: a, consumed :: String, remainder :: String })
  -> ParserT String m a
consumeWith f = ParserT
  ( mkFn5 \state1@(ParseState input pos _) _ _ throw done ->
      case f input of
        Left err ->
          runFn2 throw state1 (ParseError err pos)
        Right { value, consumed, remainder } ->
          runFn2 done (ParseState remainder (updatePosString pos consumed remainder) true) value
  )
