-- Run tests:
--
--     spago -x spago-dev.dhall test
--

module Test.Main where

import Prelude hiding (between, when)

import Control.Alt ((<|>))
import Control.Lazy (fix, defer)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.State (State, lift, modify, runState)
import Data.Array (some, toUnfoldable)
import Data.Array as Array
import Data.Bifunctor (lmap, rmap)
import Data.CodePoint.Unicode as CodePoint.Unicode
import Data.Either (Either(..), either, fromLeft, hush)
import Data.Foldable (oneOf)
import Data.Function.Uncurried (mkFn5, runFn2)
import Data.List (List(..), fromFoldable, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList(..), catMaybes, cons, cons')
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.NonEmpty ((:|))
import Data.Number (infinity, nan)
import Data.Number as Data.Number
import Data.String (toUpper)
import Data.String.CodePoints as SCP
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.String.CodeUnits as SCU
import Data.String.Regex.Flags (RegexFlags, ignoreCase, noFlags)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (get2, (/\))
import Effect (Effect)
import Effect.Console (log, logShow)
import Effect.Unsafe (unsafePerformEffect)
import Node.Process (lookupEnv)
import Parsing (ParseError(..), ParseState(..), Parser, ParserT(..), Position(..), consume, fail, getParserT, initialPos, parseErrorPosition, position, region, runParser)
import Parsing.Combinators (advance, between, chainl, chainl1, chainr, chainr1, choice, empty, endBy, endBy1, lookAhead, many, many1, many1Till, many1Till_, manyIndex, manyTill, manyTill_, notFollowedBy, optional, optionMaybe, replicateA, sepBy, sepBy1, sepEndBy, sepEndBy1, skipMany, skipMany1, try, tryRethrow, (<?>), (<??>), (<~?>))
import Parsing.Combinators as Combinators
import Parsing.Combinators.Array as Combinators.Array
import Parsing.Expr (Assoc(..), Operator(..), buildExprParser)
import Parsing.Language (haskellDef, haskellStyle, javaStyle)
import Parsing.String (anyChar, anyCodePoint, anyTill, char, eof, match, parseErrorHuman, regex, rest, satisfy, string, takeN)
import Parsing.String.Basic (intDecimal, letter, noneOfCodePoints, number, oneOfCodePoints, skipSpaces, takeWhile, takeWhile1, whiteSpace)
import Parsing.String.Basic as String.Basic
import Parsing.String.Replace (breakCap, replace, replaceT, splitCap, splitCapT)
import Parsing.Token (TokenParser, makeTokenParser, token, when)
import Parsing.Token as Token
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert', assertEqual')
import Test.IndentationTests as IndentationTests
import Test.Lib (class ParseErrorHuman__OnlyString, TestM, mkParseErrorTestMessage, mkParseErrorTestPosition, mkParseTest)

parseTest :: forall s a. Show a => Eq a => ParseErrorHuman__OnlyString s => s -> a -> Parser s a -> Effect Unit
parseTest = mkParseTest runParser

parseErrorTestPosition :: forall s a. Show a => Parser s a -> s -> Position -> Effect Unit
parseErrorTestPosition = mkParseErrorTestPosition runParser

parseErrorTestMessage :: forall s a. Show a => Parser s a -> s -> String -> Effect Unit
parseErrorTestMessage = mkParseErrorTestMessage runParser

parseState :: forall m s a. (ParseState s -> Tuple (ParseState s) a) -> ParserT s m a
parseState k = ParserT
  ( mkFn5 \state1 _ _ _ done -> do
      let Tuple state2 res = k state1
      runFn2 done state2 res
  )

parens :: forall m a. ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

nested :: forall m. ParserT String m Int
nested = fix \p ->
  ( do
      _ <- string "a"
      pure 0
  ) <|> ((+) 1) <$> parens p

opTest :: Parser String String
opTest = chainl (singleton <$> anyChar) (char '+' $> append) ""

digit :: Parser String Int
digit = (string "0" >>= \_ -> pure 0)
  <|> (string "1" >>= \_ -> pure 1)
  <|> (string "2" >>= \_ -> pure 2)
  <|> (string "3" >>= \_ -> pure 3)
  <|> (string "4" >>= \_ -> pure 4)
  <|> (string "5" >>= \_ -> pure 5)
  <|> (string "6" >>= \_ -> pure 6)
  <|> (string "7" >>= \_ -> pure 7)
  <|> (string "8" >>= \_ -> pure 8)
  <|> (string "9" >>= \_ -> pure 9)

exprTest :: Parser String Int
exprTest = buildExprParser
  [ [ Infix (string "/" >>= \_ -> pure (/)) AssocRight ]
  , [ Infix (string "*" >>= \_ -> pure (*)) AssocRight ]
  , [ Infix (string "-" >>= \_ -> pure (-)) AssocRight ]
  , [ Infix (string "+" >>= \_ -> pure (+)) AssocRight ]
  ]
  digit

exprTest' :: Parser String Int
exprTest' = buildExprParser
  [ [ Postfix (string "--" >>= \_ -> pure (flip (-) 1))
    , Postfix (string "++" >>= \_ -> pure ((+) 1))
    ]
  , [ Prefix (string "-" >>= \_ -> pure negate)
    , Prefix (string "+" >>= \_ -> pure identity)
    ]
  , [ Infix (string "/" >>= \_ -> pure (/)) AssocLeft
    , Infix (string "*" >>= \_ -> pure (*)) AssocLeft
    ]
  , [ Infix (string "-" >>= \_ -> pure (-)) AssocLeft
    , Infix (string "+" >>= \_ -> pure (+)) AssocLeft
    ]
  ]
  digit

word :: String -> Parser String String
word s = string s <* whiteSpace

bool :: Parser String Boolean
bool = (word "True" >>= \_ -> pure true) <|> (word "False" >>= \_ -> pure false)

chainExprTest :: Parser String Boolean
chainExprTest = buildExprParser
  [ [ Prefix (word "not" >>= \_ -> pure not) ]
  , [ Infix (word "and" >>= \_ -> pure (&&)) AssocLeft ]
  , [ Postfix (word "ton" >>= \_ -> pure \x -> not x) ]
  ]
  bool

manySatisfyTest :: Parser String String
manySatisfyTest = do
  r <- some $ satisfy (\s -> s /= '?')
  _ <- char '?'
  pure (fromCharArray r)

mkRegexTest :: String -> String -> String -> RegexFlags -> (Parser String String -> Parser String String) -> Effect Unit
mkRegexTest input expected pattern flags pars =
  case regex pattern flags of
    Left err -> assert' ("error: " <> show err) false
    Right p -> parseTest input expected $ pars p

-- This test doesn't test the actual stack safety of these combinators, mainly
-- because I don't know how to come up with an example guaranteed to be large
-- enough to overflow the stack. But thankfully, their stack safety is more or
-- less guaranteed by construction.
--
-- Instead, this test checks functional correctness of the combinators, since
-- that's the more tricky part to get right (or to break later) in the absence
-- of clear explicit recursion.
stackSafeLoopsTest :: TestM
stackSafeLoopsTest = do
  parseTest "aaabaa" (toUnfoldable [ "a", "a", "a" ]) $
    manyTill (string "a") (string "b")
  parseTest "baa" Nil $
    manyTill (string "a") (string "b")

  parseTest "aaabaa" (NE.cons' "a" $ toUnfoldable [ "a", "a" ]) $
    many1Till (string "a") (string "b")
  parseErrorTestPosition
    (many1Till (string "a") (string "b"))
    "baa"
    (Position { index: 0, line: 1, column: 1 })

  parseTest "a,a,a,b,a,a" (toUnfoldable [ "a", "a", "a" ]) $
    sepEndBy (string "a") (string ",")
  parseTest "a,a,abaa" (toUnfoldable [ "a", "a", "a" ]) $
    sepEndBy (string "a") (string ",")
  parseTest "b,a,a" Nil $
    sepEndBy (string "a") (string ",")

  parseTest "a,a,a,b,a,a" (NE.cons' "a" $ toUnfoldable [ "a", "a" ]) $
    sepEndBy1 (string "a") (string ",")
  parseTest "a,a,abaa" (NE.cons' "a" $ toUnfoldable [ "a", "a" ]) $
    sepEndBy1 (string "a") (string ",")
  parseErrorTestPosition
    (sepEndBy1 (string "a") (string ","))
    "b,a,a"
    (Position { index: 0, line: 1, column: 1 })

  -- 8 `div` (8 `div` 2) == 2
  parseTest "8x8x2" 2 $
    chainr digit (string "x" $> div) 42
  parseTest "" 42 $
    chainr digit (string "x" $> div) 42
  parseTest "8x8x2" 2 $
    chainr1 digit (string "x" $> div)
  parseErrorTestPosition
    (chainr1 digit (string "x" $> div))
    ""
    (Position { index: 0, line: 1, column: 1 })

  -- (8 `div` 2) `div` 2 == 2
  parseTest "8x2x2" 2 $
    chainl digit (string "x" $> div) 42
  parseTest "" 42 $
    chainl digit (string "x" $> div) 42
  parseTest "8x2x2" 2 $
    chainl1 digit (string "x" $> div)
  parseErrorTestPosition
    (chainl1 digit (string "x" $> div))
    ""
    (Position { index: 0, line: 1, column: 1 })

  parseTest "aaaabcd" "b"
    $ skipMany1 (string "a")
        *> string "b"
  parseErrorTestPosition
    (skipMany1 (string "a"))
    "bcd"
    (Position { index: 0, line: 1, column: 1 })

  parseTest "aaaabcd" "b"
    $ skipMany (string "a")
        *> string "b"
  parseTest "bcd" "b"
    $ skipMany (string "a")
        *> string "b"

  parseTest "aaa" (NE.cons' "a" $ toUnfoldable [ "a", "a" ]) $
    many1 (string "a")
  parseErrorTestPosition
    (many1 (string "a"))
    ""
    (Position { index: 0, line: 1, column: 1 })

  parseTest "a,a,ab" (toUnfoldable [ "a", "a", "a" ])
    $ sepBy (string "a") (string ",")
        <* string "b"
  parseTest "b" Nil
    $ sepBy (string "a") (string ",")
        <* string "b"
  parseTest "a,a,ab" (NE.cons' "a" $ toUnfoldable [ "a", "a" ])
    $ sepBy1 (string "a") (string ",")
        <* string "b"
  parseErrorTestPosition
    (sepBy1 (string "a") (string ","))
    ""
    (Position { index: 0, line: 1, column: 1 })
  parseErrorTestPosition
    (sepBy1 (string "a") (string ","))
    "a,"
    (Position { index: 2, line: 1, column: 3 })

  parseTest "a,a,a,b" (toUnfoldable [ "a", "a", "a" ])
    $ endBy (string "a") (string ",")
        <* string "b"
  parseTest "b" Nil
    $ endBy (string "a") (string ",")
        <* string "b"
  parseTest "a,a,a,b" (NE.cons' "a" $ toUnfoldable [ "a", "a" ])
    $ endBy1 (string "a") (string ",")
        <* string "b"
  parseErrorTestPosition
    (endBy1 (string "a") (string ","))
    ""
    (Position { index: 0, line: 1, column: 1 })
  parseErrorTestPosition
    (endBy1 (string "a") (string ","))
    "a,a"
    (Position { index: 3, line: 1, column: 4 })

data TestToken = A | B

instance showTestTokens :: Show TestToken where
  show A = "A"
  show B = "B"

instance testTokensEq :: Eq TestToken where
  eq A A = true
  eq B B = true
  eq _ _ = false

isA :: TestToken -> Boolean
isA A = true
isA _ = false

testTokenParser :: TokenParser
testTokenParser = makeTokenParser haskellDef

mkPos :: Int -> Position
mkPos n = Position { index: n - 1, line: 1, column: n }

tokenParserIdentifierTest :: TestM
tokenParserIdentifierTest = do
  -- parse normal identifier
  parseTest "hello" "hello" testTokenParser.identifier

  -- error on reserved words
  parseErrorTestPosition testTokenParser.identifier "let" $ mkPos 4

  -- parse whitespace after identifier
  parseTest "hello     twice   " "twice" (testTokenParser.identifier *> testTokenParser.identifier)

  -- can't start identifiers with numbers
  parseErrorTestPosition testTokenParser.identifier "3hello" $ mkPos 1

  -- but numbers can be in the identifier
  parseTest "h3ll0  " "h3ll0" testTokenParser.identifier

  -- comments count as whitespace
  parseTest "h3ll0  -- this is a comment\nbye {- this is another comment -}" (Tuple "h3ll0" "bye")
    (Tuple <$> testTokenParser.identifier <*> testTokenParser.identifier)

  -- multiline comments work well
  parseTest "hello {- this \nis a comment -} bye" (Tuple "hello" "bye")
    (Tuple <$> testTokenParser.identifier <*> testTokenParser.identifier)

  -- nested comments are okay
  parseTest "hello {- this {- \nis a comment -} foo -} bye" (Tuple "hello" "bye")
    (Tuple <$> testTokenParser.identifier <*> testTokenParser.identifier)

  -- fail on non-matching comments
  parseErrorTestPosition testTokenParser.identifier "hello {-" $ mkPos 9

tokenParserReservedTest :: TestM
tokenParserReservedTest = do
  -- parse reserved identifier
  parseTest "forall" unit $ testTokenParser.reserved "forall"

  -- fail on nonmatching reserved identifier
  parseErrorTestPosition (testTokenParser.reserved "forall") "forall3" $ mkPos 7

  -- fail on nonmatching reserved identifier
  parseErrorTestPosition (testTokenParser.reserved "forall") "forall3" $ mkPos 7

tokenParserOperatorTest :: TestM
tokenParserOperatorTest = do
  -- parse operator
  parseTest "<>" "<>" testTokenParser.operator

  -- fail on nonoperator
  parseErrorTestPosition testTokenParser.operator "foo" $ mkPos 1

  -- fail on reserved operator
  parseErrorTestPosition testTokenParser.operator "=" $ mkPos 2

-- TODO
tokenParserReservedOpTest :: TestM
tokenParserReservedOpTest = pure unit

tokenParserCharLiteralTest :: TestM
tokenParserCharLiteralTest = do
  -- parse char literal
  parseTest "'c'" 'c' testTokenParser.charLiteral

  -- fail on slash
  parseErrorTestPosition testTokenParser.charLiteral "'\'" $ mkPos 2

  -- parse escape code
  parseTest "'\\n'" '\n' testTokenParser.charLiteral

  -- parse oct number
  parseTest "'\\o101'" 'A' testTokenParser.charLiteral

  -- parse hex number
  parseTest "'\\x41'" 'A' testTokenParser.charLiteral

  -- fail on bad oct
  parseErrorTestPosition testTokenParser.charLiteral "'\\o389'" $ mkPos 5

  -- parse ascii
  parseTest "'\\^I'" '\t' testTokenParser.charLiteral

tokenParserStringLiteralTest :: TestM
tokenParserStringLiteralTest = do
  -- parse string char
  parseTest "\"hello\"" "hello" testTokenParser.stringLiteral

  -- fail on non-operator
  parseErrorTestPosition testTokenParser.stringLiteral "he\"llo" $ mkPos 1

  -- parse string gap
  parseTest "\"he\\       \\llo\"" "hello" testTokenParser.stringLiteral

  -- parse string empty
  parseTest "\"he\\&llo\"" "hello" testTokenParser.stringLiteral

  -- parse string escape
  parseTest "\"he\\nllo\"" "he\nllo" testTokenParser.stringLiteral

tokenParserNaturalTest :: TestM
tokenParserNaturalTest = do
  -- parse natural
  parseTest "1" 1 testTokenParser.natural

  -- parse hex natural
  parseTest "0xFF" 255 testTokenParser.natural

  -- parse oct natural
  parseTest "0o10  " 8 testTokenParser.natural

  -- fail on nonoct
  parseErrorTestPosition testTokenParser.natural "0o8" $ mkPos 3

  -- fail on no digits
  parseErrorTestPosition testTokenParser.natural "0o" $ mkPos 3

tokenParserIntegerTest :: TestM
tokenParserIntegerTest = do
  -- parse integer
  parseTest "100" 100 testTokenParser.integer

  -- parse plus
  parseTest "+200" 200 testTokenParser.integer

  -- parse minus
  parseTest "-      100" (-100) testTokenParser.integer

tokenParserFloatTest :: TestM
tokenParserFloatTest = do
  -- parse float
  parseTest "100.5" 100.5 testTokenParser.float

  -- parse float prefixed with a plus sign
  parseTest "+100.5" 100.5 testTokenParser.float

  -- parse a negative float
  parseTest "-100.5" (-100.5) testTokenParser.float

  -- parse float with exponent
  parseTest "100e1" 1000.0 testTokenParser.float

  -- parse float with exponent
  parseTest "100.5e1" 1005.0 testTokenParser.float

  -- parse a negative float with exponent
  parseTest "-100.5e1" (-1005.0) testTokenParser.float

  -- fail on nonfloat
  parseErrorTestPosition testTokenParser.float "100.e1" $ mkPos 5

-- TODO
tokenParserNaturalOrFloatTest :: TestM
tokenParserNaturalOrFloatTest = do
  pure unit

tokenParserDecimalTest :: TestM
tokenParserDecimalTest = do
  -- parse decimal
  parseTest "0202" 202 testTokenParser.decimal

  -- fail on nondecimal
  parseErrorTestPosition testTokenParser.decimal "foo" $ mkPos 1

-- TODO
tokenParserHexadecimalTest :: TestM
tokenParserHexadecimalTest = do
  pure unit

-- TODO
tokenParserOctalTest :: TestM
tokenParserOctalTest = do
  pure unit

tokenParserSymbolTest :: TestM
tokenParserSymbolTest = do
  -- parse symbol
  parseTest "hello    " "hello" $ testTokenParser.symbol "hello"

-- TODO
tokenParserLexemeTest :: TestM
tokenParserLexemeTest = do
  pure unit

-- TODO
tokenParserWhiteSpaceTest :: TestM
tokenParserWhiteSpaceTest = do
  pure unit

tokenParserParensTest :: TestM
tokenParserParensTest = do
  -- parse parens
  parseTest "(hello)" "hello" $ testTokenParser.parens $ string "hello"

  -- fail on non-closed parens
  parseErrorTestPosition (testTokenParser.parens $ string "hello") "(hello" $ mkPos 7

tokenParserBracesTest :: TestM
tokenParserBracesTest = do
  -- parse braces
  parseTest "{hello}" "hello" $ testTokenParser.braces $ string "hello"

  -- fail on non-closed braces
  parseErrorTestPosition (testTokenParser.braces $ string "hello") "{hello" $ mkPos 7

tokenParserAnglesTest :: TestM
tokenParserAnglesTest = do
  -- parse angles
  parseTest "<hello>" "hello" $ testTokenParser.angles $ string "hello"

  -- fail on non-closed angles
  parseErrorTestPosition (testTokenParser.angles $ string "hello") "<hello" $ mkPos 7

tokenParserBracketsTest :: TestM
tokenParserBracketsTest = do
  -- parse brackets
  parseTest "[hello]" "hello" $ testTokenParser.brackets $ string "hello"

  -- fail on non-closed brackets
  parseErrorTestPosition (testTokenParser.brackets $ string "hello") "[hello" $ mkPos 7

tokenParserSemiTest :: TestM
tokenParserSemiTest = do
  -- parse semicolon
  parseTest ";" ";" testTokenParser.semi

  -- fail on non-semicolon
  parseErrorTestPosition testTokenParser.semi "a" $ mkPos 1

tokenParserCommaTest :: TestM
tokenParserCommaTest = do
  -- parse comma
  parseTest "," "," testTokenParser.comma

  -- fail on non-comma
  parseErrorTestPosition testTokenParser.comma "a" $ mkPos 1

tokenParserColonTest :: TestM
tokenParserColonTest = do
  -- parse colon
  parseTest ":" ":" testTokenParser.colon

  -- fail on non-colon
  parseErrorTestPosition testTokenParser.colon "a" $ mkPos 1

tokenParserDotTest :: TestM
tokenParserDotTest = do
  -- parse dot
  parseTest "." "." testTokenParser.dot

  -- fail on non-dot
  parseErrorTestPosition testTokenParser.dot "a" $ mkPos 1

tokenParserSemiSepTest :: TestM
tokenParserSemiSepTest = do
  -- parse semi sep
  parseTest "foo; foo" (fromFoldable [ "foo", "foo" ]) $ testTokenParser.semiSep $ string "foo"

  -- parse semi sep with newline
  parseTest "foo; \nfoo" (fromFoldable [ "foo", "foo" ]) $ testTokenParser.semiSep $ string "foo"

  -- parse no semi sep
  parseTest "" (fromFoldable []) $ testTokenParser.semiSep $ string "foo"

-- parseErrorTestPosition testTokenParser.operator "foo" $ mkPos 1

tokenParserSemiSep1Test :: TestM
tokenParserSemiSep1Test = do
  -- parse semi sep1
  parseTest "foo; foo" (cons "foo" (cons' "foo" Nil)) $ testTokenParser.semiSep1 $ string "foo"

  -- parse semi sep1 with newline
  parseTest "foo; \nfoo" (cons "foo" (cons' "foo" Nil)) $ testTokenParser.semiSep1 $ string "foo"

  -- no parse on empty string
  parseErrorTestPosition (testTokenParser.semiSep1 $ string "foo") "" $ mkPos 1

tokenParserCommaSepTest :: TestM
tokenParserCommaSepTest = do
  -- parse comma sep
  parseTest "foo, foo" (fromFoldable [ "foo", "foo" ]) $ testTokenParser.commaSep $ string "foo"

  -- parse comma sep with newline
  parseTest "foo, \nfoo" (fromFoldable [ "foo", "foo" ]) $ testTokenParser.commaSep $ string "foo"

  -- parse no comma sep
  parseTest "" (fromFoldable []) $ testTokenParser.commaSep $ string "foo"

tokenParserCommaSep1Test :: TestM
tokenParserCommaSep1Test = do
  -- parse comma sep1
  parseTest "foo, foo" (cons "foo" (cons' "foo" Nil)) $ testTokenParser.commaSep1 $ string "foo"

  -- parse comma sep1 with newline
  parseTest "foo, \nfoo" (cons "foo" (cons' "foo" Nil)) $ testTokenParser.commaSep1 $ string "foo"

  -- no parse on empty string
  parseErrorTestPosition (testTokenParser.commaSep1 $ string "foo") "" $ mkPos 1

haskellStyleTest :: TestM
haskellStyleTest = do
  let haskellTokParser = makeTokenParser haskellStyle

  -- make sure haskell-style comments work
  parseTest "hello {- comment\n -} fo_" "fo_" $ haskellTokParser.identifier *> haskellTokParser.identifier

  -- make sure java-style comments do not work
  parseErrorTestPosition
    (haskellTokParser.identifier *> haskellTokParser.identifier)
    "hello /* comment\n */ foo"
    (mkPos 7)

javaStyleTest :: TestM
javaStyleTest = do
  let javaTokParser = makeTokenParser javaStyle
  -- make sure java-style comments work
  parseTest "hello /* comment\n */ fo_" "fo_" $ javaTokParser.identifier *> javaTokParser.identifier

  -- make sure java-style identifier work
  parseTest "$hello /* comment\n */ _f$o_" "_f$o_" $ javaTokParser.identifier *> javaTokParser.identifier

  -- make sure haskell-style comments do not work
  parseErrorTestPosition
    (javaTokParser.identifier *> javaTokParser.identifier)
    "hello {- comment\n -} foo"
    (mkPos 7)

takeWhilePropagateFail :: TestM
takeWhilePropagateFail = do
  -- `takeWhile` always succeeds, but if input was consumed prior and failure happens
  -- later, then the failure with consumption should propagate past `optional` #236
  parseErrorTestPosition
    (optional (char 'f' <* takeWhile (const false) <* fail "failure"))
    "f"
    (Position { index: 1, line: 1, column: 2 })

applicativeSemantics :: Parser String String
applicativeSemantics =
  ( string "foo"
      <* parseState (\(ParseState a b _) -> Tuple (ParseState a b false) unit)
      <* fail "fail"
  )
    <|> pure ""

bindSemantics :: Parser String String
bindSemantics =
  ( do
      _ <- string "foo"
      parseState (\(ParseState a b _) -> Tuple (ParseState a b false) unit)
      fail "fail"
  )
    <|> pure ""

monadRecSemantics :: Parser String String
monadRecSemantics = loop <|> pure ""
  where
  loop = tailRecM
    ( case _ of
        1 -> do
          _ <- string "foo"
          pure (Loop 2)
        2 ->
          parseState (\(ParseState a b _) -> Tuple (ParseState a b false) (Loop 3))
        _ ->
          fail "fail"
    )
    1

main :: Effect Unit
main = do
  log "\nTESTS Semantics\n"
  parseErrorTestMessage applicativeSemantics "foo" "fail"
  parseErrorTestMessage bindSemantics "foo" "fail"
  parseErrorTestMessage monadRecSemantics "foo" "fail"

  log "\nTESTS Indentation\n"
  IndentationTests.testIndentationParser

  log "\nTESTS String\n"
  parseErrorTestPosition
    (many $ char 'f' *> char '?')
    "foo"
    (Position { index: 1, column: 2, line: 1 })

  parseErrorTestPosition
    (satisfy (_ == '?'))
    "foo"
    (Position { index: 0, column: 1, line: 1 })

  parseTest
    "foo"
    Nil
    (many $ try $ char 'f' *> char '?')

  parseTest "(((a)))" 3 nested
  parseTest "aaa" (Cons "a" (Cons "a" (Cons "a" Nil))) $ many (string "a")
  parseTest "abc-" [ 'a', 'b', 'c' ] $ Combinators.Array.many letter
  parseTest "(ab)" (Just "b") $ parens do
    _ <- string "a"
    optionMaybe $ string "b"
  parseTest "a,a,a" (cons "a" (cons "a" (cons' "a" Nil))) $ string "a" `sepBy1` string ","
  parseTest "a,a,a," (cons "a" (cons "a" (cons' "a" Nil))) $ do
    as <- string "a" `endBy1` string ","
    eof
    pure as
  parseTest "a+b+c" "abc" opTest
  parseTest "1*2+3/4-5" (-3) exprTest
  parseTest "1*2+3/4-5" (-3) exprTest'
  parseTest "1+++-2-----3+++4" (2) exprTest'
  parseTest "not False and not not True" (true) chainExprTest
  parseTest "True ton ton and False ton" (true) chainExprTest
  parseTest "ab?" "ab" manySatisfyTest

  parseTest "ab" unit (char 'a' *> notFollowedBy (char 'a'))

  parseTest "rest" "rest" rest
  parseTest "rest" unit (rest *> eof)
  parseTest "rest\nrest" (Position { index: 9, line: 2, column: 5 }) (rest *> position)

  parseErrorTestPosition
    (rest *> notFollowedBy eof)
    "aa\naa"
    (Position { index: 5, column: 3, line: 2 })

  parseErrorTestPosition
    (string "𝅘𝅥𝅘𝅥𝅮" *> string "𝅘𝅥𝅘𝅥𝅮")
    "𝅘𝅥𝅘𝅥𝅮x𝅘𝅥𝅯"
    (Position { index: 2, column: 3, line: 1 })

  parseTest "𝅘𝅥𝅘𝅥𝅮x𝅘𝅥𝅯" [ "𝅘𝅥", "𝅘𝅥𝅮", "x", "𝅘𝅥𝅯" ] do
    quarter <- anyCodePoint
    eighth <- (singleton <$> char 'x') <|> string "𝅘𝅥𝅮"
    letterx <- string "𝅘𝅥𝅯" <|> string "x"
    sixteenth <- string "𝅘𝅥𝅯" <|> (singleton <$> char 'x')
    pure $ [ SCP.singleton quarter, eighth, letterx, sixteenth ]

  parseTest "🤔💯✅🤔💯" [ "🤔💯", "✅🤔💯" ] do
    none <- Array.many $ noneOfCodePoints $ SCP.toCodePointArray "❓✅"
    one <- Array.many $ oneOfCodePoints $ SCP.toCodePointArray "🤔💯✅"
    pure $ SCP.fromCodePointArray <$> [ none, one ]

  parseTest "abcd" "ab" $ takeN 2
  parseTest "abcd" "" $ takeN 0
  parseErrorTestPosition (takeN 10) "abcd" (Position { index: 0, column: 1, line: 1 })
  parseErrorTestPosition (takeN (-1)) "abcd" (Position { index: 0, column: 1, line: 1 })

  parseErrorTestMessage
    (noneOfCodePoints $ SCP.toCodePointArray "❓✅")
    "❓"
    "Expected none of [\"❓\",\"✅\"]"

  parseErrorTestMessage
    (oneOfCodePoints $ SCP.toCodePointArray "❓✅")
    "abc"
    "Expected one of [\"❓\",\"✅\"]"

  parseTest "aa  bb" [ "aa", "  ", "bb" ] do
    aa <- SCU.fromCharArray <$> some letter
    w <- whiteSpace
    bb <- SCU.fromCharArray <$> some letter
    pure [ aa, w, bb ]

  let tokpos = const initialPos
  parseTest (fromFoldable [ A, B ]) A (token tokpos)
  parseTest (fromFoldable [ B, A ]) B (token tokpos)

  parseTest (fromFoldable [ A, B ]) A (when tokpos isA)

  parseTest (fromFoldable [ A ]) A (Token.match tokpos A)
  parseTest (fromFoldable [ B ]) B (Token.match tokpos B)
  parseTest (fromFoldable [ A, B ]) A (Token.match tokpos A)

  parseTest (fromFoldable []) unit Token.eof

  parseTest "aabb" (Tuple (fromFoldable [ 'a', 'a' ]) 'b') (manyTill_ (char 'a') (char 'b'))
  parseTest "aabb" (Tuple (unsafePartial $ fromJust (NE.fromFoldable [ 'a', 'a' ])) 'b') (many1Till_ (char 'a') (char 'b'))

  parseTest "aab" (Tuple (fromFoldable [ 'a', 'a' ]) 'b') do
    Tuple a b <- manyTill_ letter do
      char 'b'
    pure (Tuple a b)

  parseTest "ababab" [ 'b', 'b', 'b' ] $ Array.many (char 'a' *> char 'b')
  parseTest "abaXab" [ 'b' ] $ Array.many (try (char 'a' *> char 'b'))

  parseErrorTestPosition (string "abc") "bcd" (Position { index: 0, column: 1, line: 1 })
  parseErrorTestPosition (string "abc" *> eof) "abcdefg" (Position { index: 3, column: 4, line: 1 })
  parseErrorTestPosition (string "a\nb\nc\n" *> eof) "a\nb\nc\nd\n" (Position { index: 6, column: 1, line: 4 })
  parseErrorTestPosition (string "\ta" *> eof) "\tab" (Position { index: 2, column: 10, line: 1 })

  assertEqual' "skipSpaces consumes if position advancement issue #200"
    { actual: runParser " " do
        skipSpaces
        ParseState _ _ c <- getParserT
        pure c
    , expected: Right true
    }

  assertEqual' "skipSpaces doesn't consume if no position advancement issue #200"
    { actual: runParser "x" do
        skipSpaces
        ParseState _ _ c <- getParserT
        pure c
    , expected: Right false
    }

  do
    let
      inContext :: forall s m a. (String -> String) -> ParserT s m a -> ParserT s m a
      inContext context = region \(ParseError message pos) -> ParseError (context message) pos
      input = "Tokyo thirty-nine million"
    assertEqual' "region 1"
      { actual: runParser input do
          inContext ("Megacity list: " <> _) do
            cityname <- inContext ("city name: " <> _) (takeWhile CodePoint.Unicode.isLetter)
            skipSpaces
            population <- inContext ("population: " <> _) intDecimal
            pure $ Tuple cityname population
      , expected: (Left (ParseError "Megacity list: population: Expected Int" (Position { column: 7, index: 6, line: 1 })))
      }

  assertEqual' "tryRethrow 1"
    { actual: runParser "abx" $ char 'a' *> tryRethrow (char 'b' *> char 'c')
    , expected: Left $ ParseError "Expected 'c'" (Position { index: 1, column: 2, line: 1 })
    }

  assertEqual' "takeWhile 1"
    { actual: runParser "Tackling the Awkward" do
        takeWhile CodePoint.Unicode.isLetter <* string " the Awkward"
    , expected: Right "Tackling"
    }

  assertEqual' "takeWhile1 1"
    { actual: runParser "3ackling the Awkward" do
        takeWhile1 CodePoint.Unicode.isLetter <* string " the Awkward" <?> "letter"
    , expected: Left $ ParseError "Expected letter" (Position { index: 0, line: 1, column: 1 })
    }

  takeWhilePropagateFail

  log "\nTESTS number\n"

  -- assert' "Number.fromString" $ Just infinity == Data.Number.fromString "Infinity"
  assertEqual' "number Infinity"
    { actual: runParser "Infinity" number
    , expected: Right infinity
    }
  assertEqual' "number +Infinity"
    { actual: runParser "+Infinity" number
    , expected: Right infinity
    }
  assertEqual' "number -Infinity"
    { actual: runParser "-Infinity" number
    , expected: Right (negate infinity)
    }
  assertEqual' "number +xxx"
    { actual: lmap parseErrorPosition $ runParser "+xxx" number
    , expected: Left $ Position { index: 0, line: 1, column: 1 }
    }

  assertEqual' "number 1"
    { actual: runParser "-3.0E-1.0" number
    , expected: Right (-0.3)
    }

  assertEqual' "number xEy"
    { actual: runParser "2e1" number
    , expected: Right 20.0
    }

  -- test from issue #73
  assertEqual' "number 2"
    { actual: runParser "0.7531531167929774" number
    , expected: Right 0.7531531167929774
    }

  -- test from issue #115
  assertEqual' "number 3"
    { actual: runParser "-6.0" number
    , expected: Right (-6.0)
    }
  assertEqual' "number 4"
    { actual: runParser "+6.0" number
    , expected: Right (6.0)
    }

  assert' "number NaN 1" $ either (\_ -> false) Data.Number.isNaN (runParser (show nan) number)
  assert' "number NaN 2" $ either (\_ -> false) Data.Number.isNaN (runParser "NaN" number)

  assertEqual' "number 5"
    { actual: runParser "1..3" number
    , expected: Right (1.0)
    }

  log "\nTESTS Operator\n"
  -- test from issue #161
  -- all the below operators should play well together
  parseErrorTestMessage
    ( oneOf
        [ fail "test <?>"
        , string " " <?> "1"
        , string " " <|> string " " <?> "2"
        , string " " <?> "3" <|> string " " <?> "4"
        , "" <$ string " " <?> "5"
            <|> string " " $> "" <?> "6"
            <|> const "" <$> string " " <?> "7"
              <* string " "
              $> ""
              <?> "8"
              *> string " "
              $> ""
              <?> "9"
        , fail "test <~?>"
        , string " " <~?> \_ -> "21"
        , string " " <|> string " " <~?> \_ -> "22"
        , string " " <~?> (\_ -> "23") <|> string " " <~?> \_ -> "24"
        , "" <$ string " " <~?> (\_ -> "25")
            <|> string " " $> "" <~?> (\_ -> "26")
            <|> const "" <$> string " " <~?> (\_ -> "27")
              <* string " "
              $> ""
              <~?> (\_ -> "28")
              *> string " "
              $> ""
              <~?> \_ -> "29"
        , fail "test <??>"
        , "41" <??> string " "
        , "42" <??> string " " <|> string " "
        , "43" <??> string " " <|> "44" <??> string " "
        , "45" <??> "" <$ string " "
            <|>
              "46"
            <??> string " " $> ""
            <|>
              "47"
            <??> const "" <$> string " "
              <* ("48" <??> string " ")
              *> ("49" <??> string " ")
        ]
    )
    "no"
    "No alternative"

  -- Choice shouldn't always yield "No alternative", that's what `oneOf` is for.
  parseErrorTestMessage
    ( choice
        [ string "a"
        , string "b"
        ]
    )
    "c"
    "Expected \"b\""

  log "\nTESTS intDecimal\n"
  parseTest "-300" (-300) intDecimal

  log "\nTESTS Regex\n"
  mkRegexTest "regex-" "regex" "regex" noFlags (\regex -> regex <* char '-' <* eof)
  mkRegexTest "-regex" "regex" "regex" noFlags (\regex -> char '-' *> regex <* eof)
  mkRegexTest "regexregex" "regexregex" "(regex)*" noFlags identity
  mkRegexTest "regexregex" "regex" "(^regex)*" noFlags identity
  mkRegexTest "ReGeX" "ReGeX" "regex" ignoreCase identity
  mkRegexTest "regexcapregexcap" "regexcap" "(?<CaptureGroupName>regexcap)" noFlags identity
  mkRegexTest "regexcapregexcap" "regexcap" "(((?<CaptureGroupName>(r)e(g)excap)))" noFlags identity

  log "\nTESTS Stack Safe Loops\n"
  stackSafeLoopsTest

  log "\nTESTS Token Parser\n"

  tokenParserIdentifierTest
  tokenParserReservedTest
  tokenParserOperatorTest
  tokenParserReservedOpTest

  tokenParserCharLiteralTest
  tokenParserStringLiteralTest
  tokenParserNaturalTest
  tokenParserIntegerTest
  tokenParserFloatTest
  tokenParserNaturalOrFloatTest
  tokenParserDecimalTest
  tokenParserHexadecimalTest
  tokenParserOctalTest

  tokenParserSymbolTest
  tokenParserLexemeTest
  tokenParserWhiteSpaceTest

  tokenParserParensTest
  tokenParserBracesTest
  tokenParserAnglesTest
  tokenParserBracketsTest
  tokenParserSemiTest
  tokenParserCommaTest
  tokenParserColonTest
  tokenParserDotTest
  tokenParserSemiSepTest
  tokenParserSemiSep1Test
  tokenParserCommaSepTest
  tokenParserCommaSep1Test

  log "\nTESTS Haskell Style\n"
  haskellStyleTest
  log "\nTESTS Java Style\n"
  javaStyleTest

  log "\nTESTS region\n"
  let
    prependContext m' (ParseError m pos) = ParseError (m' <> m) pos
    p = region (prependContext "context1 ") $ do
      _ <- string "a"
      region (prependContext "context2 ") $ do
        string "b"
  case runParser "aa" p of
    Right _ -> assert' "error: ParseError expected!" false
    Left (ParseError message _) -> do
      let messageExpected = "context1 context2 Expected \"b\""
      assert' ("expected message: " <> messageExpected <> ", message: " <> message) (message == messageExpected)
      logShow messageExpected

  log "\nTESTS anyTill\n"
  parseTest "𝅘𝅥𝅮𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" (Tuple "" "𝅘𝅥𝅮") $ anyTill (string "𝅘𝅥𝅮")
  parseTest "𝅘𝅥𝅘𝅥𝅘𝅥𝅮𝅘𝅥𝅘𝅥" (Tuple "𝅘𝅥𝅘𝅥" "𝅘𝅥𝅮") $ anyTill (string "𝅘𝅥𝅮")
  parseTest "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅮" (Tuple "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" "𝅘𝅥𝅮") $ anyTill (string "𝅘𝅥𝅮") <* eof
  parseErrorTestPosition (anyTill (string "𝅘𝅥𝅮")) "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" (Position { index: 4, line: 1, column: 5 })

  log "\nTESTS Replace\n"

  assertEqual' "anyTill1"
    { actual: runParser "Baaaa" $ anyTill $ string "B"
    , expected: Right $ Tuple "" "B"
    }
  assertEqual' "anyTill2"
    { actual: runParser "aaBaa" $ anyTill $ string "B"
    , expected: Right $ Tuple "aa" "B"
    }
  assertEqual' "anyTill3"
    { actual: runParser "aaaaB" $ anyTill $ string "B"
    , expected: Right $ Tuple "aaaa" "B"
    }
  assertEqual' "breakCap1"
    { actual: breakCap "Baaaa" (string "B")
    , expected: Just $ "" /\ "B" /\ "aaaa"
    }
  assertEqual' "breakCap2"
    { actual: breakCap "aaBaa" (string "B")
    , expected: Just $ "aa" /\ "B" /\ "aa"
    }
  assertEqual' "breakCap3"
    { actual: breakCap "aaaaB" (string "B")
    , expected: Just $ "aaaa" /\ "B" /\ ""
    }
  assertEqual' "breakCap3"
    { actual: breakCap "" (string "B")
    , expected: Nothing
    }
  assertEqual' "breakCap4"
    { actual: breakCap "aaBaa" (lookAhead $ string "B")
    , expected: Just $ "aa" /\ "B" /\ "Baa"
    }
  assertEqual' "breakCap5"
    { actual: breakCap "aaBaa" (match $ string "B")
    , expected: Just $ "aa" /\ ("B" /\ "B") /\ "aa"
    }
  assertEqual' "breakCap6"
    { actual: breakCap "aaBaa" (match $ lookAhead $ string "B")
    , expected: Just $ "aa" /\ ("" /\ "B") /\ "Baa"
    }
  assertEqual' "splitCap1"
    { actual: splitCap "BaB" (string "B")
    , expected: NonEmptyList $ Right "B" :| Left "a" : Right "B" : Nil
    }
  assertEqual' "splitCap2"
    { actual: splitCap "aBaB" (string "B")
    , expected: NonEmptyList $ Left "a" :| Right "B" : Left "a" : Right "B" : Nil
    }
  assertEqual' "splitCap3"
    { actual: splitCap "aBaBa" (string "B")
    , expected: NonEmptyList $ Left "a" :| Right "B" : Left "a" : Right "B" : Left "a" : Nil
    }
  assertEqual' "splitCap4"
    { actual: splitCap "a" (string "B")
    , expected: NonEmptyList $ Left "a" :| Nil
    }
  assertEqual' "splitCap5"
    { actual: splitCap "" (string "B")
    , expected: NonEmptyList $ Left "" :| Nil
    }
  assertEqual' "splitCap6"
    { actual: splitCap "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" (string "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯")
    , expected: NonEmptyList $ Left "𝄞𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" :| Right "𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯𝅘𝅥𝅯" : Left "𝅘𝅥𝅘𝅥𝅘𝅥𝅘𝅥" : Nil
    }
  assertEqual' "splitCap7"
    { actual: splitCap "aa" (lookAhead $ string "a" *> pure unit)
    , expected: NonEmptyList $ Right unit :| Left "a" : Right unit : Left "a" : Nil
    }
  assertEqual' "splitCap8B"
    { actual: splitCap "BaBa" (lookAhead $ string "B" *> pure unit)
    , expected: NonEmptyList $ Right unit :| Left "Ba" : Right unit : Left "Ba" : Nil
    }
  assertEqual' "splitCap9" $
    { actual: splitCap "aBaBa" (match $ lookAhead $ string "B" *> pure unit)
    , expected: NonEmptyList $ Left "a" :| Right (Tuple "" unit) : Left "Ba" : Right (Tuple "" unit) : Left "Ba" : Nil
    }
  assertEqual' "splitCap10"
    { actual: splitCap "abc" (pure unit)
    , expected: NonEmptyList $ Right unit :| Left "a" : Right unit : Left "b" : Right unit : Left "c" : Right unit : Nil
    }
  assertEqual' "splitCap11"
    { actual: splitCap "abc" consume
    , expected: NonEmptyList $ Right unit :| Left "a" : Right unit : Left "b" : Right unit : Left "c" : Right unit : Nil
    }
  assertEqual' "replace2"
    { actual: replace "aBd" (string "B" *> pure "C")
    , expected: "aCd"
    }
  assertEqual' "replace3"
    { actual: replace "abcd" (toUpper <$> takeN 1)
    , expected: "ABCD"
    }
  assertEqual' "replace4"
    { actual: replace "abc" (pure "X")
    , expected: "XaXbXcX"
    }
  assertEqual' "String.Replace example0"
    { actual: breakCap "abc 123 def" (match intDecimal)
    , expected: Just $ "abc " /\ ("123" /\ 123) /\ " def"
    }
  assertEqual' "String.Replace example1"
    { actual: map get2 $ breakCap "..A.." (position <* string "A")
    , expected: Just (Position { index: 2, line: 1, column: 3 })
    }
  assertEqual' "String.Replace example2"
    { actual: catMaybes $ hush <$> splitCap ".A...\n...A." (position <* string "A")
    , expected: (Position { index: 1, line: 1, column: 2 }) : (Position { index: 9, line: 2, column: 4 } : Nil)
    }
  assertEqual' "String.Replace example3'"
    { actual: unsafePerformEffect $ replaceT "◀ {HOME} ▶" do
        _ <- string "{"
        Tuple home _ <- anyTill (string "}")
        lift (lookupEnv home) >>= maybe empty pure
    , expected: "◀ " <> unsafePartial (fromJust (unsafePerformEffect (lookupEnv "HOME"))) <> " ▶"
    }
  assertEqual' "String.Replace example4'"
    { actual: replace "1 6 21 107" (show <$> (_ * 2) <$> intDecimal)
    , expected: "2 12 42 214"
    }
  assertEqual' "String.Replace example4"
    { actual:
        let
          letterCount :: ParserT String (State Int) (Tuple Char Int)
          letterCount = do
            l <- letter
            i <- modify (_ + 1)
            pure $ l /\ i
        in
          flip runState 0 $ splitCapT "A B" letterCount
    , expected: (NonEmptyList $ Right ('A' /\ 1) :| Left " " : Right ('B' /\ 2) : Nil) /\ 2
    }
  assertEqual' "String.Replace example5"
    { actual:
        let
          balancedParens :: Parser String Unit
          balancedParens = do
            void $ char '('
            void $ manyTill (balancedParens <|> void anyCodePoint) (char ')')
        in
          rmap fst <$> splitCap "((🌼)) (()())" (match balancedParens)
    , expected: NonEmptyList $ Right "((🌼))" :| Left " " : Right "(()())" : Nil
    }
  assertEqual' "String.Replace example6"
    { actual: replace "hay needle hay" (toUpper <$> string "needle")
    , expected: "hay NEEDLE hay"
    }

  log "\nTESTS manyIndex\n"

  assertEqual' "manyIndex 1"
    { actual: runParser "aaab" $ manyIndex 0 3 (\_ -> char 'a')
    , expected: Right (Tuple 3 ('a' : 'a' : 'a' : Nil))
    }
  assertEqual' "manyIndex 2"
    { actual: runParser "aaaa" $ manyIndex 0 3 (\_ -> char 'a')
    , expected: Right (Tuple 3 ('a' : 'a' : 'a' : Nil))
    }
  assertEqual' "manyIndex 3"
    { actual: runParser "b" $ manyIndex 0 3 (\_ -> char 'a')
    , expected: Right (Tuple 0 (Nil))
    }
  assertEqual' "manyIndex 4"
    { actual: lmap parseErrorPosition $ runParser "ab" $ manyIndex 3 3 (\_ -> char 'a')
    , expected: Left (Position { index: 1, line: 1, column: 2 })
    }
  assertEqual' "manyIndex 5"
    { actual: runParser "aaa" $ manyIndex (-2) (1) (\_ -> char 'a')
    , expected: Right (Tuple 0 (Nil))
    }
  assertEqual' "manyIndex 6 errors"
    { actual: lmap parseErrorPosition $ runParser "aab" $ map snd $ manyIndex 3 3 (\_ -> char 'a')
    , expected: lmap parseErrorPosition $ runParser "aab" $ (replicateA 3 (char 'a') :: Parser String (List Char))
    }

  log "\nTESTS advance\n"

  assertEqual' "advance 1"
    { actual: runParser "aa" $ advance $ char 'a'
    , expected: Right 'a'
    }
  assertEqual' "advance 2"
    { actual: lmap parseErrorPosition $ runParser "aa" $ advance consume
    , expected: Left (Position { index: 0, line: 1, column: 1 })
    }

  log "\nTESTS error messages\n"
  do
    let input = "12345six789"
    assertEqual' "parseErrorHuman 1"
      { actual: Array.drop 1 $ parseErrorHuman input 20 $ fromLeft (ParseError "" initialPos)
          $ runParser input (replicateA 9 String.Basic.digit :: Parser String (List Char))
      , expected: [ "     ▼", "12345six789" ]
      }

  do
    let input = "12345six789"
    assertEqual' "parseErrorHuman 2"
      { actual: Array.drop 1 $ parseErrorHuman input 5 $ fromLeft (ParseError "" initialPos)
          $ runParser input (replicateA 9 String.Basic.digit :: Parser String (List Char))
      , expected: [ "  ▼", "45six" ]
      }

  do
    let input = "aaaa🍷\r\nbbbb"
    assertEqual' "parseErrorHuman 3"
      { actual: parseErrorHuman input 20 $ fromLeft (ParseError "" initialPos)
          $ runParser input
          $ string "aaaa" *> (replicateA 7 letter :: Parser String (List Char))
      , expected: [ "Expected letter at position index:4 (line:1, column:5)", "    ▼", "aaaa🍷" ]
      }

  do
    let input = "aaaa\r\n🍷bbbb"
    assertEqual' "parseErrorHuman 4"
      { actual: parseErrorHuman input 20 $ fromLeft (ParseError "" initialPos)
          $ runParser input
          $ string "aaaa\r\n" *> (replicateA 5 letter :: Parser String (List Char))
      , expected: [ "Expected letter at position index:6 (line:2, column:1)", "▼", "🍷bbbb" ]
      }

  do
    let input = "aCaB"
    assertEqual' "parseErrorHuman ayebee"
      { actual: parseErrorHuman input 20 $ fromLeft (ParseError "" initialPos)
          $ runParser input do
              _ <- char 'a'
              b <- char 'b' <|> char 'B'
              pure (b == 'B')
      , expected: [ "Expected 'B' at position index:1 (line:1, column:2)", " ▼", input ]
      }

  log "\nTESTS recursion"

  do
    let
      aye :: Parser String Char
      aye = defer \_ -> char 'a' *> (aye <|> pure 'e')
    assertEqual' "recusion aye"
      { actual: runParser "aaa" aye
      , expected: Right 'e'
      }

  do
    let
      aye :: Parser String (List Char)
      aye = defer \_ -> List.Cons <$> char 'a' <*> (aye <|> bee <|> pure List.Nil)

      bee :: Parser String (List Char)
      bee = defer \_ -> List.Cons <$> char 'b' <*> (aye <|> bee <|> pure List.Nil)
    assertEqual' "mutual recusion aye bee"
      { actual: runParser "aabbaa" aye
      , expected: Right ('a' : 'a' : 'b' : 'b' : 'a' : 'a' : List.Nil)
      }

  do
    log "\nTESTS Combinators many"
    -- https://github.com/purescript-contrib/purescript-parsing/issues/234

    let
      failAfterConsuming :: forall a. Parser String a
      failAfterConsuming = anyChar *> fail "failure"

    parseErrorTestPosition
      (Combinators.Array.many (char 'a' <|> failAfterConsuming))
      "abc"
      (Position { index: 2, line: 1, column: 3 })

    parseErrorTestPosition
      (Combinators.Array.many1 (char 'a' <|> failAfterConsuming))
      "abc"
      (Position { index: 2, line: 1, column: 3 })

    parseErrorTestPosition
      (Combinators.many (char 'a' <|> failAfterConsuming))
      "abc"
      (Position { index: 2, line: 1, column: 3 })

    parseErrorTestPosition
      (Combinators.many (char 'a' <|> failAfterConsuming))
      "abc"
      (Position { index: 2, line: 1, column: 3 })
