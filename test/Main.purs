module Test.Main where

import Prelude hiding (between,when)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (some)
import Data.Either (Either(..))
import Data.List (List(..), fromFoldable, many)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Test.Assert (assert')
import Text.Parsing.Parser (Parser, ParserT, ParseError(..), runParser, parseErrorPosition, label)
import Text.Parsing.Parser.Combinators (endBy1, sepBy1, optionMaybe, try, chainl, between)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (javaStyle, haskellStyle, haskellDef)
import Text.Parsing.Parser.Pos (Position(..), initialPos)
import Text.Parsing.Parser.String (eof, string, char, satisfy, anyChar)
import Text.Parsing.Parser.Token (TokenParser, match, when, token, makeTokenParser)

parens :: forall m a. Monad m => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

nested :: forall m. Monad m => ParserT String m Int
nested = fix \p -> (do
  _ <- string "a"
  pure 0) <|> ((+) 1) <$> parens p

parseTest :: forall s a. Show a => Eq a => s -> a -> Parser s a -> Effect Unit
parseTest input expected p = case runParser input p of
  Right actual -> do
    assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
    logShow actual
  Left err -> assert' ("error: " <> show err) false

parseErrorTestPosition :: forall s a. Show a => Parser s a -> s -> Position -> Effect Unit
parseErrorTestPosition p input expected = case runParser input p of
  Right _ -> assert' "error: ParseError expected!" false
  Left err -> do
    let pos = parseErrorPosition err
    assert' ("expected: " <> show expected <> ", pos: " <> show pos) (expected == pos)
    logShow expected

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
exprTest = buildExprParser [ [ Infix (string "/" >>= \_ -> pure (/)) AssocRight ]
                           , [ Infix (string "*" >>= \_ -> pure (*)) AssocRight ]
                           , [ Infix (string "-" >>= \_ -> pure (-)) AssocRight ]
                           , [ Infix (string "+" >>= \_ -> pure (+)) AssocRight ]
                           ] digit


manySatisfyTest :: Parser String String
manySatisfyTest = do
  r <- some $ satisfy (\s -> s /= '?')
  _ <- char '?'
  pure (fromCharArray r)

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
mkPos n = mkPos' n 1

mkPos' :: Int -> Int -> Position
mkPos' column line = Position { column: column, line: line }

type TestM = Effect Unit

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

    -- parse float with exponent
    parseTest "100e1" 1000.0 testTokenParser.float

    -- parse float with exponent
    parseTest "100.5e1" 1005.0 testTokenParser.float

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
    parseTest "foo; foo" (fromFoldable ["foo", "foo"]) $ testTokenParser.semiSep $ string "foo"

    -- parse semi sep with newline
    parseTest "foo; \nfoo" (fromFoldable ["foo", "foo"]) $ testTokenParser.semiSep $ string "foo"

    -- parse no semi sep
    parseTest "" (fromFoldable []) $ testTokenParser.semiSep $ string "foo"
    -- parseErrorTestPosition testTokenParser.operator "foo" $ mkPos 1

tokenParserSemiSep1Test :: TestM
tokenParserSemiSep1Test = do
    -- parse semi sep1
    parseTest "foo; foo" (fromFoldable ["foo", "foo"]) $ testTokenParser.semiSep1 $ string "foo"

    -- parse semi sep1 with newline
    parseTest "foo; \nfoo" (fromFoldable ["foo", "foo"]) $ testTokenParser.semiSep1 $ string "foo"

    -- no parse on empty string
    parseErrorTestPosition (testTokenParser.semiSep1 $ string "foo") "" $ mkPos 1

tokenParserCommaSepTest :: TestM
tokenParserCommaSepTest = do
    -- parse comma sep
    parseTest "foo, foo" (fromFoldable ["foo", "foo"]) $ testTokenParser.commaSep $ string "foo"

    -- parse comma sep with newline
    parseTest "foo, \nfoo" (fromFoldable ["foo", "foo"]) $ testTokenParser.commaSep $ string "foo"

    -- parse no comma sep
    parseTest "" (fromFoldable []) $ testTokenParser.commaSep $ string "foo"

tokenParserCommaSep1Test :: TestM
tokenParserCommaSep1Test = do
    -- parse comma sep1
    parseTest "foo, foo" (fromFoldable ["foo", "foo"]) $ testTokenParser.commaSep1 $ string "foo"

    -- parse comma sep1 with newline
    parseTest "foo, \nfoo" (fromFoldable ["foo", "foo"]) $ testTokenParser.commaSep1 $ string "foo"

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

main :: Effect Unit
main = do

  parseErrorTestPosition
    (many $ char 'f' *> char '?')
    "foo"
    (Position { column: 2, line: 1 })

  parseErrorTestPosition
    (satisfy (_ == '?'))
    "foo"
    (Position { column: 1, line: 1 })

  parseTest
    "foo"
    Nil
    (many $ try $ char 'f' *> char '?')

  parseTest "(((a)))" 3 nested
  parseTest "aaa" (Cons "a" (Cons "a" (Cons "a" Nil))) $ many (string "a")
  parseTest "(ab)" (Just "b") $ parens do
    _ <- string "a"
    optionMaybe $ string "b"
  parseTest "a,a,a" (Cons "a" (Cons "a" (Cons "a" Nil))) $ string "a" `sepBy1` string ","
  parseTest "a,a,a," (Cons "a" (Cons "a" (Cons "a" Nil))) $ do
    as <- string "a" `endBy1` string ","
    eof
    pure as
  parseTest "a+b+c" "abc" opTest
  parseTest "1*2+3/4-5" (-3) exprTest
  parseTest "ab?" "ab" manySatisfyTest

  let tokpos = const initialPos
  parseTest (fromFoldable [A, B]) A (token tokpos)
  parseTest (fromFoldable [B, A]) B (token tokpos)

  parseTest (fromFoldable [A, B]) A (when tokpos isA)

  parseTest (fromFoldable [A]) A (match tokpos A)
  parseTest (fromFoldable [B]) B (match tokpos B)
  parseTest (fromFoldable [A, B]) A (match tokpos A)

  parseErrorTestPosition (string "abc") "bcd" (Position { column: 1, line: 1 })
  parseErrorTestPosition (string "abc" *> eof) "abcdefg" (Position { column: 4, line: 1 })
  parseErrorTestPosition (string "a\nb\nc\n" *> eof) "a\nb\nc\nd\n" (Position { column: 1, line: 4 })
  parseErrorTestPosition (string "\ta" *> eof) "\tab" (Position { column: 10, line: 1 })

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

  haskellStyleTest
  javaStyleTest

  case runParser "aa" p of
    Right _ -> assert' "error: ParseError expected!" false
    Left (ParseError message pos) -> do
      let messageExpected = "context1context2Expected \"b\""
      assert' ("expected message: " <> messageExpected <> ", message: " <> message) (message == messageExpected)
      logShow messageExpected
   where
    p = label "context1" $ do
          _ <- string "a"
          label "context2" $ do
            string "b"
