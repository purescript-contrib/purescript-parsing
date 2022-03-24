module Test.Main where

import Prelude hiding (between, when)

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (some, toUnfoldable)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (oneOf)
import Data.List (List(..), fromFoldable, many)
import Data.List.NonEmpty (cons, cons')
import Data.List.NonEmpty as NE
import Data.Maybe (Maybe(..), fromJust)
import Data.Number (infinity, isNaN)
import Data.String.CodePoints as SCP
import Data.String.CodeUnits (fromCharArray, singleton)
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (logShow)
import Partial.Unsafe (unsafePartial)
import Test.Assert (assert')
import Text.Parsing.Parser (ParseError(..), Parser, ParserT, fail, parseErrorMessage, parseErrorPosition, position, region, runParser)
import Text.Parsing.Parser.Combinators (between, chainl, chainl1Rec, chainlRec, chainr1Rec, chainrRec, endBy1, endBy1Rec, endByRec, many1Rec, many1TillRec, many1TillRec_, many1Till_, manyTillRec, manyTillRec_, manyTill_, notFollowedBy, optionMaybe, sepBy1, sepBy1Rec, sepByRec, sepEndBy1Rec, sepEndByRec, skipMany1Rec, skipManyRec, try, (<?>), (<~?>), (<??>))
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.Language (haskellDef, haskellStyle, javaStyle)
import Text.Parsing.Parser.Pos (Position(..), initialPos)
import Text.Parsing.Parser.String (anyChar, anyCodePoint, char, eof, noneOfCodePoints, oneOfCodePoints, regex, rest, satisfy, string, takeN, whiteSpace)
import Text.Parsing.Parser.String.Basic (intDecimal, number, letter)
import Text.Parsing.Parser.Token (TokenParser, makeTokenParser, match, token, when)
import Text.Parsing.Parser.Token as Parser.Token

parens :: forall m a. Monad m => ParserT String m a -> ParserT String m a
parens = between (string "(") (string ")")

nested :: forall m. Monad m => ParserT String m Int
nested = fix \p ->
  ( do
      _ <- string "a"
      pure 0
  ) <|> ((+) 1) <$> parens p

parseTest :: forall s a. Show a => Eq a => s -> a -> Parser s a -> Effect Unit
parseTest input expected p = case runParser input p of
  Right actual -> do
    assert' ("expected: " <> show expected <> ", actual: " <> show actual) (expected == actual)
    logShow actual
  Left err -> assert' ("error: " <> show err) false

parseErrorTestPosition :: forall s a. Show a => Parser s a -> s -> Position -> Effect Unit
parseErrorTestPosition p input expected = case runParser input p of
  Right x -> assert' ("ParseError expected at " <> show expected <> " but parsed " <> show x) false
  Left err -> do
    let pos = parseErrorPosition err
    assert' ("expected: " <> show expected <> ", pos: " <> show pos) (expected == pos)
    logShow expected

parseErrorTestMessage :: forall s a. Show a => Parser s a -> s -> String -> Effect Unit
parseErrorTestMessage p input expected = case runParser input p of
  Right x -> assert' ("ParseError expected '" <> expected <> "' but parsed " <> show x) false
  Left err -> do
    let msg = parseErrorMessage err
    assert' ("expected: " <> expected <> ", message: " <> msg) (expected == msg)
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
exprTest = buildExprParser
  [ [ Infix (string "/" >>= \_ -> pure (/)) AssocRight ]
  , [ Infix (string "*" >>= \_ -> pure (*)) AssocRight ]
  , [ Infix (string "-" >>= \_ -> pure (-)) AssocRight ]
  , [ Infix (string "+" >>= \_ -> pure (+)) AssocRight ]
  ]
  digit

manySatisfyTest :: Parser String String
manySatisfyTest = do
  r <- some $ satisfy (\s -> s /= '?')
  _ <- char '?'
  pure (fromCharArray r)

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
    manyTillRec (string "a") (string "b")
  parseTest "baa" Nil $
    manyTillRec (string "a") (string "b")

  parseTest "aaabaa" (NE.cons' "a" $ toUnfoldable [ "a", "a" ]) $
    many1TillRec (string "a") (string "b")
  parseErrorTestPosition
    (many1TillRec (string "a") (string "b"))
    "baa"
    (Position { line: 1, column: 1 })

  parseTest "a,a,a,b,a,a" (toUnfoldable [ "a", "a", "a" ]) $
    sepEndByRec (string "a") (string ",")
  parseTest "a,a,abaa" (toUnfoldable [ "a", "a", "a" ]) $
    sepEndByRec (string "a") (string ",")
  parseTest "b,a,a" Nil $
    sepEndByRec (string "a") (string ",")

  parseTest "a,a,a,b,a,a" (NE.cons' "a" $ toUnfoldable [ "a", "a" ]) $
    sepEndBy1Rec (string "a") (string ",")
  parseTest "a,a,abaa" (NE.cons' "a" $ toUnfoldable [ "a", "a" ]) $
    sepEndBy1Rec (string "a") (string ",")
  parseErrorTestPosition
    (sepEndBy1Rec (string "a") (string ","))
    "b,a,a"
    (Position { line: 1, column: 1 })

  -- 8 `div` (8 `div` 2) == 2
  parseTest "8x8x2" 2 $
    chainrRec digit (string "x" $> div) 42
  parseTest "" 42 $
    chainrRec digit (string "x" $> div) 42
  parseTest "8x8x2" 2 $
    chainr1Rec digit (string "x" $> div)
  parseErrorTestPosition
    (chainr1Rec digit (string "x" $> div))
    ""
    (Position { line: 1, column: 1 })

  -- (8 `div` 2) `div` 2 == 2
  parseTest "8x2x2" 2 $
    chainlRec digit (string "x" $> div) 42
  parseTest "" 42 $
    chainlRec digit (string "x" $> div) 42
  parseTest "8x2x2" 2 $
    chainl1Rec digit (string "x" $> div)
  parseErrorTestPosition
    (chainl1Rec digit (string "x" $> div))
    ""
    (Position { line: 1, column: 1 })

  parseTest "aaaabcd" "b"
    $ skipMany1Rec (string "a")
    *> string "b"
  parseErrorTestPosition
    (skipMany1Rec (string "a"))
    "bcd"
    (Position { line: 1, column: 1 })

  parseTest "aaaabcd" "b"
    $ skipManyRec (string "a")
    *> string "b"
  parseTest "bcd" "b"
    $ skipManyRec (string "a")
    *> string "b"

  parseTest "aaa" (NE.cons' "a" $ toUnfoldable [ "a", "a" ]) $
    many1Rec (string "a")
  parseErrorTestPosition
    (many1Rec (string "a"))
    ""
    (Position { line: 1, column: 1 })

  parseTest "a,a,ab" (toUnfoldable [ "a", "a", "a" ])
    $ sepByRec (string "a") (string ",")
    <* string "b"
  parseTest "b" Nil
    $ sepByRec (string "a") (string ",")
    <* string "b"
  parseTest "a,a,ab" (NE.cons' "a" $ toUnfoldable [ "a", "a" ])
    $ sepBy1Rec (string "a") (string ",")
    <* string "b"
  parseErrorTestPosition
    (sepBy1Rec (string "a") (string ","))
    ""
    (Position { line: 1, column: 1 })
  parseErrorTestPosition
    (sepBy1Rec (string "a") (string ","))
    "a,"
    (Position { line: 1, column: 3 })

  parseTest "a,a,a,b" (toUnfoldable [ "a", "a", "a" ])
    $ endByRec (string "a") (string ",")
    <* string "b"
  parseTest "b" Nil
    $ endByRec (string "a") (string ",")
    <* string "b"
  parseTest "a,a,a,b" (NE.cons' "a" $ toUnfoldable [ "a", "a" ])
    $ endBy1Rec (string "a") (string ",")
    <* string "b"
  parseErrorTestPosition
    (endBy1Rec (string "a") (string ","))
    ""
    (Position { line: 1, column: 1 })
  parseErrorTestPosition
    (endBy1Rec (string "a") (string ","))
    "a,a"
    (Position { line: 1, column: 4 })

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
  parseTest "a,a,a" (cons "a" (cons "a" (cons' "a" Nil))) $ string "a" `sepBy1` string ","
  parseTest "a,a,a," (cons "a" (cons "a" (cons' "a" Nil))) $ do
    as <- string "a" `endBy1` string ","
    eof
    pure as
  parseTest "a+b+c" "abc" opTest
  parseTest "1*2+3/4-5" (-3) exprTest
  parseTest "ab?" "ab" manySatisfyTest

  parseTest "ab" unit (char 'a' *> notFollowedBy (char 'a'))

  parseTest "rest" "rest" rest
  parseTest "rest" unit (rest *> eof)
  parseTest "rest\nrest" (Position { line: 2, column: 5 }) (rest *> position)

  parseErrorTestPosition
    (rest *> notFollowedBy eof)
    "aa\naa"
    (Position { column: 3, line: 2 })

  parseErrorTestPosition
    anyChar
    "𝅘𝅥𝅯"
    (Position { column: 1, line: 1 })

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
  parseErrorTestPosition (takeN 10) "abcd" (Position { column: 1, line: 1 })
  parseErrorTestPosition (takeN (-1)) "abcd" (Position { column: 1, line: 1 })

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

  parseTest (fromFoldable [ A ]) A (match tokpos A)
  parseTest (fromFoldable [ B ]) B (match tokpos B)
  parseTest (fromFoldable [ A, B ]) A (match tokpos A)

  parseTest (fromFoldable []) unit Parser.Token.eof

  parseTest "aabb" (Tuple (fromFoldable [ 'a', 'a' ]) 'b') (manyTill_ (char 'a') (char 'b'))
  parseTest "aabb" (Tuple (unsafePartial $ fromJust (NE.fromFoldable [ 'a', 'a' ])) 'b') (many1Till_ (char 'a') (char 'b'))
  parseTest "aabb" (Tuple (fromFoldable [ 'a', 'a' ]) 'b') (manyTillRec_ (char 'a') (char 'b'))
  parseTest "aabb" (Tuple (unsafePartial $ fromJust (NE.fromFoldable [ 'a', 'a' ])) 'b') (many1TillRec_ (char 'a') (char 'b'))

  parseTest "aab" (Tuple (fromFoldable [ 'a', 'a' ]) 'b') do
    Tuple a b <- manyTill_ letter do
      char 'b'
    pure (Tuple a b)

  parseTest "ababab" [ 'b', 'b', 'b' ] $ Array.many (char 'a' *> char 'b')
  parseTest "abaXab" [ 'b' ] $ Array.many (try (char 'a' *> char 'b'))

  parseErrorTestPosition (string "abc") "bcd" (Position { column: 1, line: 1 })
  parseErrorTestPosition (string "abc" *> eof) "abcdefg" (Position { column: 4, line: 1 })
  parseErrorTestPosition (string "a\nb\nc\n" *> eof) "a\nb\nc\nd\n" (Position { column: 1, line: 4 })
  parseErrorTestPosition (string "\ta" *> eof) "\tab" (Position { column: 10, line: 1 })

  parseTest "Infinity" infinity number
  parseTest "+Infinity" infinity number
  parseTest "-Infinity" (negate infinity) number
  parseErrorTestPosition number "+xxx" (mkPos 2)

  parseTest "-3.0E-1.0" (-0.3) number

  -- test from issue #73
  parseTest "0.7531531167929774" 0.7531531167929774 number

  -- test from issue #115
  parseTest "-6.0" (-6.0) number
  parseTest "+6.0" (6.0) number

  -- test from issue #161
  -- all the below operators should play well together
  parseErrorTestMessage
    ( oneOf
        [ fail "test <?>"
        , string " " <?> "1"
        , string " " <|> string " " <?> "2"
        , string " " <?> "3" <|> string " " <?> "4"
        , "" <$ string " "
            <?> "5"
              <|> string " "
              $> ""
            <?> "6"
              <|> const ""
              <$> string " "
            <?> "7"
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
        , "" <$ string " "
            <~?> (\_ -> "25")
              <|> string " "
              $> ""
            <~?> (\_ -> "26")
              <|> const ""
              <$> string " "
            <~?> (\_ -> "27")
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
        , "45"
            <??> "" <$ string " "
              <|> "46"
            <??> string " " $> ""
              <|> "47"
            <??> const "" <$> string " "
              <* ("48" <??> string " ")
              *> ("49" <??> string " ")
        ]
    )
    "no"
    "No alternative"

  -- we can't test "NaN" with `parseTest` because nan doesn't compare equal
  case runParser "NaN" number of
    Right actual -> do
      assert' ("expected: NaN, actual: " <> show actual) (isNaN actual)
      logShow actual
    Left err -> assert' ("error: " <> show err) false

  -- TODO This shows the current limitations of the number parser. Ideally this parse should fail.
  parseTest "1..3" 1.0 $ number <* eof

  parseTest "-300" (-300) intDecimal

  parseTest "regex-" "regex" (regex {} "regex" <* char '-' <* eof)
  parseTest "-regex" "regex" (char '-' *> regex {} "regex" <* eof)
  parseTest "regexregex" "regexregex" (regex {} "(regex)*")
  parseTest "regexregex" "regex" (regex {} "(^regex)*")
  parseTest "ReGeX" "ReGeX" (regex { ignoreCase: true } "regex")
  parseTest "regexcapregexcap" "regexcap" (regex {} "(?<CaptureGroupName>regexcap)")
  parseTest "regexcapregexcap" "regexcap" (regex {} "(((?<CaptureGroupName>(r)e(g)excap)))")

  -- Maybe it is nonsense to allow multiline regex.
  -- Because an end-of-line regex pattern `$` will match but then the
  -- newline character will not be consumed.
  -- Also why does this test fail? I think it should succeed.
  -- parseTest "regex\nregex\n" "regex\nregex\n" (regex {dotAll: false, multiline: true} "(^regex$)+")

  stackSafeLoopsTest

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
    Left (ParseError message _) -> do
      let messageExpected = "context1 context2 Expected \"b\""
      assert' ("expected message: " <> messageExpected <> ", message: " <> message) (message == messageExpected)
      logShow messageExpected
  where
  prependContext m' (ParseError m pos) = ParseError (m' <> m) pos
  p = region (prependContext "context1 ") $ do
    _ <- string "a"
    region (prependContext "context2 ") $ do
      string "b"
