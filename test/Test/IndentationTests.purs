module Test.IndentationTests where

import Parsing.Indent (IndentParser, checkIndent, indentParens, indented, runIndent, withPos)
import Prelude (class Eq, class Show, Unit, bind, discard, pure, void, ($), (*>), (<$>), (<*), (<<<))
import Test.Lib (class ParseErrorHuman__OnlyString, TestM, mkParseErrorTestMessage, mkParseErrorTestPosition, mkParseErrorTestPositionAndMessage, mkParseTest)

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Array.NonEmpty as NonEmpty
import Data.Generic.Rep (class Generic)
import Data.List (fromFoldable)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.String.CodeUnits (fromCharArray)
import Effect (Effect)
import Parsing (Position(..), runParserT)
import Parsing.Combinators (many, sepBy)
import Parsing.Combinators.Array as Combinators.Array
import Parsing.Indent as Indent
import Parsing.String (char, eof, string)
import Parsing.String.Basic (alphaNum, intDecimal, skipSpaces)

parseTest :: forall s a. Show a => Eq a => ParseErrorHuman__OnlyString s => s -> a -> IndentParser s a -> Effect Unit
parseTest = mkParseTest (\input -> runIndent <<< runParserT input)

parseErrorTestPosition :: forall s a. Show a => IndentParser s a -> s -> Position -> Effect Unit
parseErrorTestPosition = mkParseErrorTestPosition (\input -> runIndent <<< runParserT input)

parseErrorTestMessage :: forall s a. Show a => IndentParser s a -> s -> String -> Effect Unit
parseErrorTestMessage = mkParseErrorTestMessage (\input -> runIndent <<< runParserT input)

parseErrorTestPositionAndMessage :: forall s a. Show a => IndentParser s a -> s -> String -> Position -> Effect Unit
parseErrorTestPositionAndMessage = mkParseErrorTestPositionAndMessage (\input -> runIndent <<< runParserT input)

-- test from https://github.com/jaspervdj/indents/blob/f9e10707c1fe91d7c1aa2dbaf9fb107e146255da/tests/Text/Parsec/Indent/Tests.hs#L12
data Taxonomy = Taxonomy String (Array Taxonomy)

derive instance Generic Taxonomy _
derive instance Eq Taxonomy
instance Show Taxonomy where
  show x = genericShow x

-- | NOTE: skipSpaces will skip "        \n  " in
-- | ```purs
-- | [ "k        \n"
-- | , "  a1     \n"
-- | ...
-- | ```
pTerm :: IndentParser String String
pTerm = ((fromCharArray <$> NonEmpty.toArray) <$> Combinators.Array.many1 alphaNum) <* skipSpaces

pTaxonomy :: IndentParser String Taxonomy
pTaxonomy = withPos $ do
  term <- pTerm
  subs <- many $ Indent.indented *> pTaxonomy
  pure $ Taxonomy term (Array.fromFoldable subs)

testIndentationParser :: TestM
testIndentationParser = do
  parseTest
    ( joinWith "\n"
        [ "k        \n"
        , "  a1     \n"
        , "  a2     \n"
        , "   b1    \n"
        , "  a3     \n"
        , "   haha  \n"
        , "   it    \n"
        , "   works \n"
        , "    "
        ]
    )
    ( Taxonomy "k"
        [ Taxonomy "a1" []
        , Taxonomy "a2"
            [ Taxonomy "b1" []
            ]
        , Taxonomy "a3"
            [ Taxonomy "haha" []
            , Taxonomy "it" []
            , Taxonomy "works" []
            ]
        ]
    )
    (pTaxonomy <* eof)

  -- Testing function 'indented'
  parseTest "111\n 222\n  333" { x: 111, y: 222, z: 333 } do
    x <- intDecimal
    void $ string "\n "
    indented *> withPos do
      checkIndent
      y <- intDecimal
      void $ string "\n  "
      indented *> withPos do
        checkIndent
        z <- intDecimal
        eof
        pure { x, y, z }

  parseErrorTestPositionAndMessage
    ( do
        x <- intDecimal
        void $ string "\n  "
        indented *> withPos do
          checkIndent -- This should pass.
          y <- intDecimal
          void $ string "\n"
          indented *> withPos do -- indented should fail
            z <- intDecimal
            eof
            pure { x, y, z }
    )
    "111\n  222\n333"
    """not indented"""
    (Position { column: 1, index: 10, line: 3 })

  -- Testing function 'indentParens'
  parseTest "(1,2,3)" (fromFoldable [ "1", "2", "3" ])
    $ indentParens
    $ sepBy (string "1" <|> string "2" <|> string "3") (char ',')

  -- doesnt work, dont understand idea of this method
  -- parseTest """(\n  1,\n  2,\n  3)""" (fromFoldable ["1", "2", "3"]) $
  --   indentParens $ sepBy (string "1" <|> string "2" <|> string "3") (char ',')
  --
  -- parseTest """(1,\n  2,\n  3)""" (fromFoldable ["1", "2", "3"]) $
  --   indentParens $ sepBy (string "1" <|> string "2" <|> string "3") (char ',')

  parseErrorTestPositionAndMessage
    (indentParens $ sepBy (string "1" <|> string "2" <|> string "3") (char ','))
    "(1, 2,3)"
    """Expected "3""""
    (Position { column: 4, index: 3, line: 1 })

