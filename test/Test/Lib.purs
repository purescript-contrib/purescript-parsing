module Test.Lib where

import Prelude hiding (between, when)

import Data.Either (Either(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Console (logShow)
import Parsing (ParseError, ParserT, Position, eqPositionFull, parseErrorMessage, parseErrorPosition)
import Parsing.String (parseErrorHuman)
import Test.Assert (assert')

type TestM = Effect Unit

-- For pretty printing error if input is string, but print nothing if input is not string
class ParseErrorHuman__OnlyString s where
  parseErrorHuman__onlyString :: s -> Int -> ParseError -> Array String

instance ParseErrorHuman__OnlyString String where
  parseErrorHuman__onlyString = parseErrorHuman
else instance ParseErrorHuman__OnlyString a where
  parseErrorHuman__onlyString _ _ _ = []

-- | Generalized version of `mkParseTest` that works with any input type
mkParseTest :: forall s a m. Show a => Eq a => ParseErrorHuman__OnlyString s => (s -> ParserT s m a -> Either ParseError a) -> s -> a -> ParserT s m a -> Effect Unit
mkParseTest runParser input expected p = case runParser input p of
  Right actual -> do
    assert' ("expected output: " <> show expected <> ", actual output: " <> show actual) (expected == actual)
    logShow actual
  Left err -> assert' (joinWith "\n" $ [ "error: " <> show err ] <> parseErrorHuman__onlyString input 60 err) false

mkParseErrorTestPosition :: forall s a m. Show a => (s -> ParserT s m a -> Either ParseError a) -> ParserT s m a -> s -> Position -> Effect Unit
mkParseErrorTestPosition runParser p input expected = case runParser input p of
  Right x -> assert' ("ParseError expected at " <> show expected <> " but parsed " <> show x) false
  Left err -> do
    let pos = parseErrorPosition err
    assert' ("expected position: " <> show expected <> ", actual position: " <> show pos) (eqPositionFull expected pos)
    logShow expected

mkParseErrorTestMessage :: forall s a m. Show a => (s -> ParserT s m a -> Either ParseError a) -> ParserT s m a -> s -> String -> Effect Unit
mkParseErrorTestMessage runParser p input expected = case runParser input p of
  Right x -> assert' ("ParseError expected '" <> expected <> "' but parsed " <> show x) false
  Left err -> do
    let msg = parseErrorMessage err
    assert' ("expected message: " <> expected <> ", actual message: " <> msg) (expected == msg)
    logShow expected

mkParseErrorTestPositionAndMessage :: forall s a m. Show a => (s -> ParserT s m a -> Either ParseError a) -> ParserT s m a -> s -> String -> Position -> Effect Unit
mkParseErrorTestPositionAndMessage runParser p input expectedMessage expectedPosition = case runParser input p of
  Right x -> assert' ("ParseError '" <> expectedMessage <> "' expected at " <> show expectedPosition <> " but parsed " <> show x) false
  Left err -> do
    let pos = parseErrorPosition err
    let msg = parseErrorMessage err
    assert' ("expected position: " <> show expectedPosition <> ", actual position: " <> show pos) (eqPositionFull expectedPosition pos)
    assert' ("expected message: " <> expectedMessage <> ", actual message: " <> msg) (expectedMessage == msg)
    logShow expectedMessage
    logShow expectedPosition
