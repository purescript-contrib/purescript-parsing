module Main where

import Prelude

import Data.Foldable (fold)
import Effect (Effect)
-- https://github.com/purescript/trypurescript/blob/master/staging/src/TryPureScript.purs
import TryPureScript (h1, h3, p, text, render, code, indent, Doc)
import Parsing (Parser, runParser, ParseError(..), Position(..))
-- import Parsing.String (char, parseErrorHuman)
import Parsing.String (char)
import Control.Alt ((<|>))
import Data.Array (many)
import Data.Either (Either(..))
-- import Data.Functor (map)

ayebee :: Parser String Boolean
ayebee = do
  _ <- char 'a'
  b <- char 'b' <|> char 'B'
  pure (b == 'B')

main :: Effect Unit
main =
  render $ fold
    [ h1 $ text "Examples for Parsing Quick Start"

    , p $ text "Run the " <> (code $ text "ayebee") <> text " parser."
    , h3 $ code (text "runParser \"aB\" ayebee")
    , showResult runParser "aB" ayebee

    , p $ text "Run the " <> code (text "ayebee") <> text " parser with the " <> code (text "many") <> text " combinator."
    , h3 $ code (text "runParser \"aBabaB\" (many ayebee)")
    , showResult runParser "aBabaB" (many ayebee)

    , p $ text "Run the " <> code (text "ayebee") <> text " parser on unparsable input."
    , h3 $ code (text "runParser \"aCaB\" ayebee")
    , showResult runParser "aCaB" ayebee
    ]

showResult
  :: forall a. Show a
  => (String -> Parser String a -> Either ParseError a)
  -> String
  -> Parser String a
  -> Doc
showResult runner input parser =
  let
    result = runner input parser
  in
  case result of
    Right x -> indent $ h3$ code $ text $ show x
    Left err -> fold $ map (indent <<< h3 <<< code <<< text) $ parseErrorHuman input 40 err

-- for when parseErrorHuman becomes available in TryPureScript
-- https://pursuit.purescript.org/packages/purescript-parsing/10.1.0/docs/Parsing.String#v:parseErrorHuman
parseErrorHuman :: String -> Int -> ParseError -> Array String
parseErrorHuman input contextSize (ParseError msg (Position { line, column, index })) =
  [ msg <> " at position index:" <> show index
      <> " (line:"
      <> show line
      <> ", column:"
      <> show column
      <> ")"
  , ""
  , input
  ]

