module Main where

import Prelude

import Data.Foldable (fold)
import Effect (Effect)
import TryPureScript (h1, h2, p, text, list, indent, link, render, code)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (char)
import Control.Alt ((<|>))

ayebee :: Parser String Boolean
ayebee = do
  _ <- char 'a'
  b <- char 'b' <|> char 'B'
  pure (b == 'B')
    
main :: Effect Unit
main =
  render $ fold
    [ h1 $ text "Examples for purescript-parsing"
    , p $ code $ text $ show $ runParser "aB" ayebee
    ]
