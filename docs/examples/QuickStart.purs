module Main where

import Prelude

import Data.Foldable (fold)
import Effect (Effect)
import TryPureScript (h1, h3, p, text, render, code)
import Parsing (Parser, runParser)
import Parsing.String (char)
import Control.Alt ((<|>))
import Data.Array (many)

ayebee :: Parser String Boolean
ayebee = do
  _ <- char 'a'
  b <- char 'b' <|> char 'B'
  pure (b == 'B')
    
main :: Effect Unit
main =
  render $ fold
    [ h1 $ text "Examples for purescript-parsing Quick Start"
    
    , p $ text "Run the " <> (code $ text "ayebee") <> text " parser."
    , h3 $ code (text "runParser \"aB\" ayebee")
    , h3 $ code $ text $ show $ runParser "aB" ayebee
    
    , p $ text "Run the " <> code (text "ayebee") <> text " parser with the " <> code (text "many") <> text " combinator."
    , h3 $ code (text "runParser \"aBabaB\" (many ayebee)")
    , h3 $ code $ text $ show $ runParser "aBabaB" (many ayebee)
    ]
