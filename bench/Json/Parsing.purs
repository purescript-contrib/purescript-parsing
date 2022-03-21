module Bench.Json.Parsing where

import Prelude hiding (between)

import Bench.Json.Common (Json(..))
import Control.Lazy (defer)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser (Parser, fail)
import Text.Parsing.Parser.Combinators (between, choice, sepBy, try)
import Text.Parsing.Parser.String (regex, skipSpaces, string)

json :: Parser String Json
json = defer \_ ->
  skipSpaces *> choice
    [ JsonObject <$> jsonObject
    , JsonArray <$> jsonArray
    , JsonString <$> jsonString
    , JsonNumber <$> jsonNumber
    , JsonBoolean <$> jsonBoolean
    , JsonNull <$ jsonNull
    ]

jsonObject :: Parser String (List (Tuple String Json))
jsonObject = defer \_ ->
  between (string "{") (skipSpaces *> string "}") do
    skipSpaces *> jsonObjectPair `sepBy` (try (skipSpaces *> string ","))

jsonObjectPair :: Parser String (Tuple String Json)
jsonObjectPair = defer \_ ->
  Tuple <$> (skipSpaces *> jsonString <* skipSpaces <* string ":") <*> json

jsonArray :: Parser String (List Json)
jsonArray = defer \_ ->
  between (string "[") (skipSpaces *> string "]") do
    json `sepBy` (try (skipSpaces *> string ","))

jsonString :: Parser String String
jsonString = between (string "\"") (string "\"") do
  regex {} """\\"|[^"]*"""

jsonNumber :: Parser String Number
jsonNumber = do
  n <- regex {} """(\+|-)?(\d+(\.\d*)?|\d*\.\d+)([eE](\+|-)?\d+)?"""
  case Number.fromString n of
    Just n' -> pure n'
    Nothing -> fail "Expected number"

jsonBoolean :: Parser String Boolean
jsonBoolean = choice
  [ true <$ string "true"
  , false <$ string "false"
  ]

jsonNull :: Parser String String
jsonNull = string "null"
