module Bench.Json.StringParser where

import Prelude hiding (between)

import Bench.Json.Common (Json(..))
import Control.Lazy (defer)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.Tuple (Tuple(..))
import StringParser (Parser, fail, try)
import StringParser.CodePoints (regex, skipSpaces, string)
import StringParser.Combinators (between, choice, sepBy)

json :: Parser Json
json = defer \_ ->
  skipSpaces *> choice
    [ JsonObject <$> jsonObject
    , JsonArray <$> jsonArray
    , JsonString <$> jsonString
    , JsonNumber <$> jsonNumber
    , JsonBoolean <$> jsonBoolean
    , JsonNull <$ jsonNull
    ]

jsonObject :: Parser (List (Tuple String Json))
jsonObject = defer \_ ->
  between (string "{") (skipSpaces *> string "}") do
    skipSpaces *> jsonObjectPair `sepBy` (try (skipSpaces *> string ","))

jsonObjectPair :: Parser (Tuple String Json)
jsonObjectPair = defer \_ ->
  Tuple <$> (skipSpaces *> jsonString <* skipSpaces <* string ":") <*> json

jsonArray :: Parser (List Json)
jsonArray = defer \_ ->
  between (string "[") (skipSpaces *> string "]") do
    json `sepBy` (try (skipSpaces *> string ","))

jsonString :: Parser String
jsonString = between (string "\"") (string "\"") do
  regex """\\"|[^"]*"""

jsonNumber :: Parser Number
jsonNumber = do
  n <- regex """(\+|-)?(\d+(\.\d*)?|\d*\.\d+)([eE](\+|-)?\d+)?"""
  case Number.fromString n of
    Just n' -> pure n'
    Nothing -> fail "Expected number"

jsonBoolean :: Parser Boolean
jsonBoolean = choice
  [ true <$ string "true"
  , false <$ string "false"
  ]

jsonNull :: Parser String
jsonNull = string "null"
