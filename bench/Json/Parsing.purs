module Bench.Json.Parsing where

import Prelude hiding (between)

import Bench.Json.Common (Json(..))
import Control.Lazy (defer)
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Number as Number
import Data.String.Regex.Flags (noFlags)
import Data.Tuple (Tuple(..))
import Parsing (ParserT, fail)
import Parsing.Combinators (between, choice, sepBy, try)
import Parsing.String (regex, string)
import Parsing.String.Basic (skipSpaces)
import Partial.Unsafe (unsafeCrashWith)

json :: forall m. Monad m => ParserT String m Json
json = defer \_ ->
  skipSpaces *> choice
    [ JsonObject <$> jsonObject
    , JsonArray <$> jsonArray
    , JsonString <$> jsonString
    , JsonNumber <$> jsonNumber
    , JsonBoolean <$> jsonBoolean
    , JsonNull <$ jsonNull
    ]

jsonObject :: forall m. Monad m => ParserT String m (List (Tuple String Json))
jsonObject = defer \_ ->
  between (string "{") (skipSpaces *> string "}") do
    skipSpaces *> jsonObjectPair `sepBy` (try (skipSpaces *> string ","))

jsonObjectPair :: forall m. Monad m => ParserT String m (Tuple String Json)
jsonObjectPair = defer \_ ->
  Tuple <$> (skipSpaces *> jsonString <* skipSpaces <* string ":") <*> json

jsonArray :: forall m. Monad m => ParserT String m (List Json)
jsonArray = defer \_ ->
  between (string "[") (skipSpaces *> string "]") do
    json `sepBy` (try (skipSpaces *> string ","))

jsonString :: forall m. Monad m => ParserT String m String
jsonString = case regex """\\"|[^"]*""" noFlags of
  Left err -> unsafeCrashWith err
  Right p -> between (string "\"") (string "\"") p

jsonNumber :: forall m. Monad m => ParserT String m Number
jsonNumber = case regex """(\+|-)?(\d+(\.\d*)?|\d*\.\d+)([eE](\+|-)?\d+)?""" noFlags of
  Left err -> unsafeCrashWith err
  Right p -> do
    n <- p
    case Number.fromString n of
      Just n' -> pure n'
      Nothing -> fail "Expected number"

jsonBoolean :: forall m. Monad m => ParserT String m Boolean
jsonBoolean = choice
  [ true <$ string "true"
  , false <$ string "false"
  ]

jsonNull :: forall m. Monad m => ParserT String m String
jsonNull = string "null"
