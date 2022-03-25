module Bench.Json.Common where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)

data Json
  = JsonNull
  | JsonNumber Number
  | JsonString String
  | JsonBoolean Boolean
  | JsonArray (List Json)
  | JsonObject (List (Tuple String Json))

derive instance Generic Json _

instance Show Json where
  show a = genericShow a
