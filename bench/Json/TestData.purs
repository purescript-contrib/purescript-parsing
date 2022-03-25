module Bench.Json.TestData where

import Prelude

import Data.Array (replicate)
import Data.String (joinWith)

jsonProps :: String
jsonProps =
  """
    "some_number": 42.00009
  , "some_string": "foobarbazquux"
  , "some_null": null
  , "some_boolean": true
  , "some_other_boolean": false
  , "some_array": [ 1, 2, "foo", true, 99 ]
  , "some_object": { "foo": 42, "bar": "wat", "baz": false }
  """

smallJson :: String
smallJson = "{" <> jsonProps <> "}"

mediumJson :: String
mediumJson = "{" <> joinWith ", " (replicate 30 jsonProps) <> "}"

largeJson :: String
largeJson = "[" <> joinWith ", " (replicate 100 smallJson) <> "]"
