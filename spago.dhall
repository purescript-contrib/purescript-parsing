{ name = "parsing"
, dependencies =
  [ "arrays"
  , "control"
  , "either"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "newtype"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unicode"
  , "unsafe-coerce"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
