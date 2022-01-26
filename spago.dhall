{ name = "parsing"
, license = "BSD-2-Clause"
, repository = "https://github.com/purescript-contrib/purescript-parsing.git"
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
  , "numbers"
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
