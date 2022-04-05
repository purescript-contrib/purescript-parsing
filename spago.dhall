{ name = "parsing"
, license = "BSD-2-Clause"
, repository = "https://github.com/purescript-contrib/purescript-parsing.git"
, dependencies =
  [ "arrays"
  , "control"
  , "either"
  , "enums"
  , "foldable-traversable"
  , "functions"
  , "identity"
  , "integers"
  , "lazy"
  , "lists"
  , "maybe"
  , "newtype"
  , "numbers"
  , "partial"
  , "prelude"
  , "strings"
  , "tailrec"
  , "transformers"
  , "tuples"
  , "unfoldable"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
