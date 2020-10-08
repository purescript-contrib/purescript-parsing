{ name = "parsing"
, dependencies =
  [ "arrays"
  , "assert"
  , "console"
  , "effect"
  , "either"
  , "foldable-traversable"
  , "identity"
  , "integers"
  , "lists"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
