-- Spago configuration for testing, benchmarking, development.
--
-- See:
-- * ./CONTRIBUTING.md
-- * https://github.com/purescript/spago#devdependencies-testdependencies-or-in-general-a-situation-with-many-configurations
--

let conf = ./spago.dhall

in conf //
{ sources = [ "src/**/*.purs", "test/**/*.purs", "bench/**/*.purs" ]
, dependencies = conf.dependencies #
  [ "assert"
  , "bifunctors"
  , "console"
  , "enums"
  , "effect"
  , "minibench"
  , "node-process"
  , "nonempty"
  , "exceptions"
  , "string-parsers"
  , "partial"
  ]
, packages = ./packages.dhall
}
