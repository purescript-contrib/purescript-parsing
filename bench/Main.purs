-- | # Benchmarking
-- |
-- |     spago -x spago-dev.dhall run --main Bench.Main
-- |
-- | This benchmark suite is intended to guide changes to this package so that
-- | we can compare the benchmarks of different commits.
-- |
-- | This benchmark suite also compares parsers to equivalent Regex. This
-- | provides an answer to the common question “How much slower is this package
-- | than Regex?” Answer: approximately 100×. The Regex benchmarks also give
-- | us a rough way to calibrate benchmarks run on different platforms.
-- |
-- | # Profiling
-- |
-- | https://nodejs.org/en/docs/guides/simple-profiling/
-- | https://nodesource.com/blog/diagnostics-in-NodeJS-2
-- |
-- |     spago -x spago-dev.dhall build --source-maps
-- |     purs bundle output/**/*.js --source-maps --output ./index.bundle.js
-- |
-- |
-- |     spago -x spago-dev.dhall build --source-maps --purs-args '--codegen corefn,sourcemaps'
-- |     zephyr Bench.Main.main --codegen sourcemaps,js
-- |     purs bundle dce-output/**/*.js --source-maps --module Bench.Main --main Bench.Main --output ./index.dce.bundle.js
-- |     node index.dce.bundle.js
-- |
-- |     spago -x spago-dev.dhall build --source-maps --purs-args '--codegen corefn,sourcemaps'
-- |     purs bundle output/**/*.js --source-maps --module Bench.Main --main Bench.Main --output ./index.bundle.js
-- |     node index.bundle.js
-- |     node --prof --enable-source-maps ./index.bundle.js
-- |     node --prof-process --source-map ./index.bundle.js.map isolate--.log > prof.txt
-- |
-- |     node --prof --enable-source-maps -e 'require("./output/Bench.Main/index.js").main()'
-- |     node --prof-process isolate--.log
-- |
-- |     spago -x spago-dev.dhall build
-- |     node --prof -e 'require("./output/Bench.Main/index.js").main()'
-- |     node --prof-process isolate--.log > prof.txt

module Bench.Main where

import Prelude

import Bench.Json.Parsing as BenchParsing
import Bench.Json.StringParser as BenchStringParser
import Bench.Json.TestData (largeJson, mediumJson, smallJson)
import Data.Array (fold, replicate)
import Data.Either (either)
import Data.List (many, manyRec)
import Data.List.Types (List)
import Data.String.Regex (Regex, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Performance.Minibench (benchWith)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.String.Basic (digit)
import StringParser as StringParser
import StringParser.CodePoints as StringParser.CodePoints
import StringParser.CodeUnits as StringParser.CodeUnits

string23 :: String
string23 = "23"

string23_2 :: String
string23_2 = fold $ replicate 2 string23

string23_10000 :: String
string23_10000 = fold $ replicate 10000 string23

stringSkidoo :: String
stringSkidoo = "skidoo"

stringSkidoo_2 :: String
stringSkidoo_2 = fold $ replicate 2 stringSkidoo

stringSkidoo_10000 :: String
stringSkidoo_10000 = fold $ replicate 10000 stringSkidoo

parse23 :: Parser String (List Char)
parse23 = manyRec digit

parse23Points :: StringParser.Parser (List Char)
parse23Points = manyRec StringParser.CodePoints.anyDigit

parse23Units :: StringParser.Parser (List Char)
parse23Units = manyRec StringParser.CodeUnits.anyDigit

pattern23 :: Regex
pattern23 = either (unsafePerformEffect <<< throw) identity
  $ regex "\\d"
  $ RegexFlags
      { dotAll: true
      , global: true
      , ignoreCase: false
      , multiline: true
      , sticky: false
      , unicode: true
      }

parseSkidoo :: Parser String (List String)
parseSkidoo = many $ string "skidoo"

patternSkidoo :: Regex
patternSkidoo = either (unsafePerformEffect <<< throw) identity
  $ regex "skidoo"
  $ RegexFlags
      { dotAll: true
      , global: true
      , ignoreCase: false
      , multiline: true
      , sticky: false
      , unicode: true
      }

main :: Effect Unit
main = do
  -- log $ show $ runParser string23_2 parse23
  -- log $ show $ Regex.match pattern23 string23_2
  -- log $ show $ runParser stringSkidoo_2 parseSkidoo
  -- log $ show $ Regex.match patternSkidoo stringSkidoo_2
  log "runParser parse23"
  benchWith 200
    $ \_ -> runParser string23_10000 parse23
  log "StringParser.runParser parse23Points"
  benchWith 20
    $ \_ -> StringParser.runParser parse23Points string23_10000
  log "StringParser.runParser parse23Units"
  benchWith 200
    $ \_ -> StringParser.runParser parse23Units string23_10000
  log "Regex.match pattern23"
  benchWith 200
    $ \_ -> Regex.match pattern23 string23_10000
  log "runParser parseSkidoo"
  benchWith 200
    $ \_ -> runParser stringSkidoo_10000 parseSkidoo
  log "Regex.match patternSkidoo"
  benchWith 200
    $ \_ -> Regex.match patternSkidoo stringSkidoo_10000

  log "runParser json smallJson"
  benchWith 1000
    $ \_ -> runParser smallJson BenchParsing.json

  log "StringParser.runParser json smallJson"
  benchWith 500
    $ \_ -> StringParser.runParser BenchStringParser.json smallJson

  log "runParser json mediumJson"
  benchWith 500
    $ \_ -> runParser mediumJson BenchParsing.json

  log "StringParser.runParser json mediumJson"
  benchWith 1000
    $ \_ -> StringParser.runParser BenchStringParser.json mediumJson

  log "runParser json largeJson"
  benchWith 100
    $ \_ -> runParser largeJson BenchParsing.json

  log "StringParser.runParser json largeJson"
  benchWith 100
    $ \_ -> StringParser.runParser BenchStringParser.json largeJson
