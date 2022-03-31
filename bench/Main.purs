-- | # Benchmarking
-- |
-- |     spago -x spago-dev.dhall run --main Bench.Main --node-args '--expose-gc'
-- |
-- | This benchmark suite is intended to guide changes to this package so that
-- | we can compare the benchmarks of different commits.
-- |
-- | This benchmark suite also compares parsers to equivalent Regex. This
-- | provides an answer to the common question “How much slower is this package
-- | than Regex?” Answer: approximately 100×. The Regex benchmarks also give
-- | us a rough way to calibrate benchmarks run on different platforms.
-- |
-- | `--expose-gc` is from
-- | https://pursuit.purescript.org/packages/purescript-minibench/3.0.0/docs/Performance.Minibench#v:benchWith
-- |
-- | # Benchmark comparison for different commits
-- |
-- | The file bench.html will contain a Github-flavored-Markdown-compatible HTML
-- | table of the benchmarks side-by-side.
-- |
-- |     spago -x spago-dev.dhall run --main Bench.Main --node-args '--expose-gc' > bench1.txt
-- |     spago -x spago-dev.dhall run --main Bench.Main --node-args '--expose-gc' > bench2.txt
-- |     nix-shell -p saxon --command 'saxon <(echo "<table>"; cat bench1.txt bench2.txt; echo "</table>") bench/tabletranspose.xslt > bench.html'
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
import Control.Monad.Trampoline (runTrampoline)
import Control.Monad.Free (liftF)
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
import Parsing (Parser, runParser, runParserT)
import Parsing.String (string)
import Parsing.String.Basic (digit)
import StringParser as StringParser
import StringParser.CodePoints as StringParser.CodePoints
import StringParser.CodeUnits as StringParser.CodeUnits

string23 :: String
string23 = "23"

string23_2 :: String
string23_2 = fold $ replicate 2 string23

-- string23_10000 :: String
-- string23_10000 = fold $ replicate 10000 string23

string23_500 :: String
string23_500 = fold $ replicate 500 string23

stringSkidoo :: String
stringSkidoo = "skidoo"

stringSkidoo_2 :: String
stringSkidoo_2 = fold $ replicate 2 stringSkidoo

-- stringSkidoo_10000 :: String
-- stringSkidoo_10000 = fold $ replicate 10000 stringSkidoo

stringSkidoo_1000 :: String
stringSkidoo_1000 = fold $ replicate 1000 stringSkidoo

parse23 :: Parser String (List Char)
parse23 = many digit

parse23Points :: StringParser.Parser (List Char)
parse23Points = many StringParser.CodePoints.anyDigit

parse23Units :: StringParser.Parser (List Char)
parse23Units = many StringParser.CodeUnits.anyDigit

parse23Rec :: Parser String (List Char)
parse23Rec = manyRec digit

parse23PointsRec :: StringParser.Parser (List Char)
parse23PointsRec = manyRec StringParser.CodePoints.anyDigit

parse23UnitsRec :: StringParser.Parser (List Char)
parse23UnitsRec = manyRec StringParser.CodeUnits.anyDigit

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

parseSkidooRec :: Parser String (List String)
parseSkidooRec = manyRec $ string "skidoo"

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

htmlTableWrap :: String -> Effect Unit -> Effect Unit
htmlTableWrap caption benchmark = do
  log "<td><b>"
  log caption
  log "</b>"
  log "<pre>"
  benchmark
  log "</pre></td>"

main :: Effect Unit
main = do
  log "<tr>"
  htmlTableWrap "runParser parse23" $ benchWith 200
    $ \_ -> runParser string23_500 parse23
  htmlTableWrap "StringParser.runParser parse23Points" $ benchWith 20
    $ \_ -> StringParser.runParser parse23Points string23_500
  htmlTableWrap "StringParser.runParser parse23Units" $ benchWith 200
    $ \_ -> StringParser.runParser parse23Units string23_500
  htmlTableWrap "runParser parse23Rec" $ benchWith 200
    $ \_ -> runParser string23_500 parse23Rec
  htmlTableWrap "StringParser.runParser parse23PointsRec" $ benchWith 20
    $ \_ -> StringParser.runParser parse23PointsRec string23_500
  htmlTableWrap "StringParser.runParser parse23UnitsRec" $ benchWith 200
    $ \_ -> StringParser.runParser parse23UnitsRec string23_500
  htmlTableWrap "Regex.match pattern23" $ benchWith 200
    $ \_ -> Regex.match pattern23 string23_500
  htmlTableWrap "runParser parseSkidoo" $ benchWith 200
    $ \_ -> runParser stringSkidoo_1000 parseSkidoo
  htmlTableWrap "runParser parseSkidooRec" $ benchWith 200
    $ \_ -> runParser stringSkidoo_1000 parseSkidooRec
  htmlTableWrap "Regex.match patternSkidoo" $ benchWith 200
    $ \_ -> Regex.match patternSkidoo stringSkidoo_1000
  htmlTableWrap "runParser json smallJson" $ benchWith 1000
    $ \_ -> runParser smallJson BenchParsing.json
  htmlTableWrap "runTrampoline runParser json smallJson" $ benchWith 1000
    $ \_ -> runTrampoline $ runParserT smallJson BenchParsing.json
  htmlTableWrap "StringParser.runParser json smallJson" $ benchWith 500
    $ \_ -> StringParser.runParser BenchStringParser.json smallJson
  htmlTableWrap "runParser json mediumJson" $ benchWith 500
    $ \_ -> runParser mediumJson BenchParsing.json
  htmlTableWrap "runTrampoline runParser json mediumJson" $ benchWith 500
    $ \_ -> runTrampoline $ runParserT mediumJson BenchParsing.json
  htmlTableWrap "StringParser.runParser json mediumJson" $ benchWith 1000
    $ \_ -> StringParser.runParser BenchStringParser.json mediumJson
  htmlTableWrap "runParser json largeJson" $ benchWith 100
    $ \_ -> runParser largeJson BenchParsing.json
  htmlTableWrap "runTrampoline runParser json largeJson" $ benchWith 100
    $ \_ -> runTrampoline $ runParserT largeJson BenchParsing.json
  htmlTableWrap "StringParser.runParser json largeJson" $ benchWith 100
    $ \_ -> StringParser.runParser BenchStringParser.json largeJson
  log "</tr>"

