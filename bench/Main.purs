-- | # Benchmarking
-- |
-- |     spago -x spago-dev.dhall run --main Bench.Main --node-args '--expose-gc'
-- |
-- | This benchmark suite is intended to guide changes to this package so that
-- | we can compare the benchmarks of different commits.
-- |
-- | This benchmark suite also compares parsers to equivalent Regex. This
-- | provides an answer to the common question “How much slower is this package
-- | than Regex?” Answer: approximately 10×. The Regex benchmarks also give
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
import Bench.Json.TestData (largeJson, mediumJson)
import Data.Array (fold, replicate)
import Data.Array as Array
import Data.Either (Either(..), either)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex, regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags (RegexFlags(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Parsing (runParser)
import Parsing.Combinators (chainl, chainr, many, manyTill, manyTill_, sepBy, sepEndBy1, skipMany)
import Parsing.Combinators.Array as Combinators.Array
import Parsing.String (anyChar, eof, string)
import Parsing.String.Basic (digit)
import Performance.Minibench (benchWith)
import StringParser as StringParser
import StringParser.CodePoints as StringParser.CodePoints
import StringParser.CodeUnits as StringParser.CodeUnits

string23 :: String
string23 = "23"

string23_1000 :: String
string23_1000 = fold $ replicate 500 string23

string23_10000 :: String
string23_10000 = fold $ replicate 5000 string23

stringSkidoo :: String
stringSkidoo = "skidoooooo"

stringSkidoo_100000 :: String
stringSkidoo_100000 = fold $ replicate 10000 stringSkidoo

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

patternSkidoo :: Regex
patternSkidoo = either (unsafePerformEffect <<< throw) identity
  $ regex "skidoooooo"
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

throwLeft :: forall a b. Show a => Either a b -> b
throwLeft (Left err) = unsafePerformEffect $ throw $ show err
throwLeft (Right x) = x

throwNothing :: forall a. String -> Maybe a -> a
throwNothing err Nothing = unsafePerformEffect $ throw err
throwNothing _ (Just x) = x

main :: Effect Unit
main = do
  log "<tr>"

  log "<th><h2>digit 10000</h2></th>"
  htmlTableWrap "runParser many digit 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 (many digit)
  htmlTableWrap "runParser Array.many digit 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 (Array.many digit)
  htmlTableWrap "StringParser manyRec CodePoints.anyDigit 10000" $ benchWith 20
    $ \_ -> throwLeft $ StringParser.runParser (List.manyRec StringParser.CodePoints.anyDigit) string23_10000
  htmlTableWrap "StringParser manyRec CodeUnits.anyDigit 10000" $ benchWith 200
    $ \_ -> throwLeft $ StringParser.runParser (List.manyRec StringParser.CodeUnits.anyDigit) string23_10000
  htmlTableWrap "Regex.match \\d* 10000" $ benchWith 200
    $ \_ -> throwNothing "Regex.match failed" $ Regex.match pattern23 string23_10000

  log "<th><h2>string 100000</h2></th>"
  htmlTableWrap "runParser many string" $ benchWith 200
    $ \_ -> throwLeft $ runParser stringSkidoo_100000 (many $ string "skidoooooo")
  htmlTableWrap "Regex.match literal*" $ benchWith 200
    $ \_ -> throwNothing "Regex.match failed" $ Regex.match patternSkidoo stringSkidoo_100000

  log "<th><h2>many anyChar 10000</h2></th>"
  htmlTableWrap "runParser many anyChar 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 (many anyChar)
  htmlTableWrap "runParser Array.many anyChar 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 (Array.many anyChar)
  htmlTableWrap "runParser Combinators.Array.many anyChar 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 (Combinators.Array.many anyChar)

  log "<th><h2>skipMany anyChar 10000</h2></th>"
  htmlTableWrap "runParser skipMany anyChar 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 (skipMany anyChar)

  log "<th><h2>sepBy 10000</h2></th>"
  htmlTableWrap "runParser sepBy 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 $ sepBy anyChar (pure unit)

  log "<th><h2>sepEndBy1 10000</h2></th>"
  htmlTableWrap "runParser sepEndBy1 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 $ sepEndBy1 anyChar (pure unit)

  log "<th><h2>chainl 10000</h2></th>"
  htmlTableWrap "runParser chainl 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 $ chainl anyChar (pure const) 'x'

  log "<th><h2>chainr 1000</h2></th>"
  htmlTableWrap "runParser chainr 1000" $ benchWith 200
    $ \_ -> throwLeft $ runParser string23_1000 $ chainr anyChar (pure const) 'x'

  log "<th><h2>chainr 10000</h2></th>"
  htmlTableWrap "runParser chainr 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 $ chainr anyChar (pure const) 'x'

  log "<th><h2>manyTill 1000</h2></th>"
  htmlTableWrap "runParser manyTill 1000" $ benchWith 200
    $ \_ -> throwLeft $ runParser string23_1000 $ manyTill anyChar eof
  htmlTableWrap "runParser manyTill_ 1000" $ benchWith 200
    $ \_ -> throwLeft $ runParser string23_1000 $ manyTill_ anyChar eof

  log "<th><h2>manyTill 10000</h2></th>"
  htmlTableWrap "runParser manyTill 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 $ manyTill anyChar eof
  htmlTableWrap "runParser manyTill_ 10000" $ benchWith 50
    $ \_ -> throwLeft $ runParser string23_10000 $ manyTill_ anyChar eof

  log "<th><h2>mediumJson</h2></th>"
  htmlTableWrap "runParser json mediumJson" $ benchWith 200
    $ \_ -> throwLeft $ runParser mediumJson BenchParsing.json
  htmlTableWrap "StringParser.runParser json mediumJson" $ benchWith 200
    $ \_ -> throwLeft $ StringParser.runParser BenchStringParser.json mediumJson

  log "<th><h2>largeJson</h2></th>"
  htmlTableWrap "runParser json largeJson" $ benchWith 100
    $ \_ -> throwLeft $ runParser largeJson BenchParsing.json
  htmlTableWrap "StringParser.runParser json largeJson" $ benchWith 100
    $ \_ -> throwLeft $ StringParser.runParser BenchStringParser.json largeJson
  log "</tr>"

