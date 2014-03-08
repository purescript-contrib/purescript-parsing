all: lib test

lib:
	mkdir -p js/Text/Parsing
	psc src/Text/Parsing/Parser.purs.hs \
	  src/Text/Parsing/Parser/String.purs.hs \
	  src/Text/Parsing/Parser/Combinators.purs.hs \
	  src/Text/Parsing/Parser/Expr.purs.hs \
	  -o js/Text/Parsing/Parser.js \
	  -e js/Text/Parsing/Parser.e.purs.hs \
	  --module Text.Parsing.Parser --tco --magic-do

test:
	psc src/Text/Parsing/Parser.purs.hs \
	  src/Text/Parsing/Parser/String.purs.hs \
	  src/Text/Parsing/Parser/Combinators.purs.hs \
	  src/Text/Parsing/Parser/Expr.purs.hs \
	  examples/test.purs.hs \
	  -o js/test.js \
	  --main --module Main --tco --magic-do
