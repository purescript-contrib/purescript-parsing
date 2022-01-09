# Parsing

[![CI](https://github.com/purescript-contrib/purescript-parsing/workflows/CI/badge.svg?branch=main)](https://github.com/purescript-contrib/purescript-parsing/actions?query=workflow%3ACI+branch%3Amain)
[![Release](https://img.shields.io/github/release/purescript-contrib/purescript-parsing.svg)](https://github.com/purescript-contrib/purescript-parsing/releases)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-parsing/badge)](https://pursuit.purescript.org/packages/purescript-parsing)
[![Maintainer: jamesdbrock](https://img.shields.io/badge/maintainer-jamesdbrock-teal.svg)](https://github.com/jamesdbrock)
[![Maintainer: robertdp](https://img.shields.io/badge/maintainer-robertdp-teal.svg)](https://github.com/robertdp)

A parser combinator library based on Haskell's [Parsec](https://hackage.haskell.org/package/parsec).

## Installation

Install `parsing` with [Spago](https://github.com/purescript/spago):

```sh
spago install parsing
```

## Quick start

Here is a basic introduction to monadic parsers.

### Parsers

A parser in this library has the type `Parser s a`, where `s` the type of the input string, and `a` is the type of the data which the parser will produce on success. `Parser s a` is a monad. It’s defined in the module `Text.Parsing.Parser`.

Monads can be used to provide context for a computation, and that’s how we use them in monadic parsing. The context provided by the `Parser` monad is *our current location in the input string*. Parsing starts at the beginning of the input string.

Parsing requires two more capabilities: *choice* and *failure*.

We need *choice* to be able to make decisions about what kind of thing we’re parsing depending on the input which we encouter. This is provided by the `Alt` typeclass instance of `Parser` monad, particularly the `<|>` operator.

We need *failure* in case the input stream is not parseable. This is provided by the `fail` function, which calls the `throwError` function of the `MonadThrow` typeclass instance of the `Parser` monad.

Here is the original short classic [FUNCTIONAL PEARLS Monadic Parsing in Haskell](https://www.cs.nott.ac.uk/~pszgmh/pearl.pdf) by Graham Hutton and Erik Meijer. There are lots of other great monadic parsing tutorials on the internet.

### Running a parser

To run a parser, call the function `runParser` in the `Text.Parsing.Parser` module, and supply it with a parser and an input.

### Primitive parsers

Each type of input string needs primitive parsers. Primitive parsers for input string type `String` are in the `Text.Parsing.Parser.String` module. We can use these primitive parsers to write other parsers.

Here is a parser which will accept the input string `"ab"` or the input string `"aB"`, and will return only the `'b'` character, whether it’s lowercase or uppercase.

```purescript
ayebee :: Parser String Char
ayebee = do
  _ <- char 'a'
  b <- char 'b' <|> char 'B'
  pure b
```

We can run the parser `ayebee` like so

```purescript
runParser "aB" ayebee
```

and then the parser will succeed and return `Right 'B'`.

### More parsers

There are other `String` parsers in the module `Text.Parsing.Parser.Token`, for example the parser `letter :: Parser String Char` which will accept any single alphabetic letter.

### Parser combinators

A parser combinator is a function which takes a parser as an argument and returns another parser. The `many` combinator, for example, will repeat a parser as many time as it can. So the parser `many letter` will have type `Parser String (Array Char)`. Parser combinators are in this package in the module `Text.Parsing.Parser.Combinators`.

## Related Packages

- [`parsing-dataview`](https://pursuit.purescript.org/packages/purescript-parsing-dataview)
  Provides the module `Text.Parsing.Parser.DataView` of primitive parsers for binary parsing of
  `ArrayBuffer`.

## Documentation

`parsing` documentation is stored in a few places:

1. Module documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-parsing).
2. Written documentation is kept in the [docs directory](./docs).
3. Usage examples can be found in [the test suite](./test).

If you get stuck, there are several ways to get help:

- [Open an issue](https://github.com/purescript-contrib/purescript-parsing/issues) if you have encountered a bug or problem.
- Ask general questions on the [PureScript Discourse](https://discourse.purescript.org) forum or the [PureScript Discord](https://purescript.org/chat) chat.

## Contributing

You can contribute to `parsing` in several ways:

1. If you encounter a problem or have a question, please [open an issue](https://github.com/purescript-contrib/purescript-parsing/issues). We'll do our best to work with you to resolve or answer it.

2. If you would like to contribute code, tests, or documentation, please [read the contributor guide](./CONTRIBUTING.md). It's a short, helpful introduction to contributing to this library, including development instructions.

3. If you have written a library, tutorial, guide, or other resource based on this package, please share it on the [PureScript Discourse](https://discourse.purescript.org)! Writing libraries and learning resources are a great way to help this library succeed.
