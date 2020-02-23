# purescript-parsing

[![Latest release](http://img.shields.io/github/release/purescript-contrib/purescript-parsing.svg)](https://github.com/purescript-contrib/purescript-parsing/releases)
[![Build status](https://travis-ci.org/purescript-contrib/purescript-parsing.svg?branch=master)](https://travis-ci.org/purescript-contrib/purescript-parsing)
[![Pursuit](http://pursuit.purescript.org/packages/purescript-parsing/badge)](http://pursuit.purescript.org/packages/purescript-parsing/)
[![Maintainer: garyb](https://img.shields.io/badge/maintainer-garyb-lightgrey.svg)](http://github.com/garyb)
[![Maintainer: thomashoneyman](https://img.shields.io/badge/maintainer-thomashoneyman-lightgrey.svg)](http://github.com/thomashoneyman)

A parser combinator library based on Haskell's Parsec.

## Parsers

There are three sets of parsing primitives included in this library.

* `Text.Parsing.Parser.String` module for native `String` parsing.
* `Text.Parsing.Parser.Token` module for parsing `List`s of any type of token.
* `Text.Parsing.Parser.DataView` module for parsing native Javascript `ArrayBuffer`.

All parsing primitives are “auto-backtracking”, meaning that if they fail,
then they consume no input.

## Installation

```
bower install purescript-parsing
```

## Documentation

- [See the tests](test/Main.purs) for some example usages.
- Module documentation is [published on Pursuit](http://pursuit.purescript.org/packages/purescript-parsing).

## Contributing

Read the [contribution guidelines](https://github.com/purescript-contrib/purescript-parsing/blob/master/.github/contributing.md) to get started and see helpful related resources.
