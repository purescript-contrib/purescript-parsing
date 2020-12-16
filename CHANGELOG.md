# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):
- Non-empty combinators return `NonEmptyList` ([#102](https://github.com/purescript-contrib/purescript-parsing/pull/102))

New features:

Bugfixes:

Other improvements:

## [v5.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.1.0) - 2020-10-08

New features:
  - Adds a `region` function similar to the [function of the same name in megaparsec](https://hackage.haskell.org/package/megaparsec-9.0.0/docs/Text-Megaparsec.html#v:region) which specifies how to process `ParseError`s which occur inside the indicated region (@jamesdbrock in #97 and #96).

Bugfixes:
  - Fixes the token definitions for the Java language parser (@matoruru in #86)

## [v5.0.3](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.0.3) - 2019-05-06

Other improvements:
  - Update escape sequences to match parsing in the latest version of the PureScript compiler (@natefaubion)
  - Docs: Add a documentation comment about stack safety (@Dretch)

## [v5.0.2](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.0.2) - 2018-11-15

Bugixes:
  - Docs: Update typos in readme and documentation (@justinwoo, @rndnoise)

## [v5.0.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.0.1) - 2018-06-23

Other improvements:
  - Adds metadata including contributor guidelines
  - Pushes latest release to Pursuit

## [v5.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.0.0) - 2018-06-19

Breaking changes:
  - Updated for PureScript 0.12

## [v4.3.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v4.3.1) - 2017-08-08

Bugfixes:
  - Fixed shadowed name warnings

## [v4.3.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v4.3.0) - 2017-07-26

New features:
  - Added `tryRethrow` and improved error messages for `satisfy` and `when` (@natefaubion)

## [v4.2.2](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v4.2.2) - 2017-06-03

Bufixes:
  - Fix some issues with error messages (@safareli)

## [v4.2.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v4.2.1) - 2017-04-25

Bufixes:
  - `mapParserT` is now exported (@safareli)

## [v4.2.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v4.2.0) - 2017-04-24

New features:
  - Add `mapParserT` (@safareli)

## [v4.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v4.1.0) - 2017-04-16

Other improvements:
  - Add `Semigroup` and `Monoid` instances (@i-am-tom)

## [v4.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v4.0.0) - 2017-04-05

Breaking changes:
  - Updates for 0.11 compiler

## [v3.2.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v3.2.1) - 2017-02-15

Other improvements:
  - Avoid Discard constraints (@mlang)

## [v3.2.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v3.2.0) - 2017-01-16

Bugfixes:
  - Fix some `Show` instances (@mlang)

## [v3.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v3.1.0) - 2016-12-19

New features:
  - Added `hoistParserT`
  - Un-applied the `Parser` synonym

## [v3.0.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v3.0.1) - 2016-11-17

Bugfixes:
  - Fixed shadowed name warning (@tmcgilchrist)

## [v2.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v2.0.0) - 2016-10-27

Breaking changes:
  - Updates for 0.10 compiler
  - Switch to `transformers`-based API, add new instances
  - Use some more efficient data types
  - Add port of `indents` Haskell package (@starkstark)
  - Switch to BSD 3-clause license to be compatible with ported Haskell code

## [v1.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v1.0.0) - 2016-06-10

Breaking changes:
  - Updates for the 1.0 core libraries.

## [v0.8.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.8.1) - 2016-05-18

Other improvements:
  - Added `Eq` instance for `ParseError` (@triggerNZ)

## [v0.8.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.8.0) - 2016-02-21

New features:
  - Added `Language` and `Token` modules, ported from Parsec (@cdepillabout)

## [v0.7.2](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.7.2) - 2016-01-21

Bugfixes:
  - Fixed warnings raised by psc 0.7.6.1 (@hdgarrood)

## [v0.7.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.7.1) - 2015-10-28

Other improvements:
  - improve error message (@epost)

## [v0.7.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.7.0) - 2015-09-18

Other improvements:
  - Bump `transformers` dependency.

## [v0.6.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.6.1) - 2015-08-26

This release requires the upcoming 0.7.4.0 release of the PureScript compiler. Previous versions of this library will _not_ work with `psc` versions <= 0.7.3.0.

Breaking changes:
  - Updated to `transformers-0.7`.

## [v0.5.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.5.1) - 2015-08-18

Other improvements:
  - Updated documentation (@kRITZCREEK)

## [v0.5.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.5.0) - 2015-08-13

Other improvements:
  - Bump dependencies

## [v0.4.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.4.0) - 2015-06-30

This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.4.0-rc.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.4.0-rc.1) - 2015-06-12

Initial release for the 0.7 compiler version.

## [v0.3.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.3.1) - 2015-03-04

Add `token`, `match` and `when` (@cryogenian)

## [v0.3.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.3.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated dependencies

## [v0.2.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.2.0) - 2015-01-10

- Update dependencies (@garyb)

## [v0.1.4](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.1.4) - 2014-11-18

Bump for `Identity` changes.

## [v0.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.1.0) - 2014-08-11

Initial semver release.
