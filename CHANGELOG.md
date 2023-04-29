# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Bugfixes:
- `float` parser of `GenTokenParser` does not parse negative numbers (by @mstream)

Breaking changes:

New features:

Other improvements:

## [v10.2.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v10.2.0) - 2022-11-30

New features:

- Add `Parsing.String.Basic.takeWhile`, `takeWhile1` (#218 by @jamesdbrock)

## [v10.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v10.1.0) - 2022-11-10

New features:

- add `MonadAsk` and `MonadReader` instances (#208 by @bentongxyz)
- Add `Parsing.String.parseErrorHuman` (#209 by @jamesdbrock)
- Add `liftMaybe`, `liftEither`, `liftExceptT` (#212 by @jamesdbrock)

Other improvements:

- Better error messages for `manyIndex` (#211 by @jamesdbrock)
- Docs for `region` (#213 by @jamesdbrock)
- README Recursion (#214 by @jamesdbrock)

## [v10.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v10.0.0) - 2022-07-18

Bugfixes:

- consumeWith doesn't consume if position does not advance (#201 by @jamesdbrock)

  This will effect parsers:

  * rest
  * string
  * takeN
  * regex
  * whiteSpace
  * skipSpaces

- `number` should parse scientific notation when exponent does not contain a decimal (#204 by @MaybeJustJames)


## [v9.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v9.1.0) - 2022-06-12

Breaking changes:

New features:

- Add `Array` combinators in a new `Combinators.Array` module (#199 by @jamesdbrock)

Bugfixes:

- Bugfix in `intDecimal`.

Other improvements:

## [v9.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v9.0.0) - 2022-04-27

Breaking changes:
- Update project and deps to PureScript v0.15.0 (#160 by @JordanMartinez)
- Drop deprecated `MonadZero` instance (#160 by @JordanMartinez)
- New optimized internals. `ParserT` now has a more efficient representation,
  resulting in (up to) 20x performance improvement. In addition to the performance,
  all parser execution is always stack-safe, even monadically, obviating the need
  to run parsers with `Trampoline` as the base Monad or to explicitly use `MonadRec`.

  Code that was parametric over the underlying Monad no longer needs to propagate a
  Monad constraint.

  Code that constructs parsers via the underlying representation will need to be updated,
  but otherwise the interface is unchanged and parsers should just enjoy the speed boost.

  (#154 by @natefaubion)
- Make `<??>` right-associative (#164 by @JordanMartinez)
- Drop `<?>` and `<~?>` prec from 3 to 4 (#163, #164 by @JordanMartinez)

  `<|>` was made right associative. Decreasing these two operators
  prevents a compiler error (i.e. `MixedAssociativityError`)
  without causing issues with `<$>`.
- Rename module prefix from `Text.Parsing.Parser` to `Parsing` (#169 by @jamesdbrock)
- Replace the `regex` parser. (#170 by @jamesdbrock)
- Reorganize Combinators for #154 (#182 by @jamesdbrock)
- Add the `index` field to `Position`. (#171 by @jamesdbrock)
- Move the parsers
  * `whiteSpace`
  * `skipSpaces`
  * `oneOf`
  * `oneOfCodePoints`
  * `noneOf`
  * `noneOfCodePoints`
  from `Parsing.String` to `Parsing.String.Basic`. (#183 by @jamesdbrock)
- Change MonadState instance (#187 by jamesdbrock)

  Users who stack a `ParserT` on a `StateT` base monad will call the `MonadState` members directly like `get` instead of needing to do `lift <<< get`.

  To get the `ParserT` internal state, call `getParserT` instead of `get`.

New features:

- Add the `anyTill` primitive `String` combinator. (#186 by @jamesdbrock)
- Add the `Parsing.String.Replace` module, copied from
  https://github.com/jamesdbrock/purescript-parsing-replace (#188 by @jamesdbrock)
  `streamEditT` can be written in terms of `replaceT`.

  `streamEditT input sep editor = replaceT input (sep >>= editor >>> lift)`

  (#188 by @jamesdbrock, #194 by @jamesdbrock)
- Add the `advance` and `manyIndex` combinators. (#193 by @jamesdbrock)

Bugfixes:

- Improve correctness and speed of `number` and `intDecimal`. (#189 by @jamesdbrock)

Other improvements:

- Drop `math` dependency; update imports (#167 by @JordanMartinez)

## [v8.4.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v8.4.0) - 2022-03-15

New features:

Add `regex` parser. (#153 by @jamesdbrock)

## [v8.3.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v8.3.0) - 2022-03-13

New features:

Add `lower` parser to Text.Parsing.Parser.String.Basic. (#152 by @mkohlhass)

Bugfixes:

Add `unfoldable` to `spago.dhall`. (#152 by @mkohlhass)

## [v8.2.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v8.2.0) - 2022-01-26

Breaking changes:

New features:

- `Parser.String.rest` (#140 by @jamesdbrock)
- `Parser.String.takeN` (#140 by @jamesdbrock)
- `Parser.Token.eof` (#140 by @jamesdbrock)
- `Parser.Combinators.manyTill_` (#143 by @jamesbrock)
- `Parser.Combinators.many1Till_` (#143 by @jamesbrock)
- `Parser.Combinators.manyTillRec_` (#143 by @jamesbrock)
- `Parser.Combinators.many1TillRec_` (#143 by @jamesbrock)
- `Parser.String.Basic.number` (#142 by @jamesbrock)
- `Parser.String.Basic.intDecimal` (#142 by @jamesbrock)

Bugfixes:

- `Parser.String.eof` Set consumed on success so that this parser combines
  correctly with `notFollowedBy eof`. Added a test for this. (#140 by @jamesdbrock)

Other improvements:

- Moved the `Parser.Token` parsers `digit`, `hexDigit`, `octDigit`, `upper`,
  `space`, `letter`, `alphaNum` into the new module `Parser.String.Basic`. (#142 by @jamesdbrock)
- Documentation. (#140 by @jamesdbrock)
- Documentation. (#143 by @jamesdbrock)
- Documentation. (#142 by @jamesdbrock)

## [v8.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v8.1.0) - 2022-01-10

Other improvements: README Quick start monadic parsing tutorial

## [v8.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v8.0.0) - 2022-01-10

Breaking changes:

- De-exported the private helper functions `chainl1'` and `chainr1'`.
  (#131 by @fsoikin)

New features:

- Added more stack-safe (at the expense of `MonadRec` constraint) combinators
  `many1Rec`, `sepByRec`, `sepBy1Rec`, `endByRec`, `endBy1Rec`, `chainrRec`,
  `chainr1Rec`, `chainlRec`, `chainl1Rec`, `skipManyRec`, and `skipMany1Rec`.
  (#131 by @fsoikin)

## [v7.2.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v7.2.0) - 2022-01-07

New features:

- Added stack-safe (at the expense of `MonadRec` constraint) combinators
  `manyTillRec`, `many1TillRec`, `sepEndByRec`, and `sepEndBy1Rec`. (#130 by @fsoikin)
- Added a new operator `<~?>` (alias of `withLazyErrorMessage`), an analog of
  `<?>`, but allows the error message to be deferred until there is actually an
  error. Handy when the error message is expensive to construct. (#129 by @fsoikin)

## [v7.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v7.1.0) - 2022-01-06

Breaking changes:

New features:

- Added primitive parsers `oneOfCodePoints` and `noneOfCodePoints` - `CodePoint`
  versions of `oneOf` and `noneOf` respectively. (#127 by @fsoikin)

Bugfixes:

Other improvements:
- Added `purs-tidy` formatter (#126 by @thomashoneyman)

## [v7.0.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v7.0.1) - 2021-11-17

Other improvements:

- Split license file (#125 by @maxdeviant)

## [v7.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v7.0.0) - 2021-10-06

Breaking changes:
- `anyChar` no longer always succeeds. It will only succeed on a Basic Multilingual Plane character. The new parser `anyCodePoint` will always succeed. (#119 by @jamesdbrock)
- Deleted the `StringLike` typeclass. Users must delete all `StringLike` constraints. (#119 by @jamesdbrock)
- Moved `updatePosString` to the `String` module and don’t export it. (#119 by @jamesdbrock)
- Changed the definition of `whiteSpace` and `skipSpaces` to `Data.CodePoint.Unicode.isSpace`. (#119 by @jamesdbrock)

New features:
- Added primitive parsers `anyCodePoint` and `satisfyCodePoint` for parsing `CodePoint`s. (#119 by @jamesdbrock)
- Added `match` combinator (#119 by @jamesdbrock)

Bugfixes:
- Ensure Unicode correctness (#119 by @jamesdbrock)

Other improvements:
- Added benchmark suite (#119 by @jamesdbrock)
- Split the dev dependencies out into `spago-dev.dhall`.

## [v6.0.2](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v6.0.2) - 2021-05-09

Bugfixes:
- Reverted combinator implementation changes from #102 (@robertdp, https://github.com/purescript-contrib/purescript-parsing/pull/116)

## [v6.0.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v6.0.1) - 2021-04-20

Other improvements:
- Removed unused names found by v0.14.1 PureScript release (#112 by @JordanMartinez)
- Installed dependencies directly imported into source code that were previously installed transitively (#112 by @JordanMartinez)

## [v6.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Improved performance of `string` and update `StringLike` to have `stripPrefix` as a class member instead of `indexOf` (#93)
- Non-empty combinators now return `NonEmptyList` (#102)
- Added support for PureScript 0.14 and dropped support for all previous versions (#101)

New features:
- Derived `Generic` instance of Position (#87)

Bugfixes:

Other improvements:
- Updated code to use `Data.Char.Unicode.hexDigitToInt` and `Data.Char.Unicode.isDecDigit` instead of`Data.Char.Unicode.digitToInt` and `Data.Char.Unicode.isDigit`, as those two functions have been deprecated in the `unicode` library (#103)
- Changed default branch to `main` from `master`
- Updated to comply with Contributors library guidelines by adding new issue and pull request templates, updating documentation, and migrating to Spago for local development and CI (#98)

## [v5.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.1.0) - 2020-10-08

New features:

- Added a `region` function similar to the [function of the same name in megaparsec](https://hackage.haskell.org/package/megaparsec-9.0.0/docs/Text-Megaparsec.html#v:region) which specifies how to process `ParseError`s which occur inside the indicated region (@jamesdbrock in #97 and #96).

Bugfixes:

- Fixed the token definitions for the Java language parser (@matoruru in #86)

## [v5.0.3](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.0.3) - 2019-05-06

Other improvements:

- Updated escape sequences to match parsing in the latest version of the PureScript compiler (@natefaubion)
- Added a documentation comment about stack safety (@Dretch)

## [v5.0.2](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.0.2) - 2018-11-15

Bugixes:

- Docs: Updated typos in readme and documentation (@justinwoo, @rndnoise)

## [v5.0.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v5.0.1) - 2018-06-23

Other improvements:

- Added metadata including contributor guidelines and pushed latest release to Pursuit

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

- Fixed some issues with error messages (@safareli)

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

- Updated for PureScript 0.10
- Switch to `transformers`-based API, add new instances
- Use some more efficient data types
- Add port of `indents` Haskell package (@starkstark)
- Switched to BSD 3-clause license to be compatible with ported Haskell code

## [v1.0.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v1.0.0) - 2016-06-10

Breaking changes:

- Updated for the 1.0 core libraries.

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

- Improved error message (@epost)

## [v0.7.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.7.0) - 2015-09-18

Other improvements:

- Bumped `transformers` dependency.

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

- Updated for PureScript 0.7. This release works with versions 0.7.\* of the PureScript compiler. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.3.1](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.3.1) - 2015-03-04

- Added `token`, `match` and `when` (@cryogenian)

## [v0.3.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.3.0) - 2015-02-21

- Updated dependencies. **Note: This release requires PureScript v0.6.8 or later**

## [v0.2.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.2.0) - 2015-01-10

- Updated dependencies (@garyb)

## [v0.1.4](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.1.4) - 2014-11-18

- Bumped for `Identity` changes.

## [v0.1.0](https://github.com/purescript-contrib/purescript-parsing/releases/tag/v0.1.0) - 2014-08-11

- Initial semver release.
