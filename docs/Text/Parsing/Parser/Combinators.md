## Module Text.Parsing.Parser.Combinators

Combinators for creating parsers.

### Notes:
A few of the known combinators from Parsec are missing in this module. That
is because they have already been defined in other libraries.

```purescript
Text.Parsec.many  = Data.(Array|List).many
Text.Parsec.many1 = Data.(Array|List).some
Text.Parsec.(<|>) = Control.Alt.alt (<|>)
```

Because Strings are not Char Arrays in PureScript `many` and `some` on Char Parsers need to
be used in conjunction with `Data.String.fromCharArray` to achieve "Parsec-like" results.

```purescript
Text.Parsec.many  (char 'x') <=> fromCharArray <$> Data.Array.many (char 'x')
```

===

#### `(<?>)`

``` purescript
(<?>) :: forall m s a. (Monad m) => ParserT s m a -> String -> ParserT s m a
```

_left-associative / precedence -1_

Provide an error message in the case of failure.

#### `between`

``` purescript
between :: forall m s a open close. (Monad m) => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
```

Wrap a parser with opening and closing markers.

For example:

```purescript
parens = between (string "(") (string ")")
```

#### `option`

``` purescript
option :: forall m s a. (Monad m) => a -> ParserT s m a -> ParserT s m a
```

Provide a default result in the case where a parser fails without consuming input.

#### `optional`

``` purescript
optional :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m Unit
```

Optionally parse something, failing quietly.

#### `optionMaybe`

``` purescript
optionMaybe :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (Maybe a)
```

Return `Nothing` in the case where a parser fails without consuming input.

#### `try`

``` purescript
try :: forall m s a. (Functor m) => ParserT s m a -> ParserT s m a
```

In case of failure, reset the stream to the unconsumed state.

#### `sepBy`

``` purescript
sepBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

Parse phrases delimited by a separator.

For example:

```purescript
digit `sepBy` string ","
```

#### `sepBy1`

``` purescript
sepBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

Parse phrases delimited by a separator, requiring at least one match.

#### `sepEndBy`

``` purescript
sepEndBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

Parse phrases delimited and optionally terminated by a separator.

#### `sepEndBy1`

``` purescript
sepEndBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

Parse phrases delimited and optionally terminated by a separator, requiring at least one match.

#### `endBy1`

``` purescript
endBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

Parse phrases delimited and terminated by a separator, requiring at least one match.

#### `endBy`

``` purescript
endBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

Parse phrases delimited and terminated by a separator.

#### `chainr`

``` purescript
chainr :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
```

Parse phrases delimited by a right-associative operator.

For example:

```purescript
chainr digit (string "+" *> add) 0
```

#### `chainl`

``` purescript
chainl :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
```

Parse phrases delimited by a left-associative operator.

#### `chainl1`

``` purescript
chainl1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
```

Parse phrases delimited by a left-associative operator, requiring at least one match.

#### `chainl1'`

``` purescript
chainl1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
```

#### `chainr1`

``` purescript
chainr1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
```

Parse phrases delimited by a right-associative operator, requiring at least one match.

#### `chainr1'`

``` purescript
chainr1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
```

#### `choice`

``` purescript
choice :: forall f m s a. (Foldable f, Monad m) => f (ParserT s m a) -> ParserT s m a
```

Parse one of a set of alternatives.

#### `skipMany`

``` purescript
skipMany :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
```

Skip many instances of a phrase.

#### `skipMany1`

``` purescript
skipMany1 :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
```

Skip at least one instance of a phrase.

#### `lookAhead`

``` purescript
lookAhead :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m a
```

Parse a phrase, without modifying the consumed state or stream position.

#### `notFollowedBy`

``` purescript
notFollowedBy :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
```

Fail if the specified parser matches.

#### `manyTill`

``` purescript
manyTill :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m (List a)
```

Parse several phrases until the specified terminator matches.

#### `many1Till`

``` purescript
many1Till :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m (List a)
```

Parse several phrases until the specified terminator matches, requiring at least one match.


