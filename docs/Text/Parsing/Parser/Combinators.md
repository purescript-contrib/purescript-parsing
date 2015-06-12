## Module Text.Parsing.Parser.Combinators

#### `(<?>)`

``` purescript
(<?>) :: forall m s a. (Monad m) => ParserT s m a -> String -> ParserT s m a
```

_left-associative / precedence -1_

#### `between`

``` purescript
between :: forall m s a open close. (Monad m) => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a
```

#### `option`

``` purescript
option :: forall m s a. (Monad m) => a -> ParserT s m a -> ParserT s m a
```

#### `optional`

``` purescript
optional :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m Unit
```

#### `optionMaybe`

``` purescript
optionMaybe :: forall m s a. (Functor m, Monad m) => ParserT s m a -> ParserT s m (Maybe a)
```

#### `try`

``` purescript
try :: forall m s a. (Functor m) => ParserT s m a -> ParserT s m a
```

#### `sepBy`

``` purescript
sepBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

#### `sepBy1`

``` purescript
sepBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

#### `sepEndBy`

``` purescript
sepEndBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

#### `sepEndBy1`

``` purescript
sepEndBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

#### `endBy1`

``` purescript
endBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

#### `endBy`

``` purescript
endBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m (List a)
```

#### `chainr`

``` purescript
chainr :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
```

#### `chainl`

``` purescript
chainl :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
```

#### `chainl1`

``` purescript
chainl1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
```

#### `chainl1'`

``` purescript
chainl1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
```

#### `chainr1`

``` purescript
chainr1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a
```

#### `chainr1'`

``` purescript
chainr1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a
```

#### `choice`

``` purescript
choice :: forall f m s a. (Foldable f, Monad m) => f (ParserT s m a) -> ParserT s m a
```

#### `skipMany`

``` purescript
skipMany :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
```

#### `skipMany1`

``` purescript
skipMany1 :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
```

#### `lookAhead`

``` purescript
lookAhead :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m a
```

#### `notFollowedBy`

``` purescript
notFollowedBy :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit
```

#### `manyTill`

``` purescript
manyTill :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m (List a)
```

#### `many1Till`

``` purescript
many1Till :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m (List a)
```


