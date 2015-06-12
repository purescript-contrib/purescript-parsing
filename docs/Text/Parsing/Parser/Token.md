## Module Text.Parsing.Parser.Token

#### `token`

``` purescript
token :: forall m a. (Monad m) => (a -> Position) -> ParserT (List a) m a
```

#### `when`

``` purescript
when :: forall m a. (Monad m) => (a -> Position) -> (a -> Boolean) -> ParserT (List a) m a
```

#### `match`

``` purescript
match :: forall a m. (Monad m, Eq a) => (a -> Position) -> a -> ParserT (List a) m a
```


