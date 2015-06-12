## Module Text.Parsing.Parser.Token

Functions for working with streams of tokens.

#### `token`

``` purescript
token :: forall m a. (Monad m) => (a -> Position) -> ParserT (List a) m a
```

Create a parser which returns the first token in the stream.

#### `when`

``` purescript
when :: forall m a. (Monad m) => (a -> Position) -> (a -> Boolean) -> ParserT (List a) m a
```

Create a parser which matches any token satisfying the predicate.

#### `match`

``` purescript
match :: forall a m. (Monad m, Eq a) => (a -> Position) -> a -> ParserT (List a) m a
```

Match the specified token at the head of the stream.


