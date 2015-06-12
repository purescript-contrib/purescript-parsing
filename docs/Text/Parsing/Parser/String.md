## Module Text.Parsing.Parser.String

#### `eof`

``` purescript
eof :: forall m. (Monad m) => ParserT String m Unit
```

#### `string`

``` purescript
string :: forall m. (Monad m) => String -> ParserT String m String
```

#### `char`

``` purescript
char :: forall m. (Monad m) => ParserT String m String
```

#### `satisfy`

``` purescript
satisfy :: forall m. (Monad m) => (String -> Boolean) -> ParserT String m String
```

#### `whiteSpace`

``` purescript
whiteSpace :: forall m. (Monad m) => ParserT String m String
```

#### `skipSpaces`

``` purescript
skipSpaces :: forall m. (Monad m) => ParserT String m Unit
```

#### `oneOf`

``` purescript
oneOf :: forall s m a. (Monad m) => Array String -> ParserT String m String
```

#### `noneOf`

``` purescript
noneOf :: forall s m a. (Monad m) => Array String -> ParserT String m String
```


