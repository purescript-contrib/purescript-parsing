## Module Text.Parsing.Parser.String

Primitive parsers for working with an input stream of type `String`.

#### `eof`

``` purescript
eof :: forall m. (Monad m) => ParserT String m Unit
```

Match end-of-file.

#### `string`

``` purescript
string :: forall m. (Monad m) => String -> ParserT String m String
```

Match the specified string.

#### `anyChar`

``` purescript
anyChar :: forall m. (Monad m) => ParserT String m Char
```

Match any character.

#### `satisfy`

``` purescript
satisfy :: forall m. (Monad m) => (Char -> Boolean) -> ParserT String m Char
```

Match a character satisfying the specified predicate.

#### `char`

``` purescript
char :: forall m. (Monad m) => Char -> ParserT String m Char
```

Match the specified character

#### `whiteSpace`

``` purescript
whiteSpace :: forall m. (Monad m) => ParserT String m String
```

Match a whitespace character.

#### `skipSpaces`

``` purescript
skipSpaces :: forall m. (Monad m) => ParserT String m Unit
```

Skip whitespace characters.

#### `oneOf`

``` purescript
oneOf :: forall m. (Monad m) => Array Char -> ParserT String m Char
```

Match one of the characters in the array.

#### `noneOf`

``` purescript
noneOf :: forall m. (Monad m) => Array Char -> ParserT String m Char
```

Match any character not in the array.


