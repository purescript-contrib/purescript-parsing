## Module Text.Parsing.Parser.Expr

#### `Assoc`

``` purescript
data Assoc
  = AssocNone
  | AssocLeft
  | AssocRight
```

#### `Operator`

``` purescript
data Operator m s a
  = Infix (ParserT s m (a -> a -> a)) Assoc
  | Prefix (ParserT s m (a -> a))
  | Postfix (ParserT s m (a -> a))
```

#### `OperatorTable`

``` purescript
type OperatorTable m s a = Array (Array (Operator m s a))
```

#### `buildExprParser`

``` purescript
buildExprParser :: forall m s a. (Monad m) => OperatorTable m s a -> ParserT s m a -> ParserT s m a
```

Build a parser from an `OperatorTable`.

For example:

```purescript
buildExprParser [ [ Infix (string "/" $> div) AssocRight ]
                , [ Infix (string "*" $> mul) AssocRight ]
                , [ Infix (string "-" $> sub) AssocRight ]
                , [ Infix (string "+" $> add) AssocRight ]
                ] digit
```


