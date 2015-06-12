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

#### `SplitAccum`

``` purescript
type SplitAccum m s a = { rassoc :: List (ParserT s m (a -> a -> a)), lassoc :: List (ParserT s m (a -> a -> a)), nassoc :: List (ParserT s m (a -> a -> a)), prefix :: List (ParserT s m (a -> a)), postfix :: List (ParserT s m (a -> a)) }
```

#### `buildExprParser`

``` purescript
buildExprParser :: forall m s a. (Monad m) => OperatorTable m s a -> ParserT s m a -> ParserT s m a
```


