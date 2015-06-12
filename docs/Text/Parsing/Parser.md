## Module Text.Parsing.Parser

#### `ParseError`

``` purescript
data ParseError
  = ParseError { message :: String, position :: Position }
```

##### Instances
``` purescript
instance errorParseError :: Error ParseError
instance showParseError :: Show ParseError
```

#### `PState`

``` purescript
data PState s
  = PState { input :: s, position :: Position }
```

`PState` contains the remaining input and current position.

#### `ParserT`

``` purescript
newtype ParserT s m a
  = ParserT (PState s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position })
```

##### Instances
``` purescript
instance functorParserT :: (Functor m) => Functor (ParserT s m)
instance applyParserT :: (Monad m) => Apply (ParserT s m)
instance applicativeParserT :: (Monad m) => Applicative (ParserT s m)
instance altParserT :: (Monad m) => Alt (ParserT s m)
instance plusParserT :: (Monad m) => Plus (ParserT s m)
instance alternativeParserT :: (Monad m) => Alternative (ParserT s m)
instance bindParserT :: (Monad m) => Bind (ParserT s m)
instance monadParserT :: (Monad m) => Monad (ParserT s m)
instance monadPlusParserT :: (Monad m) => MonadPlus (ParserT s m)
instance monadTransParserT :: MonadTrans (ParserT s)
instance monadStateParserT :: (Monad m) => MonadState s (ParserT s m)
instance lazyParserT :: Lazy (ParserT s m a)
```

#### `unParserT`

``` purescript
unParserT :: forall m s a. ParserT s m a -> PState s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
```

#### `runParserT`

``` purescript
runParserT :: forall m s a. (Monad m) => PState s -> ParserT s m a -> m (Either ParseError a)
```

#### `Parser`

``` purescript
type Parser s a = ParserT s Identity a
```

#### `runParser`

``` purescript
runParser :: forall s a. s -> Parser s a -> Either ParseError a
```

#### `consume`

``` purescript
consume :: forall s m. (Monad m) => ParserT s m Unit
```

#### `fail`

``` purescript
fail :: forall m s a. (Monad m) => String -> ParserT s m a
```

#### `parseFailed`

``` purescript
parseFailed :: forall s a. s -> Position -> String -> { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
```

Creates a failed parser state for the remaining input `s` and current position
with an error message.
Most of the time, `fail` should be used instead.


