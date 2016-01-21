## Module Text.Parsing.Parser

#### `ParseError`

``` purescript
data ParseError
  = ParseError { message :: String, position :: Position }
```

A parsing error, consisting of a message and position information.

##### Instances
``` purescript
Show ParseError
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

The Parser monad transformer.

The first type argument is the stream type. Typically, this is either `String`, or some sort of token stream.

##### Instances
``` purescript
(Functor m) => Functor (ParserT s m)
(Monad m) => Apply (ParserT s m)
(Monad m) => Applicative (ParserT s m)
(Monad m) => Alt (ParserT s m)
(Monad m) => Plus (ParserT s m)
(Monad m) => Alternative (ParserT s m)
(Monad m) => Bind (ParserT s m)
(Monad m) => Monad (ParserT s m)
(Monad m) => MonadPlus (ParserT s m)
MonadTrans (ParserT s)
(Monad m) => MonadState s (ParserT s m)
Lazy (ParserT s m a)
```

#### `unParserT`

``` purescript
unParserT :: forall m s a. ParserT s m a -> PState s -> m { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
```

Apply a parser by providing an initial state.

#### `runParserT`

``` purescript
runParserT :: forall m s a. (Monad m) => PState s -> ParserT s m a -> m (Either ParseError a)
```

Apply a parser, keeping only the parsed result.

#### `Parser`

``` purescript
type Parser s a = ParserT s Identity a
```

The `Parser` monad is a synonym for the parser monad transformer applied to the `Identity` monad.

#### `runParser`

``` purescript
runParser :: forall s a. s -> Parser s a -> Either ParseError a
```

Apply a parser, keeping only the parsed result.

#### `consume`

``` purescript
consume :: forall s m. (Monad m) => ParserT s m Unit
```

Set the consumed flag.

#### `fail`

``` purescript
fail :: forall m s a. (Monad m) => String -> ParserT s m a
```

Fail with a message.

#### `parseFailed`

``` purescript
parseFailed :: forall s a. s -> Position -> String -> { input :: s, result :: Either ParseError a, consumed :: Boolean, position :: Position }
```

Creates a failed parser state for the remaining input `s` and current position
with an error message.

Most of the time, `fail` should be used instead.


