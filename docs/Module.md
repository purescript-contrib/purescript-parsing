# Module Documentation

## Module Text.Parsing.Parser

#### `ParserT`

``` purescript
newtype ParserT s m a
  = ParserT (s -> m { consumed :: Boolean, result :: Either ParseError a, input :: s })
```


#### `unParserT`

``` purescript
unParserT :: forall m s a. ParserT s m a -> s -> m { consumed :: Boolean, result :: Either ParseError a, input :: s }
```


#### `runParserT`

``` purescript
runParserT :: forall m s a. (Monad m) => s -> ParserT s m a -> m (Either ParseError a)
```


#### `Parser`

``` purescript
type Parser s a = ParserT s Identity a
```


#### `runParser`

``` purescript
runParser :: forall s a. s -> Parser s a -> Either ParseError a
```


#### `functorParserT`

``` purescript
instance functorParserT :: (Functor m) => Functor (ParserT s m)
```


#### `applyParserT`

``` purescript
instance applyParserT :: (Monad m) => Apply (ParserT s m)
```


#### `applicativeParserT`

``` purescript
instance applicativeParserT :: (Monad m) => Applicative (ParserT s m)
```


#### `altParserT`

``` purescript
instance altParserT :: (Monad m) => Alt (ParserT s m)
```


#### `plusParserT`

``` purescript
instance plusParserT :: (Monad m) => Plus (ParserT s m)
```


#### `alternativeParserT`

``` purescript
instance alternativeParserT :: (Monad m) => Alternative (ParserT s m)
```


#### `bindParserT`

``` purescript
instance bindParserT :: (Monad m) => Bind (ParserT s m)
```


#### `monadParserT`

``` purescript
instance monadParserT :: (Monad m) => Monad (ParserT s m)
```


#### `monadPlusParserT`

``` purescript
instance monadPlusParserT :: (Monad m) => MonadPlus (ParserT s m)
```


#### `monadTransParserT`

``` purescript
instance monadTransParserT :: MonadTrans (ParserT s)
```


#### `monadStateParserT`

``` purescript
instance monadStateParserT :: (Monad m) => MonadState s (ParserT s m)
```


#### `lazyParserT`

``` purescript
instance lazyParserT :: Lazy (ParserT s m a)
```


#### `consume`

``` purescript
consume :: forall s m. (Monad m) => ParserT s m Unit
```


#### `ParseError`

``` purescript
newtype ParseError
  = ParseError String
```


#### `errorParseError`

``` purescript
instance errorParseError :: Error ParseError
```


#### `showParseError`

``` purescript
instance showParseError :: Show ParseError
```


#### `fail`

``` purescript
fail :: forall m s a. (Monad m) => String -> ParserT s m a
```



## Module Text.Parsing.Parser.Combinators

#### `(<?>)`

``` purescript
(<?>) :: forall m s a. (Monad m) => ParserT s m a -> String -> ParserT s m a
```


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
sepBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
```


#### `sepBy1`

``` purescript
sepBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
```


#### `sepEndBy`

``` purescript
sepEndBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
```


#### `sepEndBy1`

``` purescript
sepEndBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
```


#### `endBy1`

``` purescript
endBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
```


#### `endBy`

``` purescript
endBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]
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
choice :: forall m s a. (Monad m) => [ParserT s m a] -> ParserT s m a
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
manyTill :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m [a]
```


#### `many1Till`

``` purescript
many1Till :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m [a]
```



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
type OperatorTable m s a = [[Operator m s a]]
```


#### `SplitAccum`

``` purescript
type SplitAccum m s a = { postfix :: [ParserT s m (a -> a)], prefix :: [ParserT s m (a -> a)], nassoc :: [ParserT s m (a -> a -> a)], lassoc :: [ParserT s m (a -> a -> a)], rassoc :: [ParserT s m (a -> a -> a)] }
```


#### `splitOp`

``` purescript
splitOp :: forall m s a. Operator m s a -> SplitAccum m s a -> SplitAccum m s a
```


#### `rassocP`

``` purescript
rassocP :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
```


#### `rassocP1`

``` purescript
rassocP1 :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
```


#### `lassocP`

``` purescript
lassocP :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
```


#### `lassocP1`

``` purescript
lassocP1 :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
```


#### `nassocP`

``` purescript
nassocP :: forall m a b c d e s. (Monad m) => a -> ParserT s m (a -> d -> e) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> d) -> ParserT s m e
```


#### `termP`

``` purescript
termP :: forall m s a b c. (Monad m) => ParserT s m (a -> b) -> ParserT s m a -> ParserT s m (b -> c) -> ParserT s m c
```


#### `buildExprParser`

``` purescript
buildExprParser :: forall m s a. (Monad m) => OperatorTable m s a -> ParserT s m a -> ParserT s m a
```



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
oneOf :: forall s m a. (Monad m) => [String] -> ParserT String m String
```


#### `noneOf`

``` purescript
noneOf :: forall s m a. (Monad m) => [String] -> ParserT String m String
```



## Module Text.Parsing.Parser.Token

#### `token`

``` purescript
token :: forall m a. (Monad m) => ParserT [a] m a
```


#### `when`

``` purescript
when :: forall m a. (Monad m) => (a -> Boolean) -> ParserT [a] m a
```


#### `match`

``` purescript
match :: forall a m. (Monad m, Eq a) => a -> ParserT [a] m a
```


#### `LanguageDef`

``` purescript
type LanguageDef s m = { caseSensitive :: Boolean, reservedOpNames :: [String], reservedNames :: [String], opLetter :: ParserT s m String, opStart :: ParserT s m String, identLetter :: ParserT s m String, identStart :: ParserT s m String, nestedComments :: Boolean, commentLine :: String, commentEnd :: String, commentStart :: String }
```


#### `TokenParser`

``` purescript
type TokenParser s m = { commaSep1 :: forall a. ParserT s m a -> ParserT s m [a], commaSep :: forall a. ParserT s m a -> ParserT s m [a], semiSep1 :: forall a. ParserT s m a -> ParserT s m [a], semiSep :: forall a. ParserT s m a -> ParserT s m [a], dot :: ParserT s m String, colon :: ParserT s m String, comma :: ParserT s m String, semi :: ParserT s m String, brackets :: forall a. ParserT s m a -> ParserT s m a, angles :: forall a. ParserT s m a -> ParserT s m a, braces :: forall a. ParserT s m a -> ParserT s m a, parens :: forall a. ParserT s m a -> ParserT s m a, whiteSpace :: ParserT s m {  }, lexme :: forall a. ParserT s m a -> ParserT s m a, symbol :: String -> ParserT s m Number, octal :: ParserT s m Number, hexadecimal :: ParserT s m Number, decimal :: ParserT s m Number, naturalOrFloat :: ParserT s m Number, float :: ParserT s m Number, integer :: ParserT s m Number, natural :: ParserT s m Number, stringLiteral :: ParserT s m String, charLiteral :: ParserT s m String, reservedOp :: String -> ParserT s m String, operator :: ParserT s m String, reserved :: String -> ParserT s m String, identifier :: ParserT s m String }
```