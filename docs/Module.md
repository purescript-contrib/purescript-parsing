# Module Documentation

## Module Text.Parsing.Parser

### Types

    data ParseError where
      ParseError :: { message :: String } -> ParseError

    type Parser s a = ParserT s Identity a

    newtype ParserT s m a where
      ParserT :: (s -> m { consumed :: Boolean, result :: Either ParseError a, input :: s }) -> ParserT s m a


### Type Class Instances

    instance altParserT :: (Monad m) => Alt (ParserT s m)

    instance alternativeParserT :: (Monad m) => Alternative (ParserT s m)

    instance applicativeParserT :: (Monad m) => Applicative (ParserT s m)

    instance applyParserT :: (Monad m) => Apply (ParserT s m)

    instance bindParserT :: (Monad m) => Bind (ParserT s m)

    instance errorParseError :: Error ParseError

    instance functorParserT :: (Functor m) => Functor (ParserT s m)

    instance lazy1ParserT :: Lazy1 (ParserT s m)

    instance monadParserT :: (Monad m) => Monad (ParserT s m)

    instance monadPlusParserT :: (Monad m) => MonadPlus (ParserT s m)

    instance monadStateParserT :: (Monad m) => MonadState s (ParserT s m)

    instance monadTransParserT :: MonadTrans (ParserT s)

    instance plusParserT :: (Monad m) => Plus (ParserT s m)

    instance showParseError :: Show ParseError


### Values

    consume :: forall s m. (Monad m) => ParserT s m Unit

    fail :: forall m s a. (Monad m) => String -> ParserT s m a

    runParser :: forall s a. s -> Parser s a -> Either ParseError a

    runParserT :: forall m s a. (Monad m) => s -> ParserT s m a -> m (Either ParseError a)

    unParserT :: forall m s a. ParserT s m a -> s -> m { consumed :: Boolean, result :: Either ParseError a, input :: s }


## Module Text.Parsing.Parser.Combinators

### Values

    (<?>) :: forall m s a. (Monad m) => ParserT s m a -> String -> ParserT s m a

    between :: forall m s a open close. (Monad m) => ParserT s m open -> ParserT s m close -> ParserT s m a -> ParserT s m a

    chainl :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a

    chainl1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a

    chainl1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a

    chainr :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a

    chainr1 :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> ParserT s m a

    chainr1' :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m (a -> a -> a) -> a -> ParserT s m a

    choice :: forall m s a. (Monad m) => [ParserT s m a] -> ParserT s m a

    endBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]

    endBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]

    lookAhead :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m a

    many1Till :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m [a]

    manyTill :: forall s a m e. (Monad m) => ParserT s m a -> ParserT s m e -> ParserT s m [a]

    option :: forall m s a. (Monad m) => a -> ParserT s m a -> ParserT s m a

    optionMaybe :: forall m s a. (Functor m, Monad m) => ParserT s m a -> ParserT s m (Maybe a)

    optional :: forall m s a. (Monad m) => ParserT s m a -> ParserT s m Unit

    sepBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]

    sepBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]

    sepEndBy :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]

    sepEndBy1 :: forall m s a sep. (Monad m) => ParserT s m a -> ParserT s m sep -> ParserT s m [a]

    skipMany :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit

    skipMany1 :: forall s a m. (Monad m) => ParserT s m a -> ParserT s m Unit

    try :: forall m s a. (Functor m) => ParserT s m a -> ParserT s m a


## Module Text.Parsing.Parser.Expr

### Types

    data Assoc where
      AssocNone :: Assoc
      AssocLeft :: Assoc
      AssocRight :: Assoc

    data Operator m s a where
      Infix :: ParserT s m (a -> a -> a) -> Assoc -> Operator m s a
      Prefix :: ParserT s m (a -> a) -> Operator m s a
      Postfix :: ParserT s m (a -> a) -> Operator m s a

    type OperatorTable m s a = [[Operator m s a]]

    type SplitAccum m s a = { postfix :: [ParserT s m (a -> a)], prefix :: [ParserT s m (a -> a)], nassoc :: [ParserT s m (a -> a -> a)], lassoc :: [ParserT s m (a -> a -> a)], rassoc :: [ParserT s m (a -> a -> a)] }


### Values

    buildExprParser :: forall m s a. (Monad m) => OperatorTable m s a -> ParserT s m a -> ParserT s m a

    lassocP :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a

    lassocP1 :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a

    nassocP :: forall m a b c d e s. (Monad m) => a -> ParserT s m (a -> d -> e) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> d) -> ParserT s m e

    rassocP :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a

    rassocP1 :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a

    splitOp :: forall m s a. Operator m s a -> SplitAccum m s a -> SplitAccum m s a

    termP :: forall m s a b c. (Monad m) => ParserT s m (a -> b) -> ParserT s m a -> ParserT s m (b -> c) -> ParserT s m c


## Module Text.Parsing.Parser.String

### Values

    char :: forall m. (Monad m) => ParserT String m String

    eof :: forall m. (Monad m) => ParserT String m Unit

    noneOf :: forall s m a. (Monad m) => [String] -> ParserT String m String

    oneOf :: forall s m a. (Monad m) => [String] -> ParserT String m String

    satisfy :: forall m. (Monad m) => (String -> Boolean) -> ParserT String m String

    skipSpaces :: forall m. (Monad m) => ParserT String m Unit

    string :: forall m. (Monad m) => String -> ParserT String m String

    whiteSpace :: forall m. (Monad m) => ParserT String m String


## Module Text.Parsing.Parser.Token

### Types

    type LanguageDef s m = { caseSensitive :: Boolean, reservedOpNames :: [String], reservedNames :: [String], opLetter :: ParserT s m String, opStart :: ParserT s m String, identLetter :: ParserT s m String, identStart :: ParserT s m String, nestedComments :: Boolean, commentLine :: String, commentEnd :: String, commentStart :: String }

    type TokenParser s m = { commaSep1 :: forall a. ParserT s m a -> ParserT s m [a], commaSep :: forall a. ParserT s m a -> ParserT s m [a], semiSep1 :: forall a. ParserT s m a -> ParserT s m [a], semiSep :: forall a. ParserT s m a -> ParserT s m [a], dot :: ParserT s m String, colon :: ParserT s m String, comma :: ParserT s m String, semi :: ParserT s m String, brackets :: forall a. ParserT s m a -> ParserT s m a, angles :: forall a. ParserT s m a -> ParserT s m a, braces :: forall a. ParserT s m a -> ParserT s m a, parens :: forall a. ParserT s m a -> ParserT s m a, whiteSpace :: ParserT s m {  }, lexme :: forall a. ParserT s m a -> ParserT s m a, symbol :: String -> ParserT s m Number, octal :: ParserT s m Number, hexadecimal :: ParserT s m Number, decimal :: ParserT s m Number, naturalOrFloat :: ParserT s m Number, float :: ParserT s m Number, integer :: ParserT s m Number, natural :: ParserT s m Number, stringLiteral :: ParserT s m String, charLiteral :: ParserT s m String, reservedOp :: String -> ParserT s m String, operator :: ParserT s m String, reserved :: String -> ParserT s m String, identifier :: ParserT s m String }