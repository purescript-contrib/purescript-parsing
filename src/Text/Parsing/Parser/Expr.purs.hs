module Text.Parsing.Parser.Expr where

import Prelude

import Data.Either
import Data.Foldable

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators

data Assoc = AssocNone | AssocLeft | AssocRight

data Operator m s a = Infix   (ParserT s m (a -> a -> a)) Assoc
                    | Prefix  (ParserT s m (a -> a))
                    | Postfix (ParserT s m (a -> a))

type OperatorTable m s a = [[Operator m s a]]

type SplitAccum m s a = { rassoc :: [ParserT s m (a -> a -> a)]
                        , lassoc :: [ParserT s m (a -> a -> a)]
                        , nassoc :: [ParserT s m (a -> a -> a)]
                        , prefix :: [ParserT s m (a -> a)]
                        , postfix :: [ParserT s m (a -> a)] }

splitOp :: forall m s a. Operator m s a -> SplitAccum m s a -> SplitAccum m s a
splitOp (Infix op AssocNone)  accum = accum { nassoc  = op:accum.nassoc }
splitOp (Infix op AssocLeft)  accum = accum { lassoc  = op:accum.lassoc }
splitOp (Infix op AssocRight) accum = accum { rassoc  = op:accum.rassoc }
splitOp (Prefix  op)          accum = accum { prefix  = op:accum.prefix }
splitOp (Postfix op)          accum = accum { postfix = op:accum.postfix }

rassocP :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
rassocP x rassocOp prefixP term postfixP = do
  f <- rassocOp
  y <- do
    z <- termP prefixP term postfixP
    rassocP1 z rassocOp prefixP term postfixP
  return (f x y)

rassocP1 :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
rassocP1 x rassocOp prefixP term postfixP = rassocP x rassocOp prefixP term postfixP <|> return x

lassocP :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
lassocP x lassocOp prefixP term postfixP = do
  f <- lassocOp
  y <- termP prefixP term postfixP
  lassocP1 (f x y) lassocOp prefixP term postfixP

lassocP1 :: forall m a b c s. (Monad m) => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
lassocP1 x lassocOp prefixP term postfixP = lassocP x lassocOp prefixP term postfixP <|> return x

nassocP :: forall m a b c d e s. (Monad m) => a -> ParserT s m (a -> d -> e) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> d) -> ParserT s m e
nassocP x nassocOp prefixP term postfixP = do
  f <- nassocOp
  y <- termP prefixP term postfixP
  return (f x y)

termP :: forall m s a b c. (Monad m) => ParserT s m (a -> b) -> ParserT s m a -> ParserT s m (b -> c) -> ParserT s m c
termP prefixP term postfixP = do
  pre   <- prefixP
  x     <- term
  post  <- postfixP
  return (post (pre x))

buildExprParser :: forall m s a. (Monad m) => OperatorTable m s a -> ParserT s m a -> ParserT s m a
buildExprParser operators simpleExpr =
  let makeParser term ops =
      let accum     = foldr splitOp { rassoc: [], lassoc: [], nassoc: [], prefix: [], postfix: [] } ops in

      let rassocOp  = choice accum.rassoc in
      let lassocOp  = choice accum.lassoc in
      let nassocOp  = choice accum.nassoc in
      let prefixOp  = choice accum.prefix <?> "" in
      let postfixOp = choice accum.postfix <?> "" in

      let postfixP = postfixOp <|> return id in
      let prefixP = prefixOp <|> return id in

      do
        x <- termP prefixP term postfixP
        rassocP x rassocOp prefixP term postfixP
          <|> lassocP x lassocOp prefixP term postfixP
          <|> nassocP x nassocOp prefixP term postfixP
          <|> return x
          <?> "operator"

  in foldl (makeParser) simpleExpr operators
