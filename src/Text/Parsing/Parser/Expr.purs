module Text.Parsing.Parser.Expr
  ( Assoc(..)
  , Operator(..)
  , OperatorTable()
  , buildExprParser
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Foldable (foldr, foldl)
import Data.List (List(..), (:))
import Text.Parsing.Parser (ParserT)
import Text.Parsing.Parser.Combinators (choice, (<?>))

data Assoc = AssocNone | AssocLeft | AssocRight

data Operator m s a = Infix   (ParserT s m (a -> a -> a)) Assoc
                    | Prefix  (ParserT s m (a -> a))
                    | Postfix (ParserT s m (a -> a))

type OperatorTable m s a = Array (Array (Operator m s a))

type SplitAccum m s a = { rassoc  :: List (ParserT s m (a -> a -> a))
                        , lassoc  :: List (ParserT s m (a -> a -> a))
                        , nassoc  :: List (ParserT s m (a -> a -> a))
                        , prefix  :: List (ParserT s m (a -> a))
                        , postfix :: List (ParserT s m (a -> a)) }

-- | Build a parser from an `OperatorTable`.
-- |
-- | For example:
-- |
-- | ```purescript
-- | buildExprParser [ [ Infix (string "/" $> div) AssocRight ]
-- |                 , [ Infix (string "*" $> mul) AssocRight ]
-- |                 , [ Infix (string "-" $> sub) AssocRight ]
-- |                 , [ Infix (string "+" $> add) AssocRight ]
-- |                 ] digit
-- | ```
buildExprParser :: forall m s a. Monad m => OperatorTable m s a -> ParserT s m a -> ParserT s m a
buildExprParser operators simpleExpr = foldl makeParser simpleExpr operators

makeParser :: forall m s a. Monad m => ParserT s m a -> Array (Operator m s a) -> ParserT s m a
makeParser term ops = do
  x <- termP prefixP term postfixP
  rassocP x rassocOp prefixP term postfixP
    <|> lassocP x lassocOp prefixP term postfixP
    <|> nassocP x nassocOp prefixP term postfixP
    <|> pure x
    <?> "operator"
  where
  accum = foldr splitOp { rassoc:  Nil
                        , lassoc:  Nil
                        , nassoc:  Nil
                        , prefix:  Nil
                        , postfix: Nil
                        } ops

  rassocOp  = choice accum.rassoc
  lassocOp  = choice accum.lassoc
  nassocOp  = choice accum.nassoc
  prefixOp  = choice accum.prefix <?> ""
  postfixOp = choice accum.postfix <?> ""

  postfixP = postfixOp <|> pure id
  prefixP = prefixOp <|> pure id

splitOp :: forall m s a. Operator m s a -> SplitAccum m s a -> SplitAccum m s a
splitOp (Infix op AssocNone)  accum = accum { nassoc  = op : accum.nassoc }
splitOp (Infix op AssocLeft)  accum = accum { lassoc  = op : accum.lassoc }
splitOp (Infix op AssocRight) accum = accum { rassoc  = op : accum.rassoc }
splitOp (Prefix  op)          accum = accum { prefix  = op : accum.prefix }
splitOp (Postfix op)          accum = accum { postfix = op : accum.postfix }

rassocP :: forall m a b c s. Monad m => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
rassocP x rassocOp prefixP term postfixP = do
  f <- rassocOp
  y <- do
    z <- termP prefixP term postfixP
    rassocP1 z rassocOp prefixP term postfixP
  pure (f x y)

rassocP1 :: forall m a b c s. Monad m => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
rassocP1 x rassocOp prefixP term postfixP = rassocP x rassocOp prefixP term postfixP <|> pure x

lassocP :: forall m a b c s. Monad m => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
lassocP x lassocOp prefixP term postfixP = do
  f <- lassocOp
  y <- termP prefixP term postfixP
  lassocP1 (f x y) lassocOp prefixP term postfixP

lassocP1 :: forall m a b c s. Monad m => a -> ParserT s m (a -> a -> a) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> a) -> ParserT s m a
lassocP1 x lassocOp prefixP term postfixP = lassocP x lassocOp prefixP term postfixP <|> pure x

nassocP :: forall m a b c d e s. Monad m => a -> ParserT s m (a -> d -> e) -> ParserT s m (b -> c) -> ParserT s m b -> ParserT s m (c -> d) -> ParserT s m e
nassocP x nassocOp prefixP term postfixP = do
  f <- nassocOp
  y <- termP prefixP term postfixP
  pure (f x y)

termP :: forall m s a b c. Monad m => ParserT s m (a -> b) -> ParserT s m a -> ParserT s m (b -> c) -> ParserT s m c
termP prefixP term postfixP = do
  pre   <- prefixP
  x     <- term
  post  <- postfixP
  pure (post (pre x))
