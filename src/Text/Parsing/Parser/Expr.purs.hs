module Text.Parsing.Parser.Expr where

import Prelude
import Data.Array
import Data.Either
import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators

data Assoc = AssocNone | AssocLeft | AssocRight

data Operator s a = Infix   (Parser s (a -> a -> a)) Assoc |
                    Prefix  (Parser s (a -> a)) |
                    Postfix (Parser s (a -> a))
                    
type OperatorTable s a = [[Operator s a]]

type SplitAccum s a = { rassoc :: [Parser s (a -> a -> a)], lassoc :: [Parser s (a -> a -> a)], nassoc :: [Parser s (a -> a -> a)], prefix :: [Parser s (a -> a)], postfix :: [Parser s (a -> a)] }

splitOp :: forall s a. SplitAccum s a -> Operator s a -> SplitAccum s a
splitOp accum (Infix op AssocNone)  = accum { nassoc = op:accum.nassoc }
splitOp accum (Infix op AssocLeft)  = accum { lassoc = op:accum.lassoc }
splitOp accum (Infix op AssocRight) = accum { rassoc = op:accum.rassoc }            
splitOp accum (Prefix  op) = accum { prefix = op:accum.prefix }
splitOp accum (Postfix op) = accum { postfix = op:accum.postfix }

rassocP x rassocOp prefixP term postfixP = do
  f <- rassocOp
  y <- do 
    z <- termP prefixP term postfixP
    rassocP1 z rassocOp prefixP term postfixP
  return (f x y)

rassocP1 x rassocOp prefixP term postfixP = rassocP x rassocOp prefixP term postfixP <|> return x

lassocP x lassocOp prefixP term postfixP = do
  f <- lassocOp
  y <- termP prefixP term postfixP
  lassocP1 (f x y) lassocOp prefixP term postfixP

lassocP1 x lassocOp prefixP term postfixP = lassocP x lassocOp prefixP term postfixP <|> return x

nassocP x nassocOp prefixP term postfixP = do
  f <- nassocOp
  y <- termP prefixP term postfixP
  return (f x y)

termP :: forall s a b c. Parser s (a -> b) -> Parser s a -> Parser s (b -> c) -> Parser s c
termP prefixP term postfixP = do
  pre   <- prefixP
  x     <- term
  post  <- postfixP
  return (post (pre x))

buildExprParser :: forall s a. OperatorTable s a -> Parser s a -> Parser s a
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
