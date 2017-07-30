-- | Primitive parsers for working with an `Stream` input.

module Text.Parsing.Parser.Stream where

import Control.Monad.State (modify, gets)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (fold, elem, notElem)
import Data.List as L
import Data.List.Lazy as LazyL
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String as S
import Prelude hiding (between)
import Text.Parsing.Parser (ParseState(..), ParserT, fail)
import Text.Parsing.Parser.Combinators (tryRethrow, (<?>))
import Text.Parsing.Parser.Pos (Position, updatePosString, updatePosChar)

-- | A newtype used to identify a prefix of a stream
newtype Prefix a = Prefix a

derive instance eqPrefix :: Eq a => Eq (Prefix a)
derive instance ordPrefix :: Ord a => Ord (Prefix a)
derive instance newtypePrefix :: Newtype (Prefix a) _

instance showPrefix :: Show a => Show (Prefix a) where
  show (Prefix s) = "(Prefix " <> show s <> ")"

class HasUpdatePosition a where
  updatePos :: Position -> a -> Position

instance stringHasUpdatePosition :: HasUpdatePosition String where
  updatePos = updatePosString

instance charHasUpdatePosition :: HasUpdatePosition Char where
  updatePos = updatePosChar

-- | This class exists to abstract over streams which support the string-like
-- | operations which this modules needs.
-- |
-- | Instances must satisfy the following laws:
-- | - `stripPrefix (Prefix a) a >>= uncons = Nothing`
class Stream s m t | s -> t where
  uncons :: s -> m (Maybe { head :: t, tail :: s, updatePos :: Position -> Position })
  stripPrefix :: Prefix s -> s -> m (Maybe {  rest :: s, updatePos :: Position -> Position })

instance stringStream :: (Applicative m) =>  Stream String m Char where
  uncons f = pure $ S.uncons f <#> \({ head, tail}) ->
    { head, tail, updatePos: (_ `updatePos` head)}
  stripPrefix (Prefix p) s = pure $ S.stripPrefix (S.Pattern p) s <#> \rest ->
    { rest, updatePos: (_ `updatePos` p)}

instance listStream :: (Applicative m, Eq a, HasUpdatePosition a) => Stream (L.List a) m a where
  uncons f = pure $ L.uncons f <#> \({ head, tail}) ->
    { head, tail, updatePos: (_ `updatePos` head)}
  stripPrefix (Prefix p) s = pure $ L.stripPrefix (L.Pattern p) s <#> \rest ->
    { rest, updatePos: unwrap (fold (p <#> (flip updatePos >>> Endo)))}

instance lazyListStream :: (Applicative m, Eq a, HasUpdatePosition a) => Stream (LazyL.List a) m a where
  uncons f = pure $ LazyL.uncons f <#> \({ head, tail}) ->
    { head, tail, updatePos: (_ `updatePos` head)}
  stripPrefix (Prefix p) s = pure $ LazyL.stripPrefix (LazyL.Pattern p) s <#> \rest ->
    { rest, updatePos: unwrap (fold (p <#> (flip updatePos >>> Endo)))}

-- | Match end of stream.
eof :: forall s t m. Stream s m t => Monad m => ParserT s m Unit
eof = do
  input <- gets \(ParseState input _ _) -> input
  (lift $ uncons input) >>= case _ of
    Nothing -> pure unit
    _ -> fail "Expected EOF"

-- | Match the specified prefix.
prefix :: forall f c m. Stream f m c => Show f => Monad m => f -> ParserT f m f
prefix p = do
  input <- gets \(ParseState input _ _) -> input
  (lift $ stripPrefix (Prefix p) input) >>= case _ of
    Just {rest, updatePos} -> do
      modify \(ParseState _ position _) ->
        ParseState rest (updatePos position) true
      pure p
    _ -> fail ("Expected " <> show p)

-- | Match any token.
token :: forall s t m. Stream s m t => Monad m => ParserT s m t
token = do
  input <- gets \(ParseState input _ _) -> input
  (lift $ uncons input) >>= case _ of
    Nothing -> fail "Unexpected EOF"
    Just ({ head, updatePos, tail }) -> do
      modify \(ParseState _ position _) ->
        ParseState tail (updatePos position) true
      pure head

-- | Match a token satisfying the specified predicate.
satisfy :: forall s t m. Stream s m t => Show t => Monad m => (t -> Boolean) -> ParserT s m t
satisfy f = tryRethrow do
  c <- token
  if f c then pure c
         else fail $ "Character " <> show c <> " did not satisfy predicate"

-- | Match the specified token
match :: forall s t m. Stream s m t => Eq t => Show t => Monad m => t -> ParserT s m t
match c = satisfy (_ == c) <?> show c


-- | Match one of the tokens in the array.
oneOf :: forall s t m. Stream s m t => Show t => Eq t => Monad m => Array t -> ParserT s m t
oneOf ss = satisfy (flip elem ss) <?> ("one of " <> show ss)

-- | Match any token not in the array.
noneOf :: forall s t m. Stream s m t => Show t => Eq t => Monad m => Array t -> ParserT s m t
noneOf ss = satisfy (flip notElem ss) <?> ("none of " <> show ss)
