-- | Primitive parsers for working with an `Stream` input.

module Text.Parsing.Parser.Stream where

import Control.Monad.State (put, get)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (foldl, elem, notElem)
import Data.List as L
import Data.List.Lazy as LazyL
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.String as S
import Data.Tuple (Tuple(..))
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
-- | operations with position tracking, which this modules needs.
-- |
-- | Instances must satisfy the following laws:
-- | - `stripPrefix (Prefix input) {input, position} >>= uncons = Nothing`

class Stream s m t | s -> t where
  uncons :: forall r. ParserCursor s r -> m (Maybe (Tuple t (ParserCursor s r)))
  stripPrefix :: forall r. Prefix s -> ParserCursor s r -> m (Maybe (ParserCursor s r))

-- Part or ParseState which is exposed to Stream instances
type ParserCursor s r = { input :: s, pos :: Position | r}


instance stringStream :: (Applicative m) => Stream String m Char where
  uncons state = pure $ S.uncons state.input <#> \({ head, tail}) ->
    Tuple head state{input = tail, pos = updatePos state.pos head }
  stripPrefix (Prefix p) state = pure $ S.stripPrefix (S.Pattern p) state.input <#> \rest ->
    state{input = rest, pos = updatePos state.pos p}

instance listStream :: (Applicative m, Eq a, HasUpdatePosition a) => Stream (L.List a) m a where
  uncons state = pure $ L.uncons state.input <#> \({ head, tail}) ->
    Tuple head state{input = tail, pos = updatePos state.pos head }
  stripPrefix (Prefix p) state = pure $ L.stripPrefix (L.Pattern p) state.input <#> \rest ->
    state{input = rest, pos = foldl updatePos state.pos p}

instance lazyListStream :: (Applicative m, Eq a, HasUpdatePosition a) => Stream (LazyL.List a) m a where
  uncons state = pure $ LazyL.uncons state.input <#> \({ head, tail}) ->
    Tuple head state{input = tail, pos = updatePos state.pos head }
  stripPrefix (Prefix p) state = pure $ LazyL.stripPrefix (LazyL.Pattern p) state.input <#> \rest ->
    state{input = rest, pos = foldl updatePos state.pos p}

-- | Match end of stream.
eof :: forall s t m. Stream s m t => Monad m => ParserT s m Unit
eof = do
  ParseState state <- get
  (lift $ uncons state) >>= case _ of
    Nothing -> pure unit
    _ -> fail "Expected EOF"

-- | Match the specified prefix.
prefix :: forall f c m. Stream f m c => Show f => Monad m => f -> ParserT f m f
prefix p = do
  ParseState state <- get
  (lift $ stripPrefix (Prefix p) state) >>= case _ of
    Nothing -> fail $ "Expected " <> show p
    Just nextState -> do
      put $ ParseState nextState{consumed = true}
      pure p

-- | Match any token.
token :: forall s t m. Stream s m t => Monad m => ParserT s m t
token = do
  ParseState state <- get
  (lift $ uncons state) >>= case _ of
    Nothing -> fail "Unexpected EOF"
    Just (Tuple head nextState) -> do
      put $ ParseState nextState{consumed = true}
      pure head

-- | Match a token satisfying the specified predicate.
satisfy :: forall s t m. Stream s m t => Show t => Monad m => (t -> Boolean) -> ParserT s m t
satisfy f = tryRethrow do
  c <- token
  if f c then pure c
         else fail $ "Token " <> show c <> " did not satisfy predicate"

-- | Match the specified token
match :: forall s t m. Stream s m t => Eq t => Show t => Monad m => t -> ParserT s m t
match c = satisfy (_ == c) <?> show c


-- | Match one of the tokens in the array.
oneOf :: forall s t m. Stream s m t => Show t => Eq t => Monad m => Array t -> ParserT s m t
oneOf ss = satisfy (flip elem ss) <?> ("one of " <> show ss)

-- | Match any token not in the array.
noneOf :: forall s t m. Stream s m t => Show t => Eq t => Monad m => Array t -> ParserT s m t
noneOf ss = satisfy (flip notElem ss) <?> ("none of " <> show ss)
