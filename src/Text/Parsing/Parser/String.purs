-- | Primitive parsers for working with an input stream of type `String`.

module Text.Parsing.Parser.String where

import Data.Array (many, toUnfoldable)
import Data.Foldable (fold, elem, notElem)
import Data.List as L
import Data.Monoid (class Monoid)
import Data.Monoid.Endo (Endo(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.String as S
import Data.Unfoldable (class Unfoldable)
import Control.Monad.State (modify, gets)
import Text.Parsing.Parser (ParseState(..), ParserT, fail)
import Text.Parsing.Parser.Combinators (try, (<?>))
import Text.Parsing.Parser.Pos (Position, updatePosString, updatePosChar)
import Prelude hiding (between)

-- | A newtype used in cases where there is a prefix to be stipPrefixed.
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
-- | - `stipPrefix (Prefix a) a >>= uncons = Nothing`
class StreamLike f c | f -> c where
  uncons :: f -> Maybe { head :: c, tail :: f, updatePos :: Position -> Position }
  stipPrefix :: Prefix f -> f -> Maybe {  rest :: f, updatePos :: Position -> Position }

instance stringStreamLike :: StreamLike String Char where
  uncons f = S.uncons f <#> \({ head, tail}) ->
    { head, tail, updatePos: (_ `updatePos` head)}
  stipPrefix (Prefix p) s = S.stripPrefix (S.Pattern p) s <#> \rest ->
    { rest, updatePos: (_ `updatePos` p)}

instance listcharStreamLike :: (Eq a, HasUpdatePosition a) => StreamLike (L.List a) a where
  uncons f = L.uncons f <#> \({ head, tail}) ->
    { head, tail, updatePos: (_ `updatePos` head)}
  stipPrefix (Prefix p) s = L.stripPrefix (L.Pattern p) s <#> \rest ->
    { rest, updatePos: unwrap (fold (p <#> (flip updatePos >>> Endo)))}

-- | Match end of stream.
eof :: forall f c m. StreamLike f c => Monad m => ParserT f m Unit
eof = do
  input <- gets \(ParseState input _ _) -> input
  case uncons input of
    Nothing -> pure unit
    _ -> fail "Expected EOF"

-- | Match the specified prefix.
prefix :: forall f c m. StreamLike f c => Show f => Monad m => f -> ParserT f m f
prefix str = do
  input <- gets \(ParseState input _ _) -> input
  case stipPrefix (Prefix str) input of
    Just {rest, updatePos} -> do
      modify \(ParseState _ position _) ->
        ParseState rest (updatePos position) true
      pure str
    _ -> fail ("Expected " <> show str)

-- | Match any token.
token :: forall f c m. StreamLike f c => Monad m => ParserT f m c
token = do
  input <- gets \(ParseState input _ _) -> input
  case uncons input of
    Nothing -> fail "Unexpected EOF"
    Just ({ head, updatePos, tail }) -> do
      modify \(ParseState _ position _) ->
        ParseState tail (updatePos position) true
      pure head

-- | Match a token satisfying the specified predicate.
satisfy :: forall f c m. StreamLike f c => Show c => Monad m => (c -> Boolean) -> ParserT f m c
satisfy f = try do
  c <- token
  if f c then pure c
         else fail $ "Character " <> show c <> " did not satisfy predicate"

-- | Match the specified token
match :: forall f c m. StreamLike f c => Eq c => Show c => Monad m => c -> ParserT f m c
match c = satisfy (_ == c) <?> show c


-- | Match a whitespace characters but returns them using Array.
whiteSpace :: forall f m. StreamLike f Char => Monad m => ParserT f m (Array Char)
whiteSpace = many $ satisfy \c -> c == '\n' || c == '\r' || c == ' ' || c == '\t'

-- | Skip whitespace characters.
skipSpaces :: forall f m. StreamLike f Char => Monad m => ParserT f m Unit
skipSpaces = void whiteSpace

-- | Match one of the tokens in the array.
oneOf :: forall f c m. StreamLike f c => Show c => Eq c => Monad m => Array c -> ParserT f m c
oneOf ss = satisfy (flip elem ss) <?> ("one of " <> show ss)

-- | Match any token not in the array.
noneOf :: forall f c m. StreamLike f c => Show c => Eq c => Monad m => Array c -> ParserT f m c
noneOf ss = satisfy (flip notElem ss) <?> ("none of " <> show ss)
