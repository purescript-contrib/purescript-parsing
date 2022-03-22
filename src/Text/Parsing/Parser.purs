module Text.Parsing.Parser
  ( ParseError(..)
  , parseErrorMessage
  , parseErrorPosition
  , ParseState(..)
  , ParserT(..)
  , Parser
  , runParser
  , runParserT
  , hoistParserT
  , mapParserT
  , consume
  , position
  , fail
  , failWithPosition
  , region
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Error.Class (class MonadThrow, catchError, throwError)
import Control.Monad.Except (class MonadError, ExceptT(..), mapExceptT, runExceptT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT(..), evalStateT, gets, mapStateT, modify_, runStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.MonadPlus (class Alternative, class MonadPlus, class Plus)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Newtype (class Newtype, over, unwrap)
import Data.Tuple (Tuple(..))
import Text.Parsing.Parser.Pos (Position, initialPos)

-- | A parsing error, consisting of a message and position information.
data ParseError = ParseError String Position

parseErrorMessage :: ParseError -> String
parseErrorMessage (ParseError msg _) = msg

parseErrorPosition :: ParseError -> Position
parseErrorPosition (ParseError _ pos) = pos

instance showParseError :: Show ParseError where
  show (ParseError msg pos) =
    "(ParseError " <> show msg <> " " <> show pos <> ")"

derive instance eqParseError :: Eq ParseError
derive instance ordParseError :: Ord ParseError

-- | Contains the remaining input and current position.
data ParseState s = ParseState s Position Boolean
-- ParseState constructor has three parameters,
-- s: the remaining input
-- Position: the current position
-- Boolean: the consumed flag.
--
-- The consumed flag is used to implement the rule for `alt` that
-- * If the left parser fails *without consuming any input*, then backtrack and try the right parser.
-- * If the left parser fails and consumes input, then fail immediately.
--
-- https://hackage.haskell.org/package/parsec/docs/Text-Parsec.html#v:try
--
-- http://blog.ezyang.com/2014/05/parsec-try-a-or-b-considered-harmful/

-- | The Parser monad transformer.
-- |
-- | The first type argument is the stream type. Typically, this is either `String`,
-- | or some sort of token stream.
newtype ParserT s m a = ParserT (ExceptT ParseError (StateT (ParseState s) m) a)

derive instance newtypeParserT :: Newtype (ParserT s m a) _

-- | Apply a parser, keeping only the parsed result.
runParserT :: forall m s a. Monad m => s -> ParserT s m a -> m (Either ParseError a)
runParserT s p = evalStateT (runExceptT (unwrap p)) initialState
  where
  initialState = ParseState s initialPos false

-- | The `Parser` monad is a synonym for the parser monad transformer applied to the `Identity` monad.
type Parser s = ParserT s Identity

-- | Apply a parser, keeping only the parsed result.
runParser :: forall s a. s -> Parser s a -> Either ParseError a
runParser s = unwrap <<< runParserT s

hoistParserT :: forall s m n a. (m ~> n) -> ParserT s m a -> ParserT s n a
hoistParserT = mapParserT

-- | Change the underlying monad action and data type in a ParserT monad action.
mapParserT
  :: forall b n s a m
   . ( m (Tuple (Either ParseError a) (ParseState s))
       -> n (Tuple (Either ParseError b) (ParseState s))
     )
  -> ParserT s m a
  -> ParserT s n b
mapParserT = over ParserT <<< mapExceptT <<< mapStateT

instance lazyParserT :: Lazy (ParserT s m a) where
  defer f = ParserT (ExceptT (defer (runExceptT <<< unwrap <<< f)))

instance semigroupParserT :: (Monad m, Semigroup a) => Semigroup (ParserT s m a) where
  append = lift2 (<>)

instance monoidParserT :: (Monad m, Monoid a) => Monoid (ParserT s m a) where
  mempty = pure mempty

derive newtype instance functorParserT :: Functor m => Functor (ParserT s m)
derive newtype instance applyParserT :: Monad m => Apply (ParserT s m)
derive newtype instance applicativeParserT :: Monad m => Applicative (ParserT s m)
derive newtype instance bindParserT :: Monad m => Bind (ParserT s m)
derive newtype instance monadParserT :: Monad m => Monad (ParserT s m)
derive newtype instance monadRecParserT :: MonadRec m => MonadRec (ParserT s m)
derive newtype instance monadStateParserT :: Monad m => MonadState (ParseState s) (ParserT s m)
derive newtype instance monadThrowParserT :: Monad m => MonadThrow ParseError (ParserT s m)
derive newtype instance monadErrorParserT :: Monad m => MonadError ParseError (ParserT s m)

-- | The alternative `Alt` instance provides the `alt` combinator `<|>`.
-- |
-- | The expression `p_left <|> p_right` will first try the `p_left` parser and if that fails
-- | __and consumes no input__ then it will try the `p_right` parser.
-- |
-- | While we are parsing down the `p_left` branch we may reach a point where
-- | we know this is the correct branch, but we cannot parse further. At
-- | that point we want to fail the entire parse instead of trying the `p_right`
-- | branch.
-- |
-- | For example, consider this `fileParser` which can parse either an HTML
-- | file that begins with `<html>` or a shell script file that begins with `#!`.
-- |
-- | ```
-- | fileParser =
-- |   string "<html>" *> parseTheRestOfTheHtml
-- |   <|>
-- |   string "#!" *> parseTheRestOfTheScript
-- | ```
-- |
-- | If we read a file from disk and run this `fileParser` on it and the
-- | `string "<html>"` parser succeeds, then we know that the first branch
-- | is the correct branch, so we want to commit to the first branch.
-- | Even if the `parseTheRestOfTheHtml` parser fails
-- | we don’t want to try the second branch.
-- |
-- | To control the point at which we commit to the `p_left` branch
-- | use the `try` combinator and the `lookAhead` combinator and
-- | the `consume` function.
-- |
-- | The `alt` combinator works this way because it gives us good localized
-- | error messages while also allowing an efficient implementation. See
-- | [*Parsec: Direct Style Monadic Parser Combinators For The Real World*](https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/parsec-paper-letter.pdf)
-- | section __2.3 Backtracking__.
instance altParserT :: Monad m => Alt (ParserT s m) where
  alt p1 p2 = (ParserT <<< ExceptT <<< StateT) \(s@(ParseState i p _)) -> do
    Tuple e s'@(ParseState _ _ consumed) <- runStateT (runExceptT (unwrap p1)) (ParseState i p false)
    case e of
      Left _
        | not consumed -> runStateT (runExceptT (unwrap p2)) s
      _ -> pure (Tuple e s')

instance plusParserT :: Monad m => Plus (ParserT s m) where
  empty = fail "No alternative"

instance alternativeParserT :: Monad m => Alternative (ParserT s m)

instance monadPlusParserT :: Monad m => MonadPlus (ParserT s m)

instance monadTransParserT :: MonadTrans (ParserT s) where
  lift = ParserT <<< lift <<< lift

-- | Set the consumed flag.
-- |
-- | Setting the consumed flag means that we're committed to this parsing branch
-- | of an alternative (`<|>`), so that if this branch fails then we want to
-- | fail the entire parse instead of trying the other alternative.
consume :: forall s m. Monad m => ParserT s m Unit
consume = modify_ \(ParseState input pos _) ->
  ParseState input pos true

-- | Returns the current position in the stream.
position :: forall s m. Monad m => ParserT s m Position
position = gets \(ParseState _ pos _) -> pos

-- | Fail with a message.
fail :: forall m s a. Monad m => String -> ParserT s m a
fail message = failWithPosition message =<< position

-- | Fail with a message and a position.
failWithPosition :: forall m s a. Monad m => String -> Position -> ParserT s m a
failWithPosition message pos = throwError (ParseError message pos)

-- | Contextualize parsing failures inside a region. If a parsing failure
-- | occurs, then the `ParseError` will be transformed by each containing
-- | `region` as the parser backs out the call stack.
region :: forall m s a. Monad m => (ParseError -> ParseError) -> ParserT s m a -> ParserT s m a
region context p = catchError p $ \err -> throwError $ context err
