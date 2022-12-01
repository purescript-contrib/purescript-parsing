-- | Primitive parsers for input type `DataView`.
-- |
-- | All of these primitive parsers will consume when they succeed.
-- |
-- | All of these primitive parsers will not consume and will automatically
-- | backtrack when they fail.
-- |
-- | ### Mutable ArrayBuffer
-- |
-- | All of the parsers in this module operate on an input stream of
-- | [__Data.ArrayBuffer.Types.DataView__](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:DataView),
-- | which represents a range of a mutable
-- | [__Data.ArrayBuffer.Types.ArrayBuffer__](https://pursuit.purescript.org/packages/purescript-arraybuffer-types/docs/Data.ArrayBuffer.Types#t:ArrayBuffer).
-- |
-- | For operations for working with `DataView`, see
-- | module
-- | [__Data.ArrayBuffer.DataView__](https://pursuit.purescript.org/packages/purescript-arraybuffer/docs/Data.ArrayBuffer.DataView)
-- | in package __arraybuffer__.
-- |
-- | Reading from an `ArrayBuffer` is an `Effect`ful activity, so
-- | all parsers in this module must be run in a
-- | `MonadEffect m => ParserT DataView m` context with
-- | `Parsing.runParserT`.
-- |
-- | ### Position
-- |
-- | In a `DataView` parser, the `Position {index}` counts the number of
-- | bytes since the beginning of the input.
-- |
-- | The `Postion {line,column}` fields are unused and will remain constant *1*.
-- |
-- | ## Usage examples
-- |
-- | Parse values out of a `dataview :: Data.ArrayBuffer.Types.DataView`. All
-- | `DataView` parsing must be done in an `Effect` context. The `result` will be
-- | `Either` a parse error or the parsed value.
-- |
-- | ### Parse two numbers
-- |
-- | Parse two big-endian IEEE 754 double-precision `Number`s.
-- |
-- | ```purescript
-- | import Text.Parsing.Parser (runParserT)
-- | import Text.Parsing.Parser.DataView (anyFloat64be)
-- |
-- | do
-- |   result <- runParserT dataview do
-- |     float1 <- anyFloat64be
-- |     float2 <- anyFloat64be
-- |     pure $ Tuple float1 float2
-- | ```
-- |
-- | ### Parse an array
-- |
-- | Parse an array of `n` 32-bit big-endian signed `Int`s.
-- |
-- | ```purescript
-- | import Text.Parsing.Parser (runParserT)
-- | import Text.Parsing.Parser.DataView (anyUint32be)
-- | import Data.Unfoldable (replicateA)
-- |
-- | do
-- |   result <- runParserT dataview $ replicateA n anyInt32be
-- | ```
-- |
-- | ### Parse UTF-8
-- |
-- | Parse a UTF-8 `String` with a length prefix.
-- |
-- | We give this as an example, rather than supporting it in the library, because
-- | it depends on
-- | [__web-encoding__](https://pursuit.purescript.org/packages/purescript-web-encoding) for UTF-8.
-- |
-- | ```purescript
-- | import Control.Monad.Except (ExceptT)
-- | import Data.ArrayBuffer.Cast (toUint8Array)
-- | import Effect.Exception (catchException, message)
-- | import Parsing (runParserT, liftExceptT)
-- | import Parsing.DataView (anyUint32be, takeN)
-- | import Data.UInt (toInt)
-- | import Web.Encoding.TextDecoder as TextDecoder
-- | import Web.Encoding.UtfLabel as UtfLabel
-- |
-- | do
-- |   textDecoder <- TextDecoder.new UtfLabel.utf8
-- |
-- |   result <- runParserT dataview do
-- |     -- Parse a 32-bit big-endian length prefix for the length
-- |     -- of the UTF-8 string in bytes.
-- |     length      <- anyUint32be
-- |     stringview  <- takeN $ toInt length
-- |     liftExceptT $ ExceptT $ catchException (Left <<< message) do
-- |       stringarray <- toUint8Array stringview
-- |       Right <$> TextDecoder.decode stringarray
-- | ```
-- |
-- | ## Serialization
-- |
-- | See the package
-- | [__arraybuffer-builder__](https://pursuit.purescript.org/packages/purescript-arraybuffer-builder/)
-- | for a way to
-- | serialize and build `ArrayBuffer`s.
module Parsing.DataView
  ( takeN
  , rest
  , eof
  , match
  , anyPrim
  , anyInt8
  , anyInt16be
  , anyInt16le
  , anyInt32be
  , anyInt32le
  , anyUint8
  , anyUint16be
  , anyUint16le
  , anyUint32be
  , anyUint32le
  , anyFloat32be
  , anyFloat32le
  , anyFloat64be
  , anyFloat64le
  ) where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.ArrayBuffer.DataView (Endian(LE, BE))
import Data.ArrayBuffer.DataView (byteLength, byteOffset, get, part, buffer) as DV
import Data.ArrayBuffer.Types (DataView, Int16, Int32, Int8, Uint16, Uint32, Uint8, Float32, Float64, ByteLength)
import Data.ArrayBuffer.ValueMapping (class BytesPerType, class BinaryValue, class ShowArrayViewType, byteWidth)
import Data.Float32 as Float32
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.UInt (UInt)
import Effect.Class (class MonadEffect, liftEffect)
import Parsing (ParseState(..), ParserT, Position(..), consume, fail, getParserT, stateParserT)
import Type.Proxy (Proxy(..))

-- | Take *N* bytes starting from the current parser position. Fail
-- | if not enough bytes remain in the input. Fail if *N* is negative.
-- |
-- | #### Example
-- |
-- | Parse three bytes.
-- |
-- |     takeN 3
-- |
takeN :: forall m. MonadEffect m => ByteLength -> ParserT DataView m DataView
takeN n = do
  ParseState input (Position { index }) _ <- getParserT
  if (n < 0) then
    fail "takeN cannot take negative number of bytes"
  else if (index + n > DV.byteLength input) then
    fail "takeN expected N bytes"
  else do
    p <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + index) n
    stateParserT \_ -> Tuple p $ ParseState input (Position { line: 1, column: 1, index: index + n }) true

-- | Take the rest of the input, however many bytes remain. Always succeeds.
rest :: forall m. MonadEffect m => ParserT DataView m DataView
rest = do
  ParseState input (Position { index }) _ <- getParserT
  p <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + index)
    (DV.byteLength input - index)
  stateParserT \_ -> Tuple p $ ParseState input (Position { line: 1, column: 1, index: DV.byteLength input + 1 }) true

-- | Parse succeeds at the end of the input DataView.
eof :: forall m. Monad m => ParserT DataView m Unit
eof = do
  ParseState input (Position { index }) _ <- getParserT
  if (index + 1 > DV.byteLength input) then do
    -- We must consume so this combines correctly with notFollowedBy
    consume
  else do
    fail "eof expected end of DataView"

-- | Return both the result of a parse and the portion of the input that
-- | was consumed while it was being parsed.
match :: forall a m. MonadEffect m => ParserT DataView m a -> ParserT DataView m (Tuple DataView a)
match p = do
  ParseState input (Position { index: index0 }) _ <- getParserT
  x <- p
  ParseState _ (Position { index: index1 }) _ <- getParserT
  part <- lift $ liftEffect $ DV.part (DV.buffer input) (DV.byteOffset input + index0) (index1 - index0)
  pure $ Tuple part x

-- | Parse one fixed-bit-width `Data.ArrayBuffer.Types.ArrayViewType` primitive
-- | of a given endianness.
-- |
-- | #### Example
-- |
-- | Parse a little-endian 32-bit two’s-complement signed integer (4 bytes):
-- |
-- |     anyPrim LE (Proxy :: Proxy Int32)
-- |
-- | or just use the convenience function `anyInt32le`, see below.
anyPrim
  :: forall a name m t
   . BinaryValue a t
  => BytesPerType a
  => ShowArrayViewType a name
  => IsSymbol name
  => MonadEffect m
  => Endian
  -> Proxy a
  -> ParserT DataView m t
anyPrim endian _ = do
  ParseState input (Position { index }) _ <- getParserT
  lift (liftEffect (DV.get endian (Proxy :: Proxy a) input index)) >>= case _ of
    Nothing -> fail "anyPrim unexpected end of DataView"
    Just i -> do
      stateParserT \_ -> Tuple i $ ParseState input (Position { line: 1, column: 1, index: index + byteWidth (Proxy :: Proxy a) }) true

-- | Parse one 8-bit two’s-complement signed integer (byte).
anyInt8 :: forall m. MonadEffect m => ParserT DataView m Int
anyInt8 = anyPrim LE (Proxy :: Proxy Int8)

-- | Parse one 16-bit big-endian two’s-complement signed integer.
anyInt16be :: forall m. MonadEffect m => ParserT DataView m Int
anyInt16be = anyPrim BE (Proxy :: Proxy Int16)

-- | Parse one 16-bit little-endian two’s-complement signed integer.
anyInt16le :: forall m. MonadEffect m => ParserT DataView m Int
anyInt16le = anyPrim LE (Proxy :: Proxy Int16)

-- | Parse one 32-bit big-endian two’s-complement signed integer.
anyInt32be :: forall m. MonadEffect m => ParserT DataView m Int
anyInt32be = anyPrim BE (Proxy :: Proxy Int32)

-- | Parse one 32-bit little-endian two’s-complement signed integer.
anyInt32le :: forall m. MonadEffect m => ParserT DataView m Int
anyInt32le = anyPrim LE (Proxy :: Proxy Int32)

-- | Parse one 8-bit unsigned integer (octet).
anyUint8 :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint8 = anyPrim LE (Proxy :: Proxy Uint8)

-- | Parse one 16-bit big-endian unsigned integer.
anyUint16be :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint16be = anyPrim BE (Proxy :: Proxy Uint16)

-- | Parse one 16-bit little-endian unsigned integer.
anyUint16le :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint16le = anyPrim LE (Proxy :: Proxy Uint16)

-- | Parse one 32-bit big-endian unsigned integer.
anyUint32be :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint32be = anyPrim BE (Proxy :: Proxy Uint32)

-- | Parse one 32-bit little-endian unsigned integer.
anyUint32le :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint32le = anyPrim LE (Proxy :: Proxy Uint32)

-- | Parse one 32-bit big-endian IEEE 754 floating-point number.
anyFloat32be :: forall m. MonadEffect m => ParserT DataView m Float32.Float32
anyFloat32be = anyPrim BE (Proxy :: Proxy Float32)

-- | Parse one 32-bit little-endian IEEE 754 floating-point number.
anyFloat32le :: forall m. MonadEffect m => ParserT DataView m Float32.Float32
anyFloat32le = anyPrim LE (Proxy :: Proxy Float32)

-- | Parse one 64-bit big-endian IEEE 754 floating-point number.
anyFloat64be :: forall m. MonadEffect m => ParserT DataView m Number
anyFloat64be = anyPrim BE (Proxy :: Proxy Float64)

-- | Parse one 64-bit little-endian IEEE 754 floating-point number.
anyFloat64le :: forall m. MonadEffect m => ParserT DataView m Number
anyFloat64le = anyPrim LE (Proxy :: Proxy Float64)

-- ****************************** Notes ****************************************
--
-- We cannot have a primitive parser which parses a `DataView` and produces
-- an `ArrayView` (a Javascript Typed Array). Javascript DataViews are
-- endianess-aware, but Javascript Typed Arrays assume the native endianness
-- of the local machine. DataViews are intended to be used for I/O, ArrayViews
-- are intended to be used internally for graphics in a process, and they're
-- not intended to be both applied to the same ArrayBuffer.
-- The exception: `Uint8Array`
--
-- The failure messages are all constant strings form performance reasons.
-- If failure messages were constructed lazily then we could have more
-- descriptive messages.
