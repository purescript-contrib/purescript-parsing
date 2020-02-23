-- | Primitive parsers for parsing Javascript ArrayBuffers.
-- |
-- | All of the parsers in this module operate on an input stream of
-- | `Data.ArrayBuffer.Types.DataView`, which represents a range of an
-- | `Data.ArrayBuffer.Types.ArrayBuffer`.
-- |
-- | For operations for working with `ArrayBuffer` and `DataView`, see
-- | module `Data.ArrayBuffer.DataView` in package __purescript-arraybuffer__.
-- |
-- | Reading and writing `ArrayBuffer` is an `Effect`ful activity, so
-- | all parsers in this module must be run in a
-- | `MonadEffect m => ParserT DataView m` context, with
-- | `Text.Parsing.Parser.runParserT`.
-- |
-- | ## References
-- |
-- | * [MDN `ArrayBuffer`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/ArrayBuffer)
-- | * [MDN `DataView`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/DataView)
-- | * [MDN `TypedArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArray)
module Text.Parsing.Parser.DataView
  ( anyPrim
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
  , satisfy
  , satisfyInt8
  , satisfyInt16be
  , satisfyInt16le
  , satisfyInt32be
  , satisfyInt32le
  , satisfyUint8
  , satisfyUint16be
  , satisfyUint16le
  , satisfyUint32be
  , satisfyUint32le
  , satisfyFloat32be
  , satisfyFloat32le
  , satisfyFloat64be
  , satisfyFloat64le
  , takeArrayN
  , takeViewN
  , takeViewRest
  , eof
  , module Data.ArrayBuffer.Types
  , module Data.ArrayBuffer.DataView
  )
  where

import Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.State (get, put)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..), class IsSymbol, reflectSymbol)
import Data.Typelevel.Num (class Nat, toInt')
import Data.UInt (UInt)
import Data.Float32 as Float32
import Effect.Class (class MonadEffect, liftEffect)
import Type.Proxy (Proxy(..))

import Text.Parsing.Parser (ParserT, ParseState(..), fail)
import Text.Parsing.Parser.Pos (Position(..))
import Text.Parsing.Parser.Combinators (tryRethrow)
import Data.ArrayBuffer.Types
  ( DataView
  , ArrayView
  , Int16
  , Int32
  , Int8
  , Uint16
  , Uint32
  , Uint8
  , Float32
  , Float64
  , ByteLength
  )
import Data.ArrayBuffer.ValueMapping
  ( class BytesPerValue
  , class BinaryValue
  , class ShowArrayViewType
  )
import Data.ArrayBuffer.DataView
  ( AProxy(..)
  , Endian(LE,BE)
  )
import Data.ArrayBuffer.DataView (byteLength, byteOffset, get, part, buffer, remainder) as DV
import Data.ArrayBuffer.Typed (class TypedArray)
import Data.ArrayBuffer.Typed (part) as TA
import Data.ArrayBuffer.ArrayBuffer (slice) as AB


-- | Parse one fixed-bit-width `Data.ArrayBuffer.Types.ArrayViewType` primitive
-- | of a given endianness.
-- |
-- | #### Example
-- |
-- | Parse a little-endian 32-bit signed integer (4 bytes):
-- |
-- |     anyPrim LE (AProxy :: AProxy Int32)
-- |
-- | or just use the convenience function `anyInt32le`, see below.
anyPrim :: forall a name b m t
       . BinaryValue a t
      => BytesPerValue a b
      => ShowArrayViewType a name
      => IsSymbol name
      => Nat b
      => MonadEffect m
      => Endian
      -> AProxy a
      -> ParserT DataView m t
anyPrim endian aproxy = do
  ParseState input (Position{line,column}) _ <- get
  lift (liftEffect (DV.get endian (AProxy :: AProxy a) input (column-1))) >>= case _ of
    Nothing -> fail $ "Cannot parse " <> reflectSymbol (SProxy :: SProxy name) <>
      ", unexpected end of DataView"
    Just i -> do
      put $ ParseState input (Position {line, column:column + toInt' (Proxy :: Proxy b) }) true
      pure i

-- | Parse one 8-bit signed integer (byte).
anyInt8 :: forall m. MonadEffect m => ParserT DataView m Int
anyInt8 = anyPrim LE (AProxy :: AProxy Int8)

-- | Parse one 16-bit big-endian signed integer.
anyInt16be :: forall m. MonadEffect m => ParserT DataView m Int
anyInt16be = anyPrim BE (AProxy :: AProxy Int16)

-- | Parse one 16-bit little-endian signed integer.
anyInt16le :: forall m. MonadEffect m => ParserT DataView m Int
anyInt16le = anyPrim LE (AProxy :: AProxy Int16)

-- | Parse one 32-bit big-endian signed integer.
anyInt32be :: forall m. MonadEffect m => ParserT DataView m Int
anyInt32be = anyPrim BE (AProxy :: AProxy Int32)

-- | Parse one 32-bit little-endian signed integer.
anyInt32le :: forall m. MonadEffect m => ParserT DataView m Int
anyInt32le = anyPrim LE (AProxy :: AProxy Int32)

-- | Parse one 8-bit unsigned integer (octet).
anyUint8 :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint8 = anyPrim LE (AProxy :: AProxy Uint8)

-- | Parse one 16-bit big-endian unsigned integer.
anyUint16be :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint16be = anyPrim BE (AProxy :: AProxy Uint16)

-- | Parse one 16-bit little-endian unsigned integer.
anyUint16le :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint16le = anyPrim LE (AProxy :: AProxy Uint16)

-- | Parse one 32-bit big-endian unsigned integer.
anyUint32be :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint32be = anyPrim BE (AProxy :: AProxy Uint32)

-- | Parse one 32-bit little-endian unsigned integer.
anyUint32le :: forall m. MonadEffect m => ParserT DataView m UInt
anyUint32le = anyPrim LE (AProxy :: AProxy Uint32)

-- | Parse one 32-bit big-endian floating point number.
anyFloat32be :: forall m. MonadEffect m => ParserT DataView m Float32.Float32
anyFloat32be = anyPrim BE (AProxy :: AProxy Float32)

-- | Parse one 32-bit little-endian floating point number.
anyFloat32le :: forall m. MonadEffect m => ParserT DataView m Float32.Float32
anyFloat32le = anyPrim LE (AProxy :: AProxy Float32)

-- | Parse one 64-bit big-endian floating point number.
anyFloat64be :: forall m. MonadEffect m => ParserT DataView m Number
anyFloat64be = anyPrim BE (AProxy :: AProxy Float64)

-- | Parse one 64-bit little-endian floating point number.
anyFloat64le :: forall m. MonadEffect m => ParserT DataView m Number
anyFloat64le = anyPrim LE (AProxy :: AProxy Float64)

-- | Parse one fixed-bit-width primitive that satisfies the given predicate.
-- |
-- | #### Example
-- |
-- | Parse a little-endian 32-bit signed integer that is equal to *3*:
-- |
-- |     satisfy LE (AProxy :: AProxy Int32) (_ == 3)
-- |
-- | or just use the convenience function `satisfyInt32le`, see below.
satisfy :: forall a name b m t
             . BinaryValue a t
            => BytesPerValue a b
            => ShowArrayViewType a name
            => IsSymbol name
            => Nat b
            => Show t
            => MonadEffect m
            => Endian
            -> AProxy a
            -> (t -> Boolean)
            -> ParserT DataView m t
satisfy endian aproxy f = tryRethrow do
  i <- anyPrim endian aproxy
  if f i then pure i
         else fail $ reflectSymbol (SProxy :: SProxy name) <>
                     " " <> show i <> " did not satisfy predicate."

-- | Parse one 8-bit signed integer that satisfies the given predicate.
satisfyInt8 :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt8 = satisfy LE (AProxy :: AProxy Int8)

-- | Parse one 16-bit big-endian signed integer that satisfies the given predicate.
satisfyInt16be :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt16be = satisfy BE (AProxy :: AProxy Int16)

-- | Parse one 16-bit little-endian signed integer that satisfies the given predicate.
satisfyInt16le :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt16le = satisfy LE (AProxy :: AProxy Int16)

-- | Parse one 32-bit big-endian signed integer that satisfies the given predicate.
satisfyInt32be :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt32be = satisfy BE (AProxy :: AProxy Int32)

-- | Parse one 32-bit little-endian signed integer that satisfies the given predicate.
satisfyInt32le :: forall m. MonadEffect m => (Int -> Boolean) -> ParserT DataView m Int
satisfyInt32le = satisfy LE (AProxy :: AProxy Int32)

-- | Parse one 8-bit unsigned integer that satisfies the given predicate.
satisfyUint8 :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint8 = satisfy LE (AProxy :: AProxy Uint8)

-- | Parse one 16-bit big-endian unsigned integer that satisfies the given predicate.
satisfyUint16be :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint16be = satisfy BE (AProxy :: AProxy Uint16)

-- | Parse one 16-bit little-endian unsigned integer that satisfies the given predicate.
satisfyUint16le :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint16le = satisfy LE (AProxy :: AProxy Uint16)

-- | Parse one 32-bit big-endian unsigned integer that satisfies the given predicate.
satisfyUint32be :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint32be = satisfy BE (AProxy :: AProxy Uint32)

-- | Parse one 32-bit little-endian unsigned integer that satisfies the given predicate.
satisfyUint32le :: forall m. MonadEffect m => (UInt -> Boolean) -> ParserT DataView m UInt
satisfyUint32le = satisfy LE (AProxy :: AProxy Uint32)

-- | Parse one 32-bit big-endian floating-point number that satisfies the given predicate.
satisfyFloat32be :: forall m. MonadEffect m => (Float32.Float32 -> Boolean) -> ParserT DataView m Float32.Float32
satisfyFloat32be = satisfy BE (AProxy :: AProxy Float32)

-- | Parse one 32-bit little-endian floating-point number that satisfies the given predicate.
satisfyFloat32le :: forall m. MonadEffect m => (Float32.Float32 -> Boolean) -> ParserT DataView m Float32.Float32
satisfyFloat32le = satisfy LE (AProxy :: AProxy Float32)

-- | Parse one 64-bit big-endian floating-point number that satisfies the given predicate.
satisfyFloat64be :: forall m. MonadEffect m => (Number -> Boolean) -> ParserT DataView m Number
satisfyFloat64be = satisfy BE (AProxy :: AProxy Float64)

-- | Parse one 64-bit little-endian floating-point number that satisfies the given predicate.
satisfyFloat64le :: forall m. MonadEffect m => (Number -> Boolean) -> ParserT DataView m Number
satisfyFloat64le = satisfy LE (AProxy :: AProxy Float64)

-- | Take *N* primitive elements starting from the current parser position.
-- | Will fail if there is not enough input remaining. Will fail if *N*
-- | is negative.
-- |
-- | For operations for working with `ArrayView`, see module
-- | `Data.ArrayBuffer.Typed` in package __purescript-arraybuffer__.
-- |
-- | #### Example
-- |
-- | Parse three consecutive little-endian 32-bit signed integers.
-- |
-- |     takeArrayN LE (AProxy :: AProxy Int32) 3
-- |
takeArrayN :: forall a name b m t
   . TypedArray a t
  => BinaryValue a t
  => BytesPerValue a b
  => ShowArrayViewType a name
  => IsSymbol name
  => Nat b
  => Show t
  => MonadEffect m
  => Endian
  -> AProxy a
  -> Int
  -> ParserT DataView m (ArrayView a)
takeArrayN endian aproxy n = do
  ParseState input (Position {line,column}) _ <- get
  unless (n >= 0) $ fail $ "Cannot take negative number of elements."
  let nLen = n * toInt' (Proxy :: Proxy b)
  let inputLen = DV.byteLength input
  unless ((column-1) + nLen <= inputLen) $
    fail $ "Cannot take " <> show n <> " " <> reflectSymbol (SProxy :: SProxy name) <>
      ", only " <> show (inputLen - column + 1) <> " bytes remain."
  -- There's a problem here. Typed.part', which is what we need, is not exported.
  -- https://github.com/jacereda/purescript-arraybuffer/issues/27
  -- If it were exported, then we could do this:
  --
  --     p <- lift $ liftEffect $ TA.part' (DV.buffer input) (column-1) n
  --
  -- but since it's not exported, we'll have to slice the ArrayBuffer
  -- and call Typed.part on the slice. Does slice actually copy? Bad perf if it does. TODO
  let sliceBegin = ((column-1) + DV.byteOffset input)
      sliceEnd = sliceBegin + nLen
      inputSlice = AB.slice sliceBegin sliceEnd $ DV.buffer input
  p <- lift $ liftEffect $ TA.part inputSlice 0 n
  put $ ParseState input (Position {line,column:column+nLen}) true
  pure p

-- | Take *N* bytes starting from the current parser position. Will fail
-- | if not enough bytes remain in the input. Will fail if *N* is negative.
-- |
-- | #### Example
-- |
-- | Parse three bytes.
-- |
-- |     takeViewN 3
-- |
takeViewN :: forall m. MonadEffect m => ByteLength -> ParserT DataView m DataView
takeViewN n = do
  ParseState input (Position {line,column}) _ <- get
  unless (n >= 0) $ fail $ "Cannot take negative number of bytes."
  unless (column + n - 1 <= DV.byteLength input) $
    fail $ "Cannot take " <> show n <> " bytes, only " <>
      show (DV.byteLength input - column + 1) <> " bytes remain."
  p <- lift $ liftEffect $ DV.part (DV.buffer input) ((column-1) + DV.byteOffset input) n
  put $ ParseState input (Position {line,column:column+n}) true
  pure p

-- | Take the rest of the input, however many bytes remain. Always succeeds.
takeViewRest :: forall m. MonadEffect m => ParserT DataView m DataView
takeViewRest = do
  ParseState input (Position {line,column}) _ <- get
  p <- lift $ liftEffect $ DV.remainder (DV.buffer input) ((column-1) + DV.byteOffset input)
  put $ ParseState input (Position {line,column:DV.byteLength input + 1}) true
  pure p

-- | Parse succeeds at the end of the input DataView. Consumes no input.
eof :: forall m. Monad m => ParserT DataView m Unit
eof = do
  ParseState input (Position {column}) _ <- get
  unless (column > DV.byteLength input) $ fail "Expected end of DataView"
