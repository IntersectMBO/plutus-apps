{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}

module Cardano.Crypto.Util
  ( Empty
  , SignableRepresentation(..)
  , getRandomWord64

    -- * Simple serialisation used in mock instances
  , readBinaryWord64
  , writeBinaryWord64
  , readBinaryNatural
  , writeBinaryNatural
  , splitsAt

  -- * Low level conversions
  , bytesToNatural
  , naturalToBytes
  )
where

import           Data.Word
import           Numeric.Natural
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import           Data.ByteString (ByteString)

import qualified GHC.Exts    as GHC
import qualified GHC.IO      as GHC (unsafeDupablePerformIO)
import qualified GHC.Natural as GHC
import qualified GHC.Integer.GMP.Internals as GMP
import           Foreign.ForeignPtr (withForeignPtr)

import           Crypto.Random (MonadRandom (..))


class Empty a
instance Empty a



--
-- Signable
--

-- | A class of types that have a representation in bytes that can be used
-- for signing and verifying.
--
class SignableRepresentation a where
    getSignableRepresentation :: a -> ByteString

instance SignableRepresentation ByteString where
    getSignableRepresentation = id


--
-- Random source used in some mock instances
--

getRandomWord64 :: MonadRandom m => m Word64
getRandomWord64 = readBinaryWord64 <$> getRandomBytes 8


--
-- Really simple serialisation used in some mock instances
--

readBinaryWord64 :: ByteString -> Word64
readBinaryWord64 =
  BS.foldl' (\acc w8 -> unsafeShiftL acc 8 + fromIntegral w8) 0


readBinaryNatural :: ByteString -> Natural
readBinaryNatural =
  BS.foldl' (\acc w8 -> unsafeShiftL acc 8 + fromIntegral w8) 0


writeBinaryWord64 :: Word64 -> ByteString
writeBinaryWord64 =
    BS.reverse . fst
  . BS.unfoldrN 8 (\w -> Just (fromIntegral w, unsafeShiftR w 8))

writeBinaryNatural :: Int -> Natural -> ByteString
writeBinaryNatural bytes =
    BS.reverse . fst
  . BS.unfoldrN bytes (\w -> Just (fromIntegral w, unsafeShiftR w 8))

splitsAt :: [Int] -> ByteString -> [ByteString]
splitsAt = go 0
  where
    go !_   [] bs
      | BS.null bs         = []
      | otherwise          = [bs]

    go !off (sz:szs) bs
      | BS.length bs >= sz = BS.take sz bs : go (off+sz) szs (BS.drop sz bs)
      | otherwise          = []

-- | Create a 'Natural' out of a 'ByteString', in big endian.
--
-- This is fast enough to use in production.
--
bytesToNatural :: ByteString -> Natural
bytesToNatural = GHC.naturalFromInteger . bytesToInteger

-- | The inverse of 'bytesToNatural'. Note that this is a naive implementation
-- and only suitable for tests.
--
naturalToBytes :: Int -> Natural -> ByteString
naturalToBytes = writeBinaryNatural

bytesToInteger :: ByteString -> Integer
bytesToInteger (BS.PS fp (GHC.I# off#) (GHC.I# len#)) =
    -- This should be safe since we're simply reading from ByteString (which is
    -- immutable) and GMP allocates a new memory for the Integer, i.e., there is
    -- no mutation involved.
    GHC.unsafeDupablePerformIO $
      withForeignPtr fp $ \(GHC.Ptr addr#) ->
        let addrOff# = addr# `GHC.plusAddr#` off#
        -- The last parmaeter (`1#`) tells the import function to use big
        -- endian encoding.
        in GMP.importIntegerFromAddr addrOff# (GHC.int2Word# len#) 1#

