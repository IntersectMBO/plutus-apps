-- |
-- Module      : Data.Double.Conversion.ByteString
-- Copyright   : (c) 2011 MailRank, Inc.
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast, efficient support for converting between double precision
-- floating point values and text.
--
-- Although about 15 times faster than plain 'show', these functions
-- are /slower/ than their 'Text' counterparts, at roughly half the
-- speed.  (This seems to be due to the cost of allocating
-- 'ByteString' values via @malloc@.)

module Data.Double.Conversion.ByteString
    (
      toExponential
    , toFixed
    , toPrecision
    , toShortest
    ) where

import Control.Monad (when)
import Foreign.ForeignPtr (withForeignPtr)
import Data.Double.Conversion.FFI
import Data.Word (Word8)
import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Foreign.C.Types (CDouble, CInt)
import Foreign.Ptr (Ptr)
import System.IO.Unsafe (unsafePerformIO)

-- | Compute a representation in exponential format with the requested
-- number of digits after the decimal point. The last emitted digit is
-- rounded.  If -1 digits are requested, then the shortest exponential
-- representation is computed.
toExponential :: Int -> Double -> ByteString
toExponential ndigits = convert "toExponential" len $ \val mba ->
                        c_ToExponential val mba (fromIntegral ndigits)
  where len = c_ToExponentialLength
        {-# NOINLINE len #-}

-- | Compute a decimal representation with a fixed number of digits
-- after the decimal point. The last emitted digit is rounded.
toFixed :: Int -> Double -> ByteString
toFixed ndigits = convert "toFixed" len $ \val mba ->
                  c_ToFixed val mba (fromIntegral ndigits)
  where len = c_ToFixedLength
        {-# NOINLINE len #-}

-- | Compute the shortest string of digits that correctly represent
-- the input number.
toShortest :: Double -> ByteString
toShortest = convert "toShortest" len c_ToShortest
  where len = c_ToShortestLength
        {-# NOINLINE len #-}

-- | Compute @precision@ leading digits of the given value either in
-- exponential or decimal format. The last computed digit is rounded.
toPrecision :: Int -> Double -> ByteString
toPrecision ndigits = convert "toPrecision" len $ \val mba ->
                      c_ToPrecision val mba (fromIntegral ndigits)
  where len = c_ToPrecisionLength
        {-# NOINLINE len #-}

convert :: String -> CInt -> (CDouble -> Ptr Word8 -> IO CInt)
        -> Double -> ByteString
convert func len act val = unsafePerformIO $ do
  fp <- mallocByteString (fromIntegral len)
  size <- withForeignPtr fp $ act (realToFrac val)
  when (size == -1) .
    fail $ "Data.Double.Conversion.ByteString." ++ func ++
           ": conversion failed (invalid precision requested)"
  return $ PS fp 0 (fromIntegral size)
