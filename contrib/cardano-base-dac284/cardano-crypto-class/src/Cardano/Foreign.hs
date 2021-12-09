{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | Utilities for FFI
module Cardano.Foreign (
    -- * Sized pointer
    SizedPtr (..),
    allocaSized,
    memcpySized,
    memsetSized,
    -- * Low-level C functions
    c_memcpy,
    c_memset,
) where

import Control.Monad (void)
import Data.Void (Void)
import Data.Word (Word8)
import Data.Proxy (Proxy (..))
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Alloc (allocaBytes)
import GHC.TypeLits

-------------------------------------------------------------------------------
-- Sized pointer
-------------------------------------------------------------------------------

-- A pointer which knows the size of underlying memory block
newtype SizedPtr (n :: Nat) = SizedPtr (Ptr Void)

-- | Like 'allocaBytes'.
allocaSized :: forall n b. KnownNat n => (SizedPtr n -> IO b) -> IO b
allocaSized k = allocaBytes size (k . SizedPtr)
  where
    size :: Int
    size = fromInteger (natVal (Proxy @n))

memcpySized :: forall n. KnownNat n => SizedPtr n -> SizedPtr n -> IO ()
memcpySized (SizedPtr dest) (SizedPtr src) = void (c_memcpy dest src size)
  where
    size :: CSize
    size = fromInteger (natVal (Proxy @n))

memsetSized :: forall n. KnownNat n => SizedPtr n -> Word8 -> IO ()
memsetSized (SizedPtr s) c = void (c_memset s (fromIntegral c) size)
  where
    size :: CSize
    size = fromInteger (natVal (Proxy @n))

-------------------------------------------------------------------------------
-- Some C functions
-------------------------------------------------------------------------------

-- | @void *memcpy(void *dest, const void *src, size_t n);@
--
-- Note: this is safe foreign import
foreign import ccall "memcpy"
    c_memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr ())

-- | @void *memset(void *s, int c, size_t n);@
--
-- Note: for sure zeroing memory use @c_sodium_memzero@.
foreign import ccall "memset"
    c_memset :: Ptr a -> Int -> CSize -> IO (Ptr ())
