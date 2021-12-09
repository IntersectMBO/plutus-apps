{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Crypto.Libsodium.Memory.Internal (
  -- * High-level memory management
  MLockedForeignPtr (..),
  withMLockedForeignPtr,
  allocMLockedForeignPtr,
  finalizeMLockedForeignPtr,
  traceMLockedForeignPtr,
  -- * Low-level memory function
  mlockedAlloca,
  mlockedAllocaSized,
  sodiumMalloc,
  sodiumFree,
) where

import Control.Exception (bracket)
import Control.Monad (when)
import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Foreign.C.Error (errnoToIOError, getErrno)
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr, finalizeForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable (alignment, sizeOf, peek))
import GHC.TypeLits (KnownNat, natVal)
import GHC.IO.Exception (ioException)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))

import Cardano.Foreign
import Cardano.Crypto.Libsodium.C

-- | Foreign pointer to securely allocated memory.
newtype MLockedForeignPtr a = SFP { _unwrapMLockedForeignPtr :: ForeignPtr a }
  deriving NoThunks via OnlyCheckWhnfNamed "MLockedForeignPtr" (MLockedForeignPtr a)

withMLockedForeignPtr :: forall a b. MLockedForeignPtr a -> (Ptr a -> IO b) -> IO b
withMLockedForeignPtr = coerce (withForeignPtr @a @b)

finalizeMLockedForeignPtr :: forall a. MLockedForeignPtr a -> IO ()
finalizeMLockedForeignPtr = coerce (finalizeForeignPtr @a)

traceMLockedForeignPtr :: (Storable a, Show a) => MLockedForeignPtr a -> IO ()
traceMLockedForeignPtr fptr = withMLockedForeignPtr fptr $ \ptr -> do
    a <- peek ptr
    print a

{-# DEPRECATED traceMLockedForeignPtr "Don't leave traceMLockedForeignPtr in production" #-}

-- | Allocate secure memory using 'c_sodium_malloc'.
--
-- <https://libsodium.gitbook.io/doc/memory_management>
--
allocMLockedForeignPtr :: Storable a => IO (MLockedForeignPtr a)
allocMLockedForeignPtr = impl undefined where
    impl :: forall b. Storable b => b -> IO (MLockedForeignPtr b)
    impl b = do
        ptr <- sodiumMalloc size
        fmap SFP (newForeignPtr c_sodium_free_funptr ptr)

      where
        size :: CSize
        size = fromIntegral size''

        size' :: Int
        size' = sizeOf b

        align :: Int
        align = alignment b

        size'' :: Int
        size''
            | m == 0    = size'
            | otherwise = (q + 1) * align
          where
            (q,m) = size' `divMod` align

sodiumMalloc :: CSize -> IO (Ptr a)
sodiumMalloc size = do
    ptr <- c_sodium_malloc size
    when (ptr == nullPtr) $ do
        errno <- getErrno
        ioException $ errnoToIOError "c_sodium_malloc" errno Nothing Nothing
    return ptr

sodiumFree :: Ptr a -> IO ()
sodiumFree = c_sodium_free

mlockedAlloca :: forall a b. CSize -> (Ptr a -> IO b) -> IO b
mlockedAlloca size = bracket (sodiumMalloc size) sodiumFree

mlockedAllocaSized :: forall n b. KnownNat n => (SizedPtr n -> IO b) -> IO b
mlockedAllocaSized k = mlockedAlloca size (k . SizedPtr) where
    size :: CSize
    size = fromInteger (natVal (Proxy @n))
