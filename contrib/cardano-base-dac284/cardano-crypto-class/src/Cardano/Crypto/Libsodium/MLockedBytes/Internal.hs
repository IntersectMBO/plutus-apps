{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Cardano.Crypto.Libsodium.MLockedBytes.Internal (
    MLockedSizedBytes (..),
    mlsbZero,
    mlsbFromByteString,
    mlsbFromByteStringCheck,
    mlsbToByteString,
    mlsbUseAsCPtr,
    mlsbUseAsSizedPtr,
    mlsbFinalize,
) where

import Control.DeepSeq (NFData (..))
import Data.Proxy (Proxy (..))
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (castForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import GHC.TypeLits (KnownNat, natVal)
import NoThunks.Class (NoThunks, OnlyCheckWhnfNamed (..))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Data.Word (Word8)

import Cardano.Foreign
import Cardano.Crypto.Libsodium.Memory.Internal
import Cardano.Crypto.Libsodium.C
import Cardano.Crypto.PinnedSizedBytes

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BSI

{- HLINT ignore "Reduce duplication" -}

newtype MLockedSizedBytes n = MLSB (MLockedForeignPtr (PinnedSizedBytes n))
  deriving NoThunks via OnlyCheckWhnfNamed "MLockedSizedBytes" (MLockedSizedBytes n)

instance KnownNat n => Eq (MLockedSizedBytes n) where
    x == y = compare x y == EQ

instance KnownNat n => Ord (MLockedSizedBytes n) where
    compare (MLSB x) (MLSB y) = unsafeDupablePerformIO $
        withMLockedForeignPtr x $ \x' ->
        withMLockedForeignPtr y $ \y' -> do
            res <- c_sodium_compare x' y' (CSize (fromIntegral size))
            return (compare res 0)
      where
        size = natVal (Proxy @n)

instance KnownNat n => Show (MLockedSizedBytes n) where
    showsPrec d _ = showParen (d > 10)
        $ showString "_ :: MLockedSizedBytes "
        . showsPrec 11 (natVal (Proxy @n))

instance NFData (MLockedSizedBytes n) where
    rnf (MLSB p) = seq p ()

-- | Note: this doesn't need to allocate mlocked memory,
-- but we do that for consistency
mlsbZero :: forall n. KnownNat n => MLockedSizedBytes n
mlsbZero = unsafeDupablePerformIO $ do
    fptr <- allocMLockedForeignPtr
    withMLockedForeignPtr fptr $ \ptr -> do
        _ <- c_memset (castPtr ptr) 0 size
        return ()
    return (MLSB fptr)
  where
    size  :: CSize
    size = fromInteger (natVal (Proxy @n))

mlsbFromByteString :: forall n. KnownNat n => BS.ByteString -> MLockedSizedBytes n
mlsbFromByteString bs = unsafeDupablePerformIO $ BS.useAsCStringLen bs $ \(ptrBS, len) -> do
    fptr <- allocMLockedForeignPtr
    withMLockedForeignPtr fptr $ \ptr -> do
        _ <- c_memcpy (castPtr ptr) ptrBS (fromIntegral (min len size))
        return ()
    return (MLSB fptr)
  where
    size  :: Int
    size = fromInteger (natVal (Proxy @n))

mlsbFromByteStringCheck :: forall n. KnownNat n => BS.ByteString -> Maybe (MLockedSizedBytes n)
mlsbFromByteStringCheck bs
    | BS.length bs /= size = Nothing
    | otherwise = Just $ unsafeDupablePerformIO $ BS.useAsCStringLen bs $ \(ptrBS, len) -> do
    fptr <- allocMLockedForeignPtr
    withMLockedForeignPtr fptr $ \ptr -> do
        _ <- c_memcpy (castPtr ptr) ptrBS (fromIntegral (min len size))
        return ()
    return (MLSB fptr)
  where
    size  :: Int
    size = fromInteger (natVal (Proxy @n))

-- | /Note:/ the resulting 'BS.ByteString' will still refer to secure memory,
-- but the types don't prevent it from be exposed.
--
mlsbToByteString :: forall n. KnownNat n => MLockedSizedBytes n -> BS.ByteString
mlsbToByteString (MLSB (SFP fptr)) = BSI.PS (castForeignPtr fptr) 0 size where
    size  :: Int
    size = fromInteger (natVal (Proxy @n))

mlsbUseAsCPtr :: MLockedSizedBytes n -> (Ptr Word8 -> IO r) -> IO r
mlsbUseAsCPtr (MLSB x) k = withMLockedForeignPtr x (k . castPtr)

mlsbUseAsSizedPtr :: MLockedSizedBytes n -> (SizedPtr n -> IO r) -> IO r
mlsbUseAsSizedPtr (MLSB x) k = withMLockedForeignPtr x (k . ptrPsbToSizedPtr)

-- | Calls 'finalizeMLockedForeignPtr' on underlying pointer.
-- This function invalidates argument.
--
mlsbFinalize :: MLockedSizedBytes n -> IO ()
mlsbFinalize (MLSB ptr) = finalizeMLockedForeignPtr ptr
