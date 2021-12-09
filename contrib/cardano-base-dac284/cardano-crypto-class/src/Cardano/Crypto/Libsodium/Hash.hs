{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Cardano.Crypto.Libsodium.Hash (
    SodiumHashAlgorithm (..),
    digestMLockedStorable,
    digestMLockedBS,
    expandHash,
) where

import Control.Monad (unless)
import Data.Proxy (Proxy (..))
import Foreign.C.Error (errnoToIOError, getErrno)
import Foreign.C.Types (CSize)
import Foreign.Ptr (Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (sizeOf, poke))
import Data.Word (Word8)
import Data.Type.Equality ((:~:)(..))
import GHC.IO.Exception (ioException)
import GHC.TypeLits
import System.IO.Unsafe (unsafeDupablePerformIO)

import qualified Data.ByteString as BS

import Cardano.Foreign
import Cardano.Crypto.Hash (HashAlgorithm(SizeHash), SHA256, Blake2b_256)
import Cardano.Crypto.PinnedSizedBytes (ptrPsbToSizedPtr)
import Cardano.Crypto.Libsodium.C
import Cardano.Crypto.Libsodium.Memory.Internal
import Cardano.Crypto.Libsodium.MLockedBytes.Internal

-------------------------------------------------------------------------------
-- Type-Class
-------------------------------------------------------------------------------

class HashAlgorithm h => SodiumHashAlgorithm h where
    -- This function is in IO, it is "morally pure"
    -- and can be 'unsafePerformDupableIO'd.
    naclDigestPtr
        :: proxy h
        -> Ptr a  -- ^ input
        -> Int    -- ^ input length
        -> IO (MLockedSizedBytes (SizeHash h))

    -- TODO: provide interface for multi-part?
    -- That will be useful to hashing ('1' <> oldseed).

digestMLockedStorable
    :: forall h a proxy. (SodiumHashAlgorithm h, Storable a)
    => proxy h -> Ptr a -> MLockedSizedBytes (SizeHash h)
digestMLockedStorable p ptr = unsafeDupablePerformIO $
    naclDigestPtr p ptr (sizeOf (undefined :: a))

digestMLockedBS
    :: forall h proxy. (SodiumHashAlgorithm h)
    => proxy h -> BS.ByteString -> MLockedSizedBytes (SizeHash h)
digestMLockedBS p bs = unsafeDupablePerformIO $
    BS.useAsCStringLen bs $ \(ptr, len) ->
    naclDigestPtr p (castPtr ptr) len

-------------------------------------------------------------------------------
-- Hash expansion
-------------------------------------------------------------------------------

expandHash
    :: forall h proxy. SodiumHashAlgorithm h
    => proxy h
    -> MLockedSizedBytes (SizeHash h)
    -> (MLockedSizedBytes (SizeHash h), MLockedSizedBytes (SizeHash h))
expandHash h (MLSB sfptr) = unsafeDupablePerformIO $ do
    withMLockedForeignPtr sfptr $ \ptr -> do
        l <- mlockedAlloca size1 $ \ptr' -> do
            poke ptr' (1 :: Word8)
            _ <- c_memcpy (castPtr (plusPtr ptr' 1)) ptr size
            naclDigestPtr h ptr' (fromIntegral size1)

        r <- mlockedAlloca size1 $ \ptr' -> do
            poke ptr' (2 :: Word8)
            _ <- c_memcpy (castPtr (plusPtr ptr' 1)) ptr size
            naclDigestPtr h ptr' (fromIntegral size1)

        return (l, r)
  where
    size1 :: CSize
    size1 = size + 1

    size :: CSize
    size = fromInteger $ natVal (Proxy @(SizeHash h))

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance SodiumHashAlgorithm SHA256 where
    naclDigestPtr :: forall proxy a. proxy SHA256 -> Ptr a -> Int -> IO (MLockedSizedBytes (SizeHash SHA256))
    naclDigestPtr _ input inputlen = do
        output <- allocMLockedForeignPtr
        withMLockedForeignPtr output $ \output' -> do
            res <- c_crypto_hash_sha256 (ptrPsbToSizedPtr output') (castPtr input) (fromIntegral inputlen)
            unless (res == 0) $ do
                errno <- getErrno
                ioException $ errnoToIOError "digestMLocked @SHA256: c_crypto_hash_sha256" errno Nothing Nothing

        return (MLSB output)

-- Test that manually written numbers are the same as in libsodium
_testSHA256 :: SizeHash SHA256 :~: CRYPTO_SHA256_BYTES
_testSHA256 = Refl

instance SodiumHashAlgorithm Blake2b_256 where
    naclDigestPtr :: forall proxy a. proxy Blake2b_256 -> Ptr a -> Int -> IO (MLockedSizedBytes (SizeHash Blake2b_256))
    naclDigestPtr _ input inputlen = do
        output <- allocMLockedForeignPtr
        withMLockedForeignPtr output $ \output' -> do
            res <- c_crypto_generichash_blake2b
                output' (fromInteger $ natVal (Proxy @CRYPTO_BLAKE2B_256_BYTES))  -- output
                (castPtr input) (fromIntegral inputlen)  -- input
                nullPtr 0                                -- key, unused
            unless (res == 0) $ do
                errno <- getErrno
                ioException $ errnoToIOError "digestMLocked @Blake2b_256: c_crypto_hash_sha256" errno Nothing Nothing

        return (MLSB output)

_testBlake2b256 :: SizeHash Blake2b_256 :~: CRYPTO_BLAKE2B_256_BYTES
_testBlake2b256 = Refl
