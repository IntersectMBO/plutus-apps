{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implementation of the SHA256 hashing algorithm.
module Cardano.Crypto.Hash.SHA256
  ( SHA256
  )
where

import Control.Monad (unless)
import Cardano.Crypto.Libsodium.C (c_crypto_hash_sha256)
import Cardano.Foreign (SizedPtr(SizedPtr))
import Cardano.Crypto.Hash.Class (HashAlgorithm, SizeHash, hashAlgorithmName, digest)

import Foreign.Ptr (castPtr)
import Foreign.C.Error (errnoToIOError, getErrno)
import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import GHC.IO.Exception (ioException)

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI


data SHA256

instance HashAlgorithm SHA256 where
  type SizeHash SHA256 = 32
  hashAlgorithmName _ = "sha256"
  digest _ = sha256_libsodium

sha256_libsodium :: B.ByteString -> B.ByteString
sha256_libsodium input =
  BI.unsafeCreate expected_size $ \outptr ->
    B.useAsCStringLen input $ \(inptr, inputlen) -> do
      res <- c_crypto_hash_sha256 (SizedPtr (castPtr outptr)) (castPtr inptr) (fromIntegral inputlen)
      unless (res == 0) $ do
          errno <- getErrno
          ioException $ errnoToIOError "digest @SHA256: c_crypto_hash_sha256" errno Nothing Nothing

  where
    expected_size = fromIntegral (natVal (Proxy::Proxy (SizeHash SHA256)))
