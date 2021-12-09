{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implementation of the Keccak256 hashing algorithm.
module Cardano.Crypto.Hash.Keccak256
  ( Keccak256
  )
where

import Cardano.Crypto.Hash.Class
import qualified "cryptonite" Crypto.Hash as H
import qualified Data.ByteArray as BA

data Keccak256

instance HashAlgorithm Keccak256 where
  type SizeHash Keccak256 = 32
  hashAlgorithmName _ = "keccak256"
  digest _ = convert . H.hash

convert :: H.Digest H.Keccak_256 -> ByteString
convert = BA.convert
