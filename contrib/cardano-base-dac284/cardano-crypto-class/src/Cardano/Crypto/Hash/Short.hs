{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Implementation of short hashing algorithm, suitable for testing.
module Cardano.Crypto.Hash.Short
  ( ShortHash
  , Blake2bPrefix
  )
where

import Cardano.Crypto.Hash.Class
import Cardano.Crypto.Hash.Blake2b (blake2b_libsodium)

import GHC.TypeLits (Nat, KnownNat, CmpNat, natVal)
import Data.Proxy (Proxy (..))

type ShortHash = Blake2bPrefix 8

data Blake2bPrefix (n :: Nat)

instance (KnownNat n, CmpNat n 33 ~ 'LT) => HashAlgorithm (Blake2bPrefix n) where
  type SizeHash (Blake2bPrefix n) = n
  hashAlgorithmName _ = "blake2b_prefix_" <> show (natVal (Proxy :: Proxy n))
  digest _ = blake2b_libsodium (fromIntegral (natVal (Proxy :: Proxy n)))
