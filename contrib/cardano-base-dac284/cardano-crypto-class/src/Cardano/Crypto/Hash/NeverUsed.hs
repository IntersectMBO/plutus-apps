{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Crypto.Hash.NeverUsed (NeverHash) where

import Cardano.Crypto.Hash.Class

-- | HASH never used
--
-- Will throw a runtime exception when trying to hash something.
data NeverHash

instance HashAlgorithm NeverHash where
  type SizeHash NeverHash = 0
  hashAlgorithmName _ = "never"
  digest = error "HASH not available"
