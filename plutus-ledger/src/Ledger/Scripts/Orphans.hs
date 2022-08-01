{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Scripts.Orphans where

import Control.DeepSeq (NFData)
import Plutus.V1.Ledger.Scripts (MintingPolicyHash)

instance NFData MintingPolicyHash
