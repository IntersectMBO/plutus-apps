module Ledger.Contexts
    ( module Export
    , pubKeyHash
    , scriptCurrencySymbol
    ) where

import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (MintingPolicy, MintingPolicyHash (..), mintingPolicyHash)
import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Contexts as Export

{-# INLINABLE scriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'
scriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
scriptCurrencySymbol scrpt = let (MintingPolicyHash hsh) = mintingPolicyHash scrpt in Value.CurrencySymbol hsh

