module Ledger.Contexts
    ( module Export
    , pubKeyHash
    , plutusV1ScriptCurrencySymbol
    , plutusV2ScriptCurrencySymbol
    ) where

import Ledger.Crypto (pubKeyHash)
import Ledger.Scripts (MintingPolicy, MintingPolicyHash (..), plutusV1MintingPolicyHash, plutusV2MintingPolicyHash)
import Plutus.V1.Ledger.Contexts as Export
import Plutus.V1.Ledger.Value (CurrencySymbol (..))
import Plutus.V1.Ledger.Value qualified as Value

{-# INLINABLE plutusV1ScriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'
plutusV1ScriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
plutusV1ScriptCurrencySymbol scrpt =
    let (MintingPolicyHash hsh) = plutusV1MintingPolicyHash scrpt in Value.CurrencySymbol hsh

{-# INLINABLE plutusV2ScriptCurrencySymbol #-}
-- | The 'CurrencySymbol' of a 'MintingPolicy'
plutusV2ScriptCurrencySymbol :: MintingPolicy -> CurrencySymbol
plutusV2ScriptCurrencySymbol scrpt =
    let (MintingPolicyHash hsh) = plutusV2MintingPolicyHash scrpt in Value.CurrencySymbol hsh

