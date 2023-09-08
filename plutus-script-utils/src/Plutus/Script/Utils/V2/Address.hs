module Plutus.Script.Utils.V2.Address
    ( mkValidatorAddress
    , mkValidatorCardanoAddress
    , mkMintingPolicyCardanoAddress
    , mkStakeValidatorCardanoAddress
    ) where

import Cardano.Api qualified as Script

import Plutus.Script.Utils.Scripts (Script, Validator, getMintingPolicy, getStakeValidator, getValidator)
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import PlutusLedgerApi.V2 (Address (Address), Credential (ScriptCredential))

{-# INLINABLE mkValidatorAddress #-}
-- | The address that should be used by a transaction output locked by the given
-- Plutus V2 validator script.
mkValidatorAddress :: Validator -> Address
mkValidatorAddress validator = Address (ScriptCredential (PV2.scriptHash $ getValidator validator)) Nothing

-- | Cardano address of a 'PV1.Validator' script.
mkValidatorCardanoAddress :: Script.NetworkId -> PV2.Validator -> Script.AddressInEra Script.BabbageEra
mkValidatorCardanoAddress networkId = toScriptAddress networkId . getValidator

-- | Cardano address of a 'PV1.MintingPolicy' script.
mkMintingPolicyCardanoAddress :: Script.NetworkId -> PV2.MintingPolicy -> Script.AddressInEra Script.BabbageEra
mkMintingPolicyCardanoAddress networkId = toScriptAddress networkId . getMintingPolicy

-- | Cardano address of a 'PV1.MintingPolicy' script.
mkStakeValidatorCardanoAddress :: Script.NetworkId -> PV2.StakeValidator -> Script.AddressInEra Script.BabbageEra
mkStakeValidatorCardanoAddress networkId = toScriptAddress networkId . getStakeValidator

-- | Convert a 'Script' to a 'cardano-api' address.
--
-- For why we depend on `cardano-api`,
-- see note [Hash computation of datums, redeemers and scripts]
toScriptAddress :: Script.NetworkId -> Script -> Script.AddressInEra Script.BabbageEra
toScriptAddress networkId script = Script.makeShelleyAddressInEra
  networkId
  ( Script.PaymentCredentialByScript
    . Script.hashScript
    $ PV2.toCardanoApiScript script)
  Script.NoStakeAddress
