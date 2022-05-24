{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PlutusExample.WitnessPolicy
  ( witnessScript
  , witnessScriptShortBs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V1.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (PubKeyHash, Script, ScriptContext (scriptContextTxInfo), TxInfo, Validator (Validator),
                             mkMintingPolicyScript, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Contexts (txSignedBy)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup, unless, (.))

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> ScriptContext -> Bool
mkPolicy pkh ctx = traceIfFalse "not signed by redeemer pubkeyhash" checkWitness
  where
    info :: TxInfo
    info  = scriptContextTxInfo ctx

    checkWitness :: Bool
    checkWitness  = txSignedBy info pkh

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedMintingPolicy mkPolicy

plutusScript :: Script
plutusScript =
  unMintingPolicyScript policy

validator :: Validator
validator = Validator $ unMintingPolicyScript policy

script :: Script
script = unValidatorScript validator

witnessScriptShortBs :: SBS.ShortByteString
witnessScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

witnessScript :: PlutusScript PlutusScriptV1
witnessScript = PlutusScriptSerialised witnessScriptShortBs
