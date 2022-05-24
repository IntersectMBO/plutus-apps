{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PlutusExample.TokenNamePolicy
  ( tokenNameScript
  , tokenNameScriptShortBs
  ) where

import Prelude hiding (($), (>))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V1.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (Script, ScriptContext (scriptContextTxInfo), TokenName, TxInfo, Validator (Validator),
                             mkMintingPolicyScript, unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Contexts (ownCurrencySymbol, txInfoMint)
import Plutus.V1.Ledger.Value (valueOf)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup, unless, (.))

{-# INLINEABLE mkPolicy #-}
mkPolicy :: TokenName -> ScriptContext -> Bool
mkPolicy tn ctx = traceIfFalse "wrong token name" checkTokenName
    where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkTokenName :: Bool
    checkTokenName = valueOf (txInfoMint info) (ownCurrencySymbol ctx) tn > 0

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

tokenNameScriptShortBs :: SBS.ShortByteString
tokenNameScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

tokenNameScript :: PlutusScript PlutusScriptV1
tokenNameScript = PlutusScriptSerialised tokenNameScriptShortBs
