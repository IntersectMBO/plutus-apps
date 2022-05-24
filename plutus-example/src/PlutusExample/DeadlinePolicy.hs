{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PlutusExample.DeadlinePolicy
  ( deadlineScript
  , deadlineScriptShortBs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V1.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Api (POSIXTime, POSIXTimeRange, Script, ScriptContext (scriptContextTxInfo),
                             TxInfo (txInfoValidRange), Validator (Validator), mkMintingPolicyScript,
                             unMintingPolicyScript, unValidatorScript)
import Plutus.V1.Ledger.Interval (contains, to)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup, unless, (.))

{-# INLINABLE mkPolicy #-}
mkPolicy :: POSIXTime -> ScriptContext -> Bool
mkPolicy dl ctx = (to dl) `contains` range
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

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

deadlineScriptShortBs :: SBS.ShortByteString
deadlineScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

deadlineScript :: PlutusScript PlutusScriptV1
deadlineScript = PlutusScriptSerialised deadlineScriptShortBs
