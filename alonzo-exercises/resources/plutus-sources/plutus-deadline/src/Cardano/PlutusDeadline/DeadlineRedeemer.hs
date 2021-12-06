{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.PlutusExample.DeadlineRedeemer
  ( deadlineScript
  , deadlineScriptShortBs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts qualified as Scripts

import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)

{-# INLINABLE mkPolicy #-}
mkPolicy :: POSIXTime -> ScriptContext -> Bool
mkPolicy dl ctx = (to dl) `contains` range
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

inst :: Scripts.MintingPolicy
inst = mkMintingPolicyScript
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

plutusScript :: Script
plutusScript =
  unMintingPolicyScript inst

deadlineValidator :: Validator
deadlineValidator =
  Validator $ unMintingPolicyScript inst

script :: Plutus.Script
script = Plutus.unValidatorScript deadlineValidator

deadlineScriptShortBs :: SBS.ShortByteString
deadlineScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

deadlineScript :: PlutusScript PlutusScriptV1
deadlineScript = PlutusScriptSerialised deadlineScriptShortBs
