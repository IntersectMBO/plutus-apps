{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Cardano.PlutusExample.DeadlinePolicy
  ( deadlineScript
  , deadlineScriptShortBs
  ) where

import Prelude (IO, Semigroup (..), Show (..), String)

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS

import Ledger hiding (singleton)
import Ledger.Interval as Interval
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value as Value
import PlutusTx (Data (..))
import PlutusTx qualified
import PlutusTx.Prelude as P hiding (Semigroup (..), unless)

deadline :: POSIXTime
deadline = 1634338471  -- transaction's valid range must be after this

{-# INLINABLE mkPolicy #-}
mkPolicy :: POSIXTime -> BuiltinData -> ScriptContext -> Bool
mkPolicy dl _ ctx = (from dl) `contains` range
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

policy :: POSIXTime -> Scripts.MintingPolicy
policy mp = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode mp

plutusScript :: Script
plutusScript =
  unMintingPolicyScript (policy deadline)

validator :: Validator
validator = Validator $ plutusScript

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

deadlineScript :: PlutusScript PlutusScriptV1
deadlineScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

deadlineScriptShortBs :: SBS.ShortByteString
deadlineScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
