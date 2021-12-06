{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.PlutusExample.Deadline
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

deadline :: POSIXTime
deadline = 1634338471  -- transaction's valid range must be before this

{-# INLINABLE mkValidator #-}
mkValidator :: POSIXTime -> BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkValidator dl _ _ ctx = (to dl) `contains` range
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    range :: POSIXTimeRange
    range = txInfoValidRange info

validator :: POSIXTime -> Plutus.Validator
validator t = Ledger.mkValidatorScript $
    $$(PlutusTx.compile [|| validatorParam ||])
     `PlutusTx.applyCode`
      PlutusTx.liftCode deadline
    where validatorParam s = Scripts.wrapValidator (mkValidator s)

script :: Plutus.Script
script = Plutus.unValidatorScript (validator deadline)

deadlineScriptShortBs :: SBS.ShortByteString
deadlineScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

deadlineScript :: PlutusScript PlutusScriptV1
deadlineScript = PlutusScriptSerialised deadlineScriptShortBs
