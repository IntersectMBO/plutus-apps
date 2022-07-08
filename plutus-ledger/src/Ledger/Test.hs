{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Ledger.Test where

import Ledger qualified
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.V1.Ledger.Api (Address, Validator)
import PlutusTx qualified
import Prelude hiding (not)

someAddress :: Address
someAddress = Ledger.scriptValidatorHashAddress (PV1.validatorHash someValidator) Nothing

someValidator :: Validator
someValidator = Ledger.mkValidatorScript $$(PlutusTx.compile [|| \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> () ||])

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> Ledger.ScriptContext -> Bool
mkPolicy _ _ = True

coinMintingPolicy :: Ledger.MintingPolicy
coinMintingPolicy = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [|| MPS.mkUntypedMintingPolicy mkPolicy ||])
