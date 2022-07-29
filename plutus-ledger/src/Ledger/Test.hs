{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Ledger.Test where

import Ledger
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies qualified as MPS
import PlutusTx qualified
import Prelude hiding (not)

someAddress :: Address
someAddress = scriptValidatorHashAddress (validatorHash someValidator) Nothing

someValidator :: Validator
someValidator = mkValidatorScript $$(PlutusTx.compile [|| \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> () ||])

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy _ _ = True

coinMintingPolicy :: MintingPolicy
coinMintingPolicy = mkMintingPolicyScript
    $$(PlutusTx.compile [|| MPS.mkUntypedMintingPolicy mkPolicy ||])
