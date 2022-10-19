{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Ledger.Test where

import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Typed as PSU
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies qualified as MPS
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V1.Ledger.Api (Address, Validator)
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import Prelude hiding (not)

someAddress :: Address
someAddress = Ledger.scriptValidatorHashAddress someValidatorHash Nothing

someValidatorHash :: PV1.ValidatorHash
someValidatorHash = PV1.validatorHash someValidator

someValidator :: Validator
someValidator = PV1.mkValidatorScript $$(PlutusTx.compile [|| \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> () ||])

someTypedValidator :: Scripts.TypedValidator Any
someTypedValidator = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV1)

someAddressV2 :: Address
someAddressV2 = Ledger.scriptValidatorHashAddress someValidatorHashV2 Nothing

someValidatorHashV2 :: PV2.ValidatorHash
someValidatorHashV2 = PV2.validatorHash someValidatorV2

someValidatorV2 :: Validator
someValidatorV2 = PV2.mkValidatorScript $$(PlutusTx.compile [|| \(_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) (_ :: PlutusTx.BuiltinData) -> () ||])

someTypedValidatorV2 :: Scripts.TypedValidator Any
someTypedValidatorV2 = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV2)

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> Ledger.ScriptContext -> Bool
mkPolicy _ _ = True

coinMintingPolicy :: Ledger.MintingPolicy
coinMintingPolicy = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [|| MPS.mkUntypedMintingPolicy mkPolicy ||])

coinMintingPolicyHash :: Ledger.MintingPolicyHash
coinMintingPolicyHash = PV1.mintingPolicyHash coinMintingPolicy

coinMintingPolicyCurrencySymbol :: Ledger.CurrencySymbol
coinMintingPolicyCurrencySymbol = Value.mpsSymbol coinMintingPolicyHash

someToken :: Ledger.Value
someToken = Value.singleton coinMintingPolicyCurrencySymbol "someToken" 1

asRedeemer :: PlutusTx.ToData a => a -> Ledger.Redeemer
asRedeemer a = Ledger.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

asDatum :: PlutusTx.ToData a => a -> Ledger.Datum
asDatum a = Ledger.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a
