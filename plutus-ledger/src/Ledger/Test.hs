{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Ledger.Test where

import Cardano.Api qualified as C
import Ledger qualified
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI (policyId)
import Plutus.Script.Utils.Typed as PSU
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V1.Ledger.Api (Address, Validator)
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import Prelude hiding (not)

someCode :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
someCode = $$(PlutusTx.compile [|| \_ _ _ -> () ||])

someValidator :: Validator
someValidator = PV1.mkValidatorScript someCode

someTypedValidator :: Scripts.TypedValidator Any
someTypedValidator = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV1)

someValidatorHash :: PV1.ValidatorHash
someValidatorHash = PV1.validatorHash someValidator

someCardanoAddress :: C.NetworkId -> Ledger.CardanoAddress
someCardanoAddress = flip PV1.mkValidatorCardanoAddress someValidator

someAddress :: Address
someAddress = Ledger.scriptValidatorHashAddress someValidatorHash Nothing

someValidatorV2 :: Validator
someValidatorV2 = PV2.mkValidatorScript someCode

someTypedValidatorV2 :: Scripts.TypedValidator Any
someTypedValidatorV2 = Scripts.unsafeMkTypedValidator (Versioned someValidator PlutusV2)

someValidatorHashV2 :: PV2.ValidatorHash
someValidatorHashV2 = PV2.validatorHash someValidatorV2

someCardanoAddressV2 :: C.NetworkId -> Ledger.CardanoAddress
someCardanoAddressV2 = flip PV2.mkValidatorCardanoAddress someValidatorV2

someAddressV2 :: Address
someAddressV2 = Ledger.scriptValidatorHashAddress someValidatorHashV2 Nothing

{-# INLINABLE mkPolicy #-}
mkPolicy :: () -> Ledger.ScriptContext -> Bool
mkPolicy _ _ = True

{-# INLINABLE mkPolicyV2 #-}
mkPolicyV2 :: () -> PV2.ScriptContext -> Bool
mkPolicyV2 _ _ = True

coinMintingPolicy :: Language -> Versioned Ledger.MintingPolicy
coinMintingPolicy lang = case lang of
  PlutusV1 -> Versioned coinMintingPolicyV1 lang
  PlutusV2 -> Versioned coinMintingPolicyV2 lang

coinMintingPolicyV1 :: Ledger.MintingPolicy
coinMintingPolicyV1 = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [|| PSU.mkUntypedMintingPolicy mkPolicy ||])

coinMintingPolicyV2 :: Ledger.MintingPolicy
coinMintingPolicyV2 = Ledger.mkMintingPolicyScript
    $$(PlutusTx.compile [|| PSU.mkUntypedMintingPolicy mkPolicyV2 ||])

coinMintingPolicyHash :: Language -> Ledger.MintingPolicyHash
coinMintingPolicyHash lang = case lang of
  PlutusV1 -> coinMintingPolicyHashV1
  PlutusV2 -> coinMintingPolicyHashV2

coinMintingPolicyHashV1 :: Ledger.MintingPolicyHash
coinMintingPolicyHashV1 = PV1.mintingPolicyHash coinMintingPolicyV1

coinMintingPolicyHashV2 :: Ledger.MintingPolicyHash
coinMintingPolicyHashV2 = PV2.mintingPolicyHash coinMintingPolicyV2

coinMintingPolicyCurrencySymbol :: Language -> Ledger.CurrencySymbol
coinMintingPolicyCurrencySymbol lang = case lang of
  PlutusV1 -> coinMintingPolicyCurrencySymbolV1
  PlutusV2 -> coinMintingPolicyCurrencySymbolV2

coinMintingPolicyCurrencySymbolV1 :: Ledger.CurrencySymbol
coinMintingPolicyCurrencySymbolV1 = Value.mpsSymbol $ coinMintingPolicyHash PlutusV1

coinMintingPolicyCurrencySymbolV2 :: Ledger.CurrencySymbol
coinMintingPolicyCurrencySymbolV2 = Value.mpsSymbol $ coinMintingPolicyHash PlutusV2

someToken :: Language -> Ledger.Value
someToken lang = Value.singleton (coinMintingPolicyCurrencySymbol lang) "someToken" 1

asRedeemer :: PlutusTx.ToData a => a -> Ledger.Redeemer
asRedeemer a = Ledger.Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

asDatum :: PlutusTx.ToData a => a -> Ledger.Datum
asDatum a = Ledger.Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData a

coinMintingPolicyId :: Language -> C.PolicyId
coinMintingPolicyId = policyId . coinMintingPolicy
