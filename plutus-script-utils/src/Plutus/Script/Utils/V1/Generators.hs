{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Plutus.Script.Utils.V1.Generators
    ( alwaysSucceedValidator
    , alwaysSucceedValidatorVersioned
    , alwaysSucceedValidatorHash
    , alwaysSucceedPolicy
    , alwaysSucceedPolicyVersioned
    , alwaysSucceedPolicyHash
    , someTokenValue
    ) where

import Plutus.Script.Utils.Scripts qualified as Ledger
import Plutus.V1.Ledger.Scripts qualified as Ledger
import Plutus.V1.Ledger.Value (TokenName, Value)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified

import Plutus.Script.Utils.V1.Scripts qualified as Scripts

alwaysSucceedValidator :: Ledger.Validator
alwaysSucceedValidator =
    Ledger.mkValidatorScript $$(PlutusTx.compile [|| \_ _ _ -> () ||])

alwaysSucceedValidatorVersioned :: Ledger.Versioned Ledger.Validator
alwaysSucceedValidatorVersioned = Ledger.Versioned alwaysSucceedValidator Ledger.PlutusV1

alwaysSucceedValidatorHash :: Ledger.ValidatorHash
alwaysSucceedValidatorHash = Scripts.validatorHash alwaysSucceedValidator

alwaysSucceedPolicy :: Ledger.MintingPolicy
alwaysSucceedPolicy =
    Ledger.mkMintingPolicyScript $$(PlutusTx.compile [|| \_ _ -> () ||])

alwaysSucceedPolicyVersioned :: Ledger.Versioned Ledger.MintingPolicy
alwaysSucceedPolicyVersioned = Ledger.Versioned alwaysSucceedPolicy Ledger.PlutusV1

alwaysSucceedPolicyHash :: Ledger.MintingPolicyHash
alwaysSucceedPolicyHash = Scripts.mintingPolicyHash alwaysSucceedPolicy

someTokenValue :: TokenName -> Integer -> Value
someTokenValue = Value.singleton (Scripts.scriptCurrencySymbol alwaysSucceedPolicy)
