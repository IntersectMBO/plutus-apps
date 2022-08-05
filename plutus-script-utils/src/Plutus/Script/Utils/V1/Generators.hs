{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Plutus.Script.Utils.V1.Generators
    ( alwaysSucceedValidator
    , alwaysSucceedValidatorHash
    , alwaysSucceedPolicy
    , alwaysSucceedPolicyHash
    , someTokenValue
    ) where

import Plutus.V1.Ledger.Scripts qualified as Ledger
import Plutus.V1.Ledger.Value (TokenName, Value)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified

import Plutus.Script.Utils.V1.Scripts qualified as Scripts

alwaysSucceedValidator :: Ledger.Validator
alwaysSucceedValidator =
    Ledger.mkValidatorScript $$(PlutusTx.compile [|| \_ _ _ -> () ||])

alwaysSucceedValidatorHash :: Ledger.ValidatorHash
alwaysSucceedValidatorHash = Scripts.validatorHash alwaysSucceedValidator

alwaysSucceedPolicy :: Ledger.MintingPolicy
alwaysSucceedPolicy =
    Ledger.mkMintingPolicyScript $$(PlutusTx.compile [|| \_ _ -> () ||])

alwaysSucceedPolicyHash :: Ledger.MintingPolicyHash
alwaysSucceedPolicyHash = Scripts.mintingPolicyHash alwaysSucceedPolicy

someTokenValue :: TokenName -> Integer -> Value
someTokenValue = Value.singleton (Scripts.scriptCurrencySymbol alwaysSucceedPolicy)
