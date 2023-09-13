{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Plutus.Script.Utils.V2.Generators
    ( alwaysSucceedValidator
    , alwaysSucceedValidatorHash
    , alwaysSucceedPolicy
    , PV1.someTokenValue
    ) where

import Plutus.Script.Utils.Scripts (MintingPolicy, Validator, ValidatorHash, mkMintingPolicyScript, mkValidatorScript)
import PlutusTx qualified

import Plutus.Script.Utils.V1.Generators qualified as PV1
import Plutus.Script.Utils.V2.Scripts qualified as Scripts

alwaysSucceedValidator :: Validator
alwaysSucceedValidator =
    mkValidatorScript $$(PlutusTx.compile [|| \_ _ _ -> () ||])

alwaysSucceedValidatorHash :: ValidatorHash
alwaysSucceedValidatorHash = Scripts.validatorHash alwaysSucceedValidator

alwaysSucceedPolicy :: MintingPolicy
alwaysSucceedPolicy =
    mkMintingPolicyScript $$(PlutusTx.compile [|| \_ _ -> () ||])
