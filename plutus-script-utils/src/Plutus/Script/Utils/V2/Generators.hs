{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Plutus.Script.Utils.V2.Generators
    ( alwaysSucceedValidator
    , alwaysSucceedValidatorHash
    , alwaysSucceedPolicy
    , PV1.someTokenValue
    ) where

import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified

import Plutus.Script.Utils.V1.Generators qualified as PV1
import Plutus.Script.Utils.V2.Scripts qualified as Scripts

alwaysSucceedValidator :: PV2.Validator
alwaysSucceedValidator =
    PV2.mkValidatorScript $$(PlutusTx.compile [|| \_ _ _ -> () ||])

alwaysSucceedValidatorHash :: PV2.ValidatorHash
alwaysSucceedValidatorHash = Scripts.validatorHash alwaysSucceedValidator

alwaysSucceedPolicy :: PV2.MintingPolicy
alwaysSucceedPolicy =
    PV2.mkMintingPolicyScript $$(PlutusTx.compile [|| \_ _ -> () ||])
