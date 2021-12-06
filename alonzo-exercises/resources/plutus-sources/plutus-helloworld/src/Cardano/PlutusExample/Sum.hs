{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.PlutusExample.Sum
  ( sumWrappedSerialised
  , sumWrappedSBS
  , sumDataSerialised
  , sumDataSBS
  ) where

import Prelude qualified as Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude as P hiding (Semigroup (..), unless)

smartSum :: Integer -> Integer
smartSum a = loop a 0
 where
  loop !n !acc = if n==0
    then acc
    else loop (n - 1) (n + acc)

-- | The validation function (DataValue -> RedeemerValue -> ScriptContext -> Bool)
{-# INLINABLE validateSum #-}
validateSum :: Integer -> Integer -> x -> Bool
validateSum n s _ = isGoodSum n s

{-# INLINABLE isGoodSum #-}
isGoodSum :: Integer -> Integer -> Bool
isGoodSum n s = smartSum n == s

data SmartSum
instance Scripts.ValidatorTypes SmartSum where
    type instance RedeemerType SmartSum = Integer
    type instance DatumType SmartSum = Integer

sumInstance :: Scripts.TypedValidator SmartSum
sumInstance = Scripts.mkTypedValidator @SmartSum
    $$(PlutusTx.compile [|| validateSum ||])
    $$(PlutusTx.compile [|| wrap ||])
      where
        wrap = Scripts.wrapValidator @Integer @Integer

validator :: Plutus.Validator
validator = Scripts.validatorScript sumInstance

script :: Plutus.Script
script = Plutus.unValidatorScript validator

sumWrappedSBS :: SBS.ShortByteString
sumWrappedSBS = SBS.toShort . LBS.toStrict $ serialise script

sumWrappedSerialised :: PlutusScript PlutusScriptV1
sumWrappedSerialised = PlutusScriptSerialised sumWrappedSBS

{-# INLINABLE sumData #-}
sumData :: BuiltinData -> BuiltinData -> BuiltinData -> ()
sumData datum redeemer _context
  = if isGoodSum (PlutusTx.unsafeFromBuiltinData datum) (PlutusTx.unsafeFromBuiltinData redeemer)
    then ()
    else P.error ()

sumDataValidator :: Plutus.Validator
sumDataValidator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| sumData ||])

sumDataScript :: Plutus.Script
sumDataScript = Plutus.unValidatorScript sumDataValidator

sumDataSBS :: SBS.ShortByteString
sumDataSBS =  SBS.toShort . LBS.toStrict $ serialise sumDataScript

sumDataSerialised :: PlutusScript PlutusScriptV1
sumDataSerialised = PlutusScriptSerialised sumDataSBS
