{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module PlutusExample.PlutusVersion2.Sum
  where

import Prelude hiding (($), (+), (-), (==))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V2.Scripts qualified as Scripts
import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))


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

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedValidator validateSum

script :: Plutus.Script
script = Plutus.unValidatorScript validator

sumScriptShortBs :: SBS.ShortByteString
sumScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

sumScript :: PlutusScript PlutusScriptV2
sumScript = PlutusScriptSerialised sumScriptShortBs
