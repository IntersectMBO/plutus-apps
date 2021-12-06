{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

-- This solution has been contributed by George Flerovsky

module Cardano.PlutusExample.HelloWorldByteStringParametric
  ( hello
  , helloWorldSerialised
  , helloWorldSBS
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Ledger hiding (singleton)
import Ledger.Typed.Scripts qualified as Scripts
import PlutusTx qualified
import PlutusTx.Prelude as P hiding (Semigroup (..), unless)


{-
  The "hello world" message as a bytestring
-}

hello :: P.BuiltinByteString
hello = "Hello World!"

{-
   The Hello World validator script
-}

{-# INLINABLE helloWorld #-}

helloWorld :: P.BuiltinByteString -> P.BuiltinByteString -> () -> ScriptContext -> P.Bool
helloWorld keyword datum _redeemer _context = keyword P.== datum

{-
    As a ScriptInstance
-}

data HelloWorld
instance Scripts.ValidatorTypes HelloWorld where
    type instance DatumType HelloWorld = P.BuiltinByteString
    type instance RedeemerType HelloWorld = ()

helloWorldInstance :: Scripts.TypedValidator HelloWorld
helloWorldInstance = Scripts.mkTypedValidator @HelloWorld
    ($$(PlutusTx.compile [|| helloWorld ||]) `PlutusTx.applyCode` PlutusTx.liftCode hello)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @P.BuiltinByteString @()

{-
    As a Validator
-}

helloWorldValidator :: Validator
helloWorldValidator = Scripts.validatorScript helloWorldInstance


{-
    As a Script
-}

helloWorldScript :: Script
helloWorldScript = unValidatorScript helloWorldValidator

{-
    As a Short Byte String
-}

helloWorldSBS :: SBS.ShortByteString
helloWorldSBS =  SBS.toShort . LBS.toStrict $ serialise helloWorldScript

{-
    As a Serialised Script
-}

helloWorldSerialised :: PlutusScript PlutusScriptV1
helloWorldSerialised = PlutusScriptSerialised helloWorldSBS

