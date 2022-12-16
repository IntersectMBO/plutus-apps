{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module PlutusExample.PlutusVersion2.MintingScript
  ( v2mintingScript
  , v2mintingScriptShortBs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&))

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.Typed as Scripts
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> V2.ScriptContext -> Bool
mkPolicy _redeemer _ctx = True

policy :: V2.MintingPolicy
policy = V2.mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = Scripts.mkUntypedMintingPolicy mkPolicy

plutusScript :: V2.Script
plutusScript =
  V2.unMintingPolicyScript policy

validator :: V2.Validator
validator = V2.Validator plutusScript

scriptAsCbor :: LBS.ByteString
scriptAsCbor = serialise validator

v2mintingScript :: PlutusScript PlutusScriptV2
v2mintingScript = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict scriptAsCbor

v2mintingScriptShortBs :: SBS.ShortByteString
v2mintingScriptShortBs = SBS.toShort . LBS.toStrict $ scriptAsCbor
