{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module PlutusExample.PlutusVersion1.MintingScript
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.Typed qualified as Scripts
import Plutus.V1.Ledger.Api (MintingPolicy, Script, ScriptContext, Validator (Validator), mkMintingPolicyScript,
                             unMintingPolicyScript)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{- HLINT ignore "Avoid lambda" -}

{-# INLINABLE mkPolicy #-}
mkPolicy :: BuiltinData -> ScriptContext -> Bool
mkPolicy _redeemer _ctx = True

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = Scripts.mkUntypedMintingPolicy mkPolicy

plutusScript :: Script
plutusScript =
  unMintingPolicyScript policy

validator :: Validator
validator =
  Validator $ unMintingPolicyScript policy

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor
