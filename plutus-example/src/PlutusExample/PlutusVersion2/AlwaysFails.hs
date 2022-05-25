{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module PlutusExample.PlutusVersion2.AlwaysFails
  ( alwaysFailsScript
  , alwaysFailsScriptShortBs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless, (.))

{-# INLINABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _ _ _ = PlutusTx.Prelude.error ()

validator :: Plutus.Validator
validator = Plutus.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: Plutus.Script
script = Plutus.unValidatorScript validator

alwaysFailsScriptShortBs :: SBS.ShortByteString
alwaysFailsScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

alwaysFailsScript :: PlutusScript PlutusScriptV2
alwaysFailsScript = PlutusScriptSerialised alwaysFailsScriptShortBs

