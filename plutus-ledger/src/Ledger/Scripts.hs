{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Ledger.Scripts
    ( module Export
    , examplePlutusScriptAlwaysSucceeds
    , examplePlutusScriptAlwaysFails
    , examplePlutusScriptAlwaysSucceedsHash
    , examplePlutusScriptAlwaysFailsHash
    , WitCtx (..)
    ) where

import Cardano.Api (PlutusScriptVersion (PlutusScriptV1), Script (PlutusScript), WitCtx (..),
                    examplePlutusScriptAlwaysFails, examplePlutusScriptAlwaysSucceeds, hashScript, serialiseToRawBytes)
import Ledger.Scripts.Orphans ()
import Plutus.Script.Utils.Scripts as Export
import Plutus.Script.Utils.V1.Scripts as Export
import Plutus.V1.Ledger.Scripts as Export
import PlutusTx.Builtins.Internal (BuiltinByteString (..))


examplePlutusScriptAlwaysSucceedsHash :: WitCtx ctx -> BuiltinByteString
examplePlutusScriptAlwaysSucceedsHash = BuiltinByteString . serialiseToRawBytes . hashScript . PlutusScript PlutusScriptV1 . examplePlutusScriptAlwaysSucceeds

examplePlutusScriptAlwaysFailsHash :: WitCtx ctx -> BuiltinByteString
examplePlutusScriptAlwaysFailsHash = BuiltinByteString . serialiseToRawBytes . hashScript . PlutusScript PlutusScriptV1 . examplePlutusScriptAlwaysFails
