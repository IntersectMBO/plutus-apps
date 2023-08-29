{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Ledger.Scripts
    ( module Export
    , unitDatum
    , unitRedeemer
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
import PlutusLedgerApi.V1.Scripts as Export
import PlutusTx (toBuiltinData)
import PlutusTx.Builtins (BuiltinByteString, toBuiltin)


examplePlutusScriptAlwaysSucceedsHash :: WitCtx ctx -> BuiltinByteString
examplePlutusScriptAlwaysSucceedsHash = toBuiltin . serialiseToRawBytes . hashScript . PlutusScript PlutusScriptV1 . examplePlutusScriptAlwaysSucceeds

examplePlutusScriptAlwaysFailsHash :: WitCtx ctx -> BuiltinByteString
examplePlutusScriptAlwaysFailsHash = toBuiltin . serialiseToRawBytes . hashScript . PlutusScript PlutusScriptV1 . examplePlutusScriptAlwaysFails

-- | @()@ as a datum.
unitDatum :: Datum
unitDatum = Datum $ toBuiltinData ()

-- | @()@ as a redeemer.
unitRedeemer :: Redeemer
unitRedeemer = Redeemer $ toBuiltinData ()

