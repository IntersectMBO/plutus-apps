{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module PlutusExample.PlutusVersion2.EcdsaSecp256k1Loop
    ( v2EcdsaLoopScript
    , v2EcdsaLoopScriptShortBs
    ) where

import Cardano.Api (PlutusScript, PlutusScriptV2)
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise (serialise)
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.V2.Ledger.Api qualified as PlutusV2
import PlutusTx qualified
import PlutusTx.Builtins qualified as BI
import PlutusTx.Prelude as P hiding (Semigroup (..), unless, (.))
import Prelude ((.))

{-# INLINEABLE mkValidator #-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkValidator _datum red _txContext =
  case PlutusV2.fromBuiltinData red of
    Nothing -> P.traceError "Trace error: Invalid redeemer"
    Just (n, vkey, msg, sig) ->
      if n < (0 :: Integer)
      then traceError "redeemer is < 0"
      else loop n vkey msg sig
  where
    loop i v m s
      | i == 0 = ()
      | BI.verifyEcdsaSecp256k1Signature v m s = loop (pred i) v m s
      | otherwise = P.traceError "Trace error: Ecdsa validation failed"

validator :: PlutusV2.Validator
validator = PlutusV2.mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

script :: PlutusV2.Script
script = PlutusV2.unValidatorScript validator

v2EcdsaLoopScriptShortBs :: SBS.ShortByteString
v2EcdsaLoopScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

v2EcdsaLoopScript :: PlutusScript PlutusScriptV2
v2EcdsaLoopScript = PlutusScriptSerialised v2EcdsaLoopScriptShortBs
