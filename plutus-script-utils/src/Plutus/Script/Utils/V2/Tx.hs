module Plutus.Script.Utils.V2.Tx
    ( scriptTxOut
    ) where

import Plutus.Script.Utils.Scripts (Validator, getValidator)
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
import Plutus.Script.Utils.V2.Scripts (scriptHash)
import PlutusLedgerApi.V2 (OutputDatum, TxOut (TxOut), Value)

type HasReferenceScript = Bool

-- | Create a transaction output locked by a validator script and attach the given data
-- script.
scriptTxOut :: Validator -> Value -> OutputDatum -> HasReferenceScript -> TxOut
scriptTxOut validator val datum True =
    TxOut (mkValidatorAddress validator) val datum (Just $ scriptHash $ getValidator validator)
scriptTxOut validator val datum False =
    TxOut (mkValidatorAddress validator) val datum Nothing
