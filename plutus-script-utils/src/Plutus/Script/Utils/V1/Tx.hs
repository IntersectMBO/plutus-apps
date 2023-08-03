module Plutus.Script.Utils.V1.Tx
    ( scriptTxOut
    , scriptAddressTxOut
    ) where

import Plutus.Script.Utils.Scripts (Validator, datumHash)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import PlutusLedgerApi.V1 (Address, Datum, TxOut (TxOut), Value)

-- | Create a transaction output locked by a validator script and attach the given data
-- script.
scriptTxOut :: Validator -> Value -> Datum -> TxOut
scriptTxOut validator = scriptAddressTxOut (mkValidatorAddress validator)

-- | Create a transaction output locked by a validator script hash with the given data
-- script attached.
scriptAddressTxOut :: Address -> Value -> Datum -> TxOut
scriptAddressTxOut address val datum = TxOut address val (Just (datumHash datum))
