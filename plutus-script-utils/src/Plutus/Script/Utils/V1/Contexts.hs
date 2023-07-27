{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Plutus.Script.Utils.V1.Contexts
    ( module Contexts
    , outputsAt
    , valuePaidTo
    ) where

import PlutusLedgerApi.V1 (Address, Value)
import PlutusLedgerApi.V1.Contexts as Contexts hiding (valuePaidTo)
import PlutusTx.Prelude (Maybe (Just, Nothing), mapMaybe, mconcat, (==))

{-# INLINABLE outputsAt #-}
-- | Get the values paid to a public key address by a pending transaction.
outputsAt :: Address -> TxInfo -> [Value]
outputsAt addr p =
    let flt TxOut{txOutAddress, txOutValue} | txOutAddress == addr = Just txOutValue
        flt _                                                      = Nothing
    in mapMaybe flt (txInfoOutputs p)

{-# INLINABLE valuePaidTo #-}
-- | Get the total value paid to a public key address by a pending transaction.
valuePaidTo :: TxInfo -> Address -> Value
valuePaidTo ptx addr = mconcat (outputsAt addr ptx)
