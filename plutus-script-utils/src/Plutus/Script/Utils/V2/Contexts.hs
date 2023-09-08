{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Plutus.Script.Utils.V2.Contexts
    ( module Contexts
    , findTxInByTxOutRef
    , findTxRefInByTxOutRef
    , outputsAt
    , valuePaidTo
    ) where

import PlutusLedgerApi.V1 (Address, Value)
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V2.Contexts as Contexts hiding (findTxInByTxOutRef, valuePaidTo)
import PlutusTx.Prelude (Maybe (Just, Nothing), find, mapMaybe, mconcat, (==))

{-# INLINABLE findTxInByTxOutRef #-}
findTxInByTxOutRef :: TxOutRef -> PV2.TxInfo -> Maybe PV2.TxInInfo
findTxInByTxOutRef outRef PV2.TxInfo{PV2.txInfoInputs} =
    find (\PV2.TxInInfo{PV2.txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoInputs

{-# INLINABLE findTxRefInByTxOutRef #-}
findTxRefInByTxOutRef :: TxOutRef -> PV2.TxInfo -> Maybe PV2.TxInInfo
findTxRefInByTxOutRef outRef PV2.TxInfo{PV2.txInfoReferenceInputs} =
    find (\PV2.TxInInfo{PV2.txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoReferenceInputs

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
