{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# LANGUAGE ViewPatterns      #-}
module Plutus.Script.Utils.V2.Contexts
    ( module Contexts
    , findTxInByTxOutRef
    , findTxRefInByTxOutRef
    , outputsAt
    , valuePaidTo
    , ownHash
    , ownHashes
    ) where

import Plutus.Script.Utils.Scripts (ValidatorHash (..))
import PlutusLedgerApi.V1 (Address, Value)
import PlutusLedgerApi.V2 qualified as PV2
import PlutusLedgerApi.V2.Contexts as Contexts hiding (findTxInByTxOutRef, valuePaidTo)
import PlutusTx.Prelude (Maybe (Just, Nothing), find, fst, mapMaybe, mconcat, traceError, (==))

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

{-# INLINABLE ownHashes #-}
-- | Get the validator and datum hashes of the output that is curently being validated
ownHashes :: ScriptContext -> (ValidatorHash, PV2.OutputDatum)
ownHashes (findOwnInput -> Just TxInInfo{txInInfoResolved=TxOut{txOutAddress=PV2.Address (PV2.ScriptCredential (PV2.ScriptHash s)) _, txOutDatum=d}}) = (ValidatorHash s,d)
ownHashes _ = traceError "Lg" -- "Can't get validator and datum hashes"

{-# INLINABLE ownHash #-}
-- | Get the hash of the validator script that is currently being validated.
ownHash :: ScriptContext -> ValidatorHash
ownHash p = fst (ownHashes p)
