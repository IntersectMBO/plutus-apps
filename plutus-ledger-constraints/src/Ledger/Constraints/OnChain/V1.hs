{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ViewPatterns       #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Ledger.Constraints.OnChain.V1
    ( checkScriptContext
    , checkOwnInputConstraint
    , checkOwnOutputConstraint
    ) where

import PlutusTx (ToData (toBuiltinData))
import PlutusTx.Prelude (Bool (False, True), Eq ((==)), Functor (fmap), Maybe (Just), all, any, elem, isJust, isNothing,
                         maybe, snd, traceIfFalse, ($), (&&), (.))

import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icRedeemer, icReferenceTxOutRef, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocReferenceScriptHash, ocValue),
                                         TxConstraint (MustBeSignedBy, MustIncludeDatumInTx, MustIncludeDatumInTxWithHash, MustMintValue, MustPayToAddress, MustProduceAtLeast, MustReferenceOutput, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustUseOutputAsCollateral, MustValidateInTimeRange),
                                         TxConstraintFun (MustSpendScriptOutputWithMatchingDatumAndValue),
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs),
                                         TxOutDatum (TxOutDatumHash, TxOutDatumInTx))
import Ledger.Constraints.ValidityInterval (toPlutusInterval)
import Ledger.Credential (Credential (ScriptCredential))
import Ledger.Value (leq)
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V1.Contexts (ScriptContext (ScriptContext, scriptContextTxInfo),
                                        TxInInfo (TxInInfo, txInInfoResolved),
                                        TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoValidRange),
                                        TxOut (TxOut, txOutAddress, txOutDatumHash))
import Plutus.Script.Utils.V1.Contexts qualified as V
import Plutus.V1.Ledger.Interval (contains)

{-# INLINABLE checkScriptContext #-}
-- | Does the 'ScriptContext' satisfy the constraints?
checkScriptContext :: forall i o. (ToData i, ToData o) => TxConstraints i o -> ScriptContext -> Bool
checkScriptContext TxConstraints{txConstraints, txConstraintFuns = TxConstraintFuns txCnsFuns, txOwnInputs, txOwnOutputs} ptx =
    traceIfFalse "L!" -- "checkScriptContext failed"
    $ all (checkTxConstraint ptx) txConstraints
    && all (checkTxConstraintFun ptx) txCnsFuns
    && all (checkOwnInputConstraint ptx) txOwnInputs
    && all (checkOwnOutputConstraint ptx) txOwnOutputs

{-# INLINABLE checkOwnInputConstraint #-}
checkOwnInputConstraint
    :: ToData i
    => ScriptContext
    -> ScriptInputConstraint i
    -> Bool
checkOwnInputConstraint ctx ScriptInputConstraint{icTxOutRef, icRedeemer, icReferenceTxOutRef} =
    traceIfFalse "L0" -- "Input constraint"
    $ checkTxConstraint ctx (MustSpendScriptOutput icTxOutRef (Ledger.Redeemer $ toBuiltinData icRedeemer) icReferenceTxOutRef)

{-# INLINABLE checkOwnOutputConstraint #-}
checkOwnOutputConstraint
    :: ToData o
    => ScriptContext
    -> ScriptOutputConstraint o
    -> Bool
checkOwnOutputConstraint ctx ScriptOutputConstraint{ocDatum, ocValue, ocReferenceScriptHash} =
    let d = fmap (Ledger.Datum . toBuiltinData) ocDatum
    in traceIfFalse "L1" -- "Output constraint"
    $ maybe False (\TxInInfo{txInInfoResolved=TxOut{txOutAddress}} ->
                        checkTxConstraint ctx (MustPayToAddress txOutAddress (Just d) ocReferenceScriptHash ocValue))
        (V.findOwnInput ctx)

{-# INLINABLE checkTxConstraint #-}
checkTxConstraint :: ScriptContext -> TxConstraint -> Bool
checkTxConstraint ctx@ScriptContext{scriptContextTxInfo} = \case
    MustIncludeDatumInTx dv ->
        traceIfFalse "L2" -- "Missing datum"
        $ dv `elem` fmap snd (txInfoData scriptContextTxInfo)
    MustValidateInTimeRange interval ->
        traceIfFalse "L3" -- "Wrong validation interval"
        $ toPlutusInterval interval `contains` txInfoValidRange scriptContextTxInfo
    MustBeSignedBy pkh ->
        traceIfFalse "L4" -- "Missing signature"
        $ scriptContextTxInfo `V.txSignedBy` unPaymentPubKeyHash pkh
    MustSpendAtLeast vl ->
        traceIfFalse "L5" -- "Spent value not OK"
        $ vl `leq` V.valueSpent scriptContextTxInfo
    MustProduceAtLeast vl ->
        traceIfFalse "L6" -- "Produced value not OK"
        $ vl `leq` V.valueProduced scriptContextTxInfo
    MustSpendPubKeyOutput txOutRef ->
        traceIfFalse "L7" -- "Public key output not spent"
        $ maybe False (isNothing . txOutDatumHash . txInInfoResolved) (V.findTxInByTxOutRef txOutRef scriptContextTxInfo)
    MustSpendScriptOutput txOutRef _ _ ->
        traceIfFalse "L8" -- "Script output not spent"
        -- Unfortunately we can't check the redeemer, because TxInfo only
        -- gives us the redeemer's hash, but 'MustSpendScriptOutput' gives
        -- us the full redeemer
        $ isJust (V.findTxInByTxOutRef txOutRef scriptContextTxInfo)
    MustMintValue mps _ tn v _ ->
        traceIfFalse "L9" -- "Value minted not OK"
        $ Value.valueOf (txInfoMint scriptContextTxInfo) (Value.mpsSymbol mps) tn == v
    MustPayToAddress addr mdv refScript vl ->
        let outs = V.txInfoOutputs scriptContextTxInfo
            hsh dv = V.findDatumHash dv scriptContextTxInfo
            checkOutput (TxOutDatumHash _) TxOut{txOutDatumHash=Just _} =
                -- The datum is not added in the tx body with so we can't verify
                -- that the tx output's datum hash is the correct one w.r.t the
                -- provide datum.
                True
            checkOutput (TxOutDatumInTx dv) TxOut{txOutDatumHash=Just svh} =
                hsh dv == Just svh
            checkOutput _ _ = False
        in
        traceIfFalse "La" -- "MustPayToAddress"
        $ vl `leq` V.valuePaidTo scriptContextTxInfo addr
            && maybe True (\dv -> any (checkOutput dv) outs) mdv
            && isNothing refScript
    MustIncludeDatumInTxWithHash dvh dv ->
        traceIfFalse "Lc" -- "missing datum"
        $ V.findDatum dvh scriptContextTxInfo == Just dv
    MustSatisfyAnyOf xs ->
        traceIfFalse "Ld" -- "MustSatisfyAnyOf"
        $ any (all (checkTxConstraint ctx)) xs
    MustUseOutputAsCollateral _ ->
        True -- TxInfo does not have the collateral inputs
    MustReferenceOutput _ ->
        traceIfFalse "Lf" -- "Cannot use reference inputs in PlutusV1.ScriptContext"
        False

{-# INLINABLE checkTxConstraintFun #-}
checkTxConstraintFun :: ScriptContext -> TxConstraintFun -> Bool
checkTxConstraintFun ScriptContext{scriptContextTxInfo} = \case
    MustSpendScriptOutputWithMatchingDatumAndValue vh datumPred valuePred _ ->
        let findDatum mdh = do
                dh <- mdh
                V.findDatum dh scriptContextTxInfo
            isMatch (TxOut (Ledger.Address (ScriptCredential vh') _) val (findDatum -> Just d)) =
                vh == vh' && valuePred val && datumPred d
            isMatch _ = False
        in
        traceIfFalse "Le" -- "MustSpendScriptOutputWithMatchingDatumAndValue"
        $ any (isMatch . txInInfoResolved) (txInfoInputs scriptContextTxInfo)
