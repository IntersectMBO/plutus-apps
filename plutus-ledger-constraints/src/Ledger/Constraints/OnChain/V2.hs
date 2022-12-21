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
module Ledger.Constraints.OnChain.V2
    ( checkScriptContext
    , checkOwnInputConstraint
    , checkOwnOutputConstraint
    ) where

import Ledger qualified
import Ledger.Address (PaymentPubKeyHash (unPaymentPubKeyHash))
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icRedeemer, icReferenceTxOutRef, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocReferenceScriptHash, ocValue),
                                         TxConstraint (MustBeSignedBy, MustIncludeDatumInTx, MustIncludeDatumInTxWithHash, MustMintValue, MustPayToAddress, MustProduceAtLeast, MustReferenceOutput, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustUseOutputAsCollateral, MustValidateInTimeRange),
                                         TxConstraintFun (MustSpendScriptOutputWithMatchingDatumAndValue),
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs),
                                         TxOutDatum (TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline))
import Ledger.Constraints.ValidityInterval (toPlutusInterval)
import Ledger.Credential (Credential (ScriptCredential))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Contexts (ScriptContext (ScriptContext, scriptContextTxInfo), ScriptPurpose (Spending),
                                        TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
                                        TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoRedeemers, txInfoValidRange),
                                        TxOut (TxOut, txOutAddress, txOutDatum), findOwnInput)
import Plutus.Script.Utils.V2.Contexts qualified as PV2
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V1.Ledger.Value (leq)
import Plutus.V2.Ledger.Tx (OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash))
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Prelude (Bool (False, True), Eq ((==)), Maybe (Just, Nothing), all, any, elem, fmap, isJust, maybe,
                         traceIfFalse, ($), (&&), (.), (>>))

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
        (findOwnInput ctx)

{-# INLINABLE checkTxConstraint #-}
checkTxConstraint :: ScriptContext -> TxConstraint -> Bool
checkTxConstraint ctx@ScriptContext{scriptContextTxInfo} = \case
    MustIncludeDatumInTx dv ->
        traceIfFalse "L2" -- "Missing datum"
        $ dv `elem` AMap.elems (txInfoData scriptContextTxInfo)
    MustValidateInTimeRange interval ->
        traceIfFalse "L3" -- "Wrong validation interval"
        $ toPlutusInterval interval `contains` txInfoValidRange scriptContextTxInfo
    MustBeSignedBy pkh ->
        traceIfFalse "L4" -- "Missing signature"
        $ scriptContextTxInfo `PV2.txSignedBy` unPaymentPubKeyHash pkh
    MustSpendAtLeast vl ->
        traceIfFalse "L5" -- "Spent value not OK"
        $ vl `leq` PV2.valueSpent scriptContextTxInfo
    MustProduceAtLeast vl ->
        traceIfFalse "L6" -- "Produced value not OK"
        $ vl `leq` PV2.valueProduced scriptContextTxInfo
    MustSpendPubKeyOutput txOutRef ->
        let isNoOutputDatum NoOutputDatum = True
            isNoOutputDatum _             = False
        in
        traceIfFalse "L7" -- "Public key output not spent"
        $ maybe False (isNoOutputDatum . txOutDatum . txInInfoResolved) (PV2.findTxInByTxOutRef txOutRef scriptContextTxInfo)
    MustSpendScriptOutput txOutRef rdmr mRefTxOutRef ->
        traceIfFalse "L8" -- "Script output not spent"
        $ Just rdmr == AMap.lookup (Spending txOutRef) (txInfoRedeemers scriptContextTxInfo)
        && isJust (PV2.findTxInByTxOutRef txOutRef scriptContextTxInfo)
        && maybe True (\ref -> isJust (PV2.findTxRefInByTxOutRef ref scriptContextTxInfo)) mRefTxOutRef
    MustMintValue mps _ tn v mRefTxOutRef ->
        traceIfFalse "L9" -- "Value minted not OK"
        $ Value.valueOf (txInfoMint scriptContextTxInfo) (Value.mpsSymbol mps) tn == v
        && maybe True (\ref -> isJust (PV2.findTxRefInByTxOutRef ref scriptContextTxInfo)) mRefTxOutRef
    MustPayToAddress addr mdv _refScript vl ->
        let outs = PV2.txInfoOutputs scriptContextTxInfo
            hsh dv = PV2.findDatumHash dv scriptContextTxInfo
            checkOutput (TxOutDatumHash _) TxOut{txOutDatum=OutputDatumHash _} =
                -- The datum is not added in the tx body with so we can't verify
                -- that the tx output's datum hash is the correct one w.r.t the
                -- provide datum.
                True
            checkOutput (TxOutDatumInTx dv) TxOut{txOutDatum=OutputDatumHash dh} =
                hsh dv == Just dh
            checkOutput (TxOutDatumInline dv) TxOut{txOutDatum=OutputDatum d} =
                dv == d
            checkOutput _ _ = False
        in
        traceIfFalse "La" -- "MustPayToAddress"
        $ vl `leq` PV2.valuePaidTo scriptContextTxInfo addr
            && maybe True (\dv -> any (checkOutput dv) outs) mdv
    MustIncludeDatumInTxWithHash dvh dv ->
        traceIfFalse "Lc" -- "missing datum"
        $ PV2.findDatum dvh scriptContextTxInfo == Just dv
    MustSatisfyAnyOf xs ->
        traceIfFalse "Ld" -- "MustSatisfyAnyOf"
        $ any (all (checkTxConstraint ctx)) xs
    MustUseOutputAsCollateral _ ->
        True -- TxInfo does not have the collateral inputs
    MustReferenceOutput txOutRef ->
        traceIfFalse "Lf" -- "Output not referenced"
        $ isJust (PV2.findTxRefInByTxOutRef txOutRef scriptContextTxInfo)

{-# INLINABLE checkTxConstraintFun #-}
checkTxConstraintFun :: ScriptContext -> TxConstraintFun -> Bool
checkTxConstraintFun ScriptContext{scriptContextTxInfo} = \case
    MustSpendScriptOutputWithMatchingDatumAndValue vh datumPred valuePred rdmr ->
        let findDatum NoOutputDatum        = Nothing
            findDatum (OutputDatumHash dh) = PV2.findDatum dh scriptContextTxInfo
            findDatum (OutputDatum d)      = PV2.findDatumHash d scriptContextTxInfo >> Just d
            txOutIsMatch (TxOut (Ledger.Address (ScriptCredential vh') _) val (findDatum -> Just d) _refScript) =
                vh == vh' && valuePred val && datumPred d
            txOutIsMatch _ = False
            rdmrIsMatch txOutRef = Just rdmr == AMap.lookup (Spending txOutRef) (txInfoRedeemers scriptContextTxInfo)
        in
        traceIfFalse "Le" -- "MustSpendScriptOutputWithMatchingDatumAndValue"
        $ any (txOutIsMatch . txInInfoResolved) (txInfoInputs scriptContextTxInfo)
        && any (rdmrIsMatch . txInInfoOutRef) (txInfoInputs scriptContextTxInfo)
