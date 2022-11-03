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
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash))
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocValue),
                                         TxConstraint (MustBeSignedBy, MustIncludeDatumInTx, MustIncludeDatumInTxWithHash, MustMintValue, MustPayToOtherScript, MustPayToPubKeyAddress, MustProduceAtLeast, MustReferenceOutput, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustUseOutputAsCollateral, MustValidateIn),
                                         TxConstraintFun (MustSpendScriptOutputWithMatchingDatumAndValue),
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs),
                                         TxOutDatum (TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline), getTxOutDatum,
                                         isTxOutDatumInTx)
import Ledger.Credential (Credential (ScriptCredential))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Contexts qualified as PV2 hiding (findTxInByTxOutRef)
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V1.Ledger.Value (geq, leq)
import Plutus.V2.Ledger.Contexts (ScriptContext (ScriptContext, scriptContextTxInfo), ScriptPurpose (Spending),
                                  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
                                  TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoRedeemers, txInfoValidRange),
                                  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue))
import Plutus.V2.Ledger.Contexts qualified as PV2
import Plutus.V2.Ledger.Tx (OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash))
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Prelude (AdditiveSemigroup ((+)), Bool (False, True), Eq ((==)), Functor (fmap), Maybe (Just, Nothing),
                         Ord ((<=), (>=)), all, any, elem, isJust, maybe, not, traceIfFalse, ($), (&&), (.), (>>), (||))

{-# INLINABLE checkScriptContext #-}
-- | Does the 'ScriptContext' satisfy the constraints?
checkScriptContext :: forall i o. ToData o => TxConstraints i o -> ScriptContext -> Bool
checkScriptContext TxConstraints{txConstraints, txConstraintFuns = TxConstraintFuns txCnsFuns, txOwnInputs, txOwnOutputs} ptx =
    traceIfFalse "L!" -- "checkScriptContext failed"
    $ all (checkTxConstraint ptx) txConstraints
    && all (checkTxConstraintFun ptx) txCnsFuns
    && all (checkOwnInputConstraint ptx) txOwnInputs
    && all (checkOwnOutputConstraint ptx) txOwnOutputs

{-# INLINABLE checkOwnInputConstraint #-}
checkOwnInputConstraint :: ScriptContext -> ScriptInputConstraint a -> Bool
checkOwnInputConstraint ScriptContext{scriptContextTxInfo} ScriptInputConstraint{icTxOutRef} =
    let checkInput TxInInfo{txInInfoOutRef} =
            txInInfoOutRef == icTxOutRef -- TODO: We should also check the redeemer but we can't right now because it's hashed
    in traceIfFalse "L0" -- "Input constraint"
    $ any checkInput (txInfoInputs scriptContextTxInfo)

{-# INLINABLE checkOwnOutputConstraint #-}
checkOwnOutputConstraint
    :: ToData o
    => ScriptContext
    -> ScriptOutputConstraint o
    -> Bool
checkOwnOutputConstraint ctx@ScriptContext{scriptContextTxInfo} ScriptOutputConstraint{ocDatum, ocValue} =
    let d = fmap (Ledger.Datum . toBuiltinData) ocDatum
        hsh = PV2.findDatumHash (getTxOutDatum d) scriptContextTxInfo
        checkOutput (TxOutDatumHash _) TxOut{txOutValue, txOutDatum=OutputDatumHash _} =
            -- The datum is not added in the tx body with so we can't verify
            -- that the tx output's datum hash is the correct one w.r.t the
            -- provide datum.
               Ada.fromValue txOutValue >= Ada.fromValue ocValue
            && Ada.fromValue txOutValue <= Ada.fromValue ocValue+ Ledger.maxMinAdaTxOut
            && Value.noAdaValue txOutValue == Value.noAdaValue ocValue
        checkOutput txOutDatum@(TxOutDatumInTx _) TxOut{txOutValue, txOutDatum=OutputDatumHash dh} =
               Ada.fromValue txOutValue >= Ada.fromValue ocValue
            && Ada.fromValue txOutValue <= Ada.fromValue ocValue + Ledger.maxMinAdaTxOut
            && Value.noAdaValue txOutValue == Value.noAdaValue ocValue
            -- False iif the datum was added in the transaction body and the
            -- hash in the transaction output does not match.
            && (not (isTxOutDatumInTx txOutDatum) || hsh == Just dh)
        checkOutput txOutDatum@(TxOutDatumInline _) TxOut{txOutValue, txOutDatum=OutputDatum id} =
               Ada.fromValue txOutValue >= Ada.fromValue ocValue
            && Ada.fromValue txOutValue <= Ada.fromValue ocValue + Ledger.maxMinAdaTxOut
            && Value.noAdaValue txOutValue == Value.noAdaValue ocValue
            && txOutDatum == TxOutDatumInline id
        checkOutput _ _ = False
    in traceIfFalse "L1" -- "Output constraint"
    $ any (checkOutput d) (PV2.getContinuingOutputs ctx)

{-# INLINABLE checkTxConstraint #-}
checkTxConstraint :: ScriptContext -> TxConstraint -> Bool
checkTxConstraint ctx@ScriptContext{scriptContextTxInfo} = \case
    MustIncludeDatumInTx dv ->
        traceIfFalse "L2" -- "Missing datum"
        $ dv `elem` AMap.elems (txInfoData scriptContextTxInfo)
    MustValidateIn interval ->
        traceIfFalse "L3" -- "Wrong validation interval"
        $ interval `contains` txInfoValidRange scriptContextTxInfo
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
    MustPayToPubKeyAddress (PaymentPubKeyHash pk) _skh mdv _refScript vl ->
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
        traceIfFalse "La" -- "MustPayToPubKey"
        $ vl `leq` PV2.valuePaidTo scriptContextTxInfo pk
            && maybe True (\dv -> any (checkOutput dv) outs) mdv
    MustPayToOtherScript vlh _skh dv _refScript vl ->
        let outs = PV2.txInfoOutputs scriptContextTxInfo
            hsh = PV2.findDatumHash (getTxOutDatum dv) scriptContextTxInfo
            addr = Address (ScriptCredential vlh) Nothing
            checkOutput (TxOutDatumHash _) TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatumHash _} =
                -- The datum is not added in the tx body with so we can't verify
                -- that the tx output's datum hash is the correct one w.r.t the
                -- provide datum.
                   Ada.fromValue txOutValue >= Ada.fromValue vl
                && Ada.fromValue txOutValue <= Ada.fromValue vl + Ledger.maxMinAdaTxOut
                && geq (Value.noAdaValue txOutValue) (Value.noAdaValue vl)
                && txOutAddress == addr
            checkOutput (TxOutDatumInTx _) TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatumHash h} =
                   Ada.fromValue txOutValue >= Ada.fromValue vl
                && Ada.fromValue txOutValue <= Ada.fromValue vl + Ledger.maxMinAdaTxOut
                && geq (Value.noAdaValue txOutValue) (Value.noAdaValue vl)
                && hsh == Just h
                && txOutAddress == addr
            -- With regards to inline datum, we have the actual datum in the tx
            -- output. Therefore, we can compare it with the provided datum.
            checkOutput (TxOutDatumInline d) TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatum id} =
                   Ada.fromValue txOutValue >= Ada.fromValue vl
                && Ada.fromValue txOutValue <= Ada.fromValue vl + Ledger.maxMinAdaTxOut
                && Value.noAdaValue txOutValue == Value.noAdaValue vl
                && d == id
                && txOutAddress == addr
            checkOutput _ _ = False
        in
        traceIfFalse "Lb" -- "MustPayToOtherScript"
        $ any (checkOutput dv) outs
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
