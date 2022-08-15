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
                                         TxConstraint (MustBeSignedBy, MustHashDatum, MustIncludeDatum, MustMintValue, MustPayToOtherScript, MustPayToPubKeyAddress, MustProduceAtLeast, MustReferenceOutput, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustUseOutputAsCollateral, MustValidateIn),
                                         TxConstraintFun (MustSpendScriptOutputWithMatchingDatumAndValue),
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs))
import Ledger.Credential (Credential (ScriptCredential))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Contexts qualified as PV2 hiding (findTxInByTxOutRef)
import Plutus.V1.Ledger.Address (Address (Address))
import Plutus.V1.Ledger.Interval (contains)
import Plutus.V1.Ledger.Value (leq)
import Plutus.V2.Ledger.Contexts (ScriptContext (ScriptContext, scriptContextTxInfo),
                                  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
                                  TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoValidRange),
                                  TxOut (TxOut, txOutAddress, txOutDatum, txOutValue))
import Plutus.V2.Ledger.Contexts qualified as PV2
import Plutus.V2.Ledger.Tx (OutputDatum (NoOutputDatum, OutputDatum, OutputDatumHash))
import PlutusTx (ToData (toBuiltinData))
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Prelude (AdditiveSemigroup ((+)), Bool (False, True), Eq ((==)), Maybe (Just, Nothing),
                         Ord ((<=), (>=)), all, any, elem, isJust, maybe, traceIfFalse, ($), (&&), (.), (>>))

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
    let d = Ledger.Datum $ toBuiltinData ocDatum
        hsh = PV2.findDatumHash d scriptContextTxInfo
        checkOutput TxOut{txOutValue, txOutDatum=OutputDatumHash dh} =
               Ada.fromValue txOutValue >= Ada.fromValue ocValue
            && Ada.fromValue txOutValue <= Ada.fromValue ocValue + Ledger.minAdaTxOut
            && Value.noAdaValue txOutValue == Value.noAdaValue ocValue
            && hsh == Just dh
        checkOutput TxOut{txOutValue, txOutDatum=OutputDatum id} =
               Ada.fromValue txOutValue >= Ada.fromValue ocValue
            && Ada.fromValue txOutValue <= Ada.fromValue ocValue + Ledger.minAdaTxOut
            && Value.noAdaValue txOutValue == Value.noAdaValue ocValue
            && d == id
        checkOutput _       = False
    in traceIfFalse "L1" -- "Output constraint"
    $ any checkOutput (PV2.getContinuingOutputs ctx)

{-# INLINABLE checkTxConstraint #-}
checkTxConstraint :: ScriptContext -> TxConstraint -> Bool
checkTxConstraint ctx@ScriptContext{scriptContextTxInfo} = \case
    MustIncludeDatum dv ->
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
    MustSpendScriptOutput txOutRef _ ->
        traceIfFalse "L8" -- "Script output not spent"
        -- Unfortunately we can't check the redeemer, because TxInfo only
        -- gives us the redeemer's hash, but 'MustSpendScriptOutput' gives
        -- us the full redeemer
        $ isJust (PV2.findTxInByTxOutRef txOutRef scriptContextTxInfo)
    MustMintValue mps _ tn v ->
        traceIfFalse "L9" -- "Value minted not OK"
        $ Value.valueOf (txInfoMint scriptContextTxInfo) (Value.mpsSymbol mps) tn == v
    MustPayToPubKeyAddress (PaymentPubKeyHash pk) _ mdv vl ->
        let outs = PV2.txInfoOutputs scriptContextTxInfo
            hsh dv = PV2.findDatumHash dv scriptContextTxInfo
            checkOutput (Just dv) TxOut{txOutDatum=OutputDatumHash dh} = hsh dv == Just dh
            checkOutput (Just dv) TxOut{txOutDatum=OutputDatum d}      = dv == d
            -- return 'True' by default meaning we fail only when the provided datum is not found
            checkOutput _ _                                            = True
        in
        traceIfFalse "La" -- "MustPayToPubKey"
        $ vl `leq` PV2.valuePaidTo scriptContextTxInfo pk && any (checkOutput mdv) outs
    MustPayToOtherScript vlh _ dv vl ->
        let outs = PV2.txInfoOutputs scriptContextTxInfo
            hsh = PV2.findDatumHash dv scriptContextTxInfo
            addr = Address (ScriptCredential vlh) Nothing
            checkOutput TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatumHash dh} =
                   Ada.fromValue txOutValue >= Ada.fromValue vl
                && Ada.fromValue txOutValue <= Ada.fromValue vl + Ledger.minAdaTxOut
                && Value.noAdaValue txOutValue == Value.noAdaValue vl
                && hsh == Just dh
                && txOutAddress == addr
            checkOutput TxOut{txOutAddress, txOutValue, txOutDatum=OutputDatum id} =
                   Ada.fromValue txOutValue >= Ada.fromValue vl
                && Ada.fromValue txOutValue <= Ada.fromValue vl + Ledger.minAdaTxOut
                && Value.noAdaValue txOutValue == Value.noAdaValue vl
                && dv == id
                && txOutAddress == addr
            checkOutput _ = False
        in
        traceIfFalse "Lb" -- "MustPayToOtherScript"
        $ any checkOutput outs
    MustHashDatum dvh dv ->
        traceIfFalse "Lc" -- "MustHashDatum"
        $ PV2.findDatum dvh scriptContextTxInfo == Just dv
    MustSatisfyAnyOf xs ->
        traceIfFalse "Ld" -- "MustSatisfyAnyOf"
        $ any (all (checkTxConstraint ctx)) xs
    MustUseOutputAsCollateral _ ->
        True -- TODO
    MustReferenceOutput txOutRef ->
        traceIfFalse "Lf" -- "Output not referenced"
        $ isJust (PV2.findTxRefInByTxOutRef txOutRef scriptContextTxInfo)

{-# INLINABLE checkTxConstraintFun #-}
checkTxConstraintFun :: ScriptContext -> TxConstraintFun -> Bool
checkTxConstraintFun ScriptContext{scriptContextTxInfo} = \case
    MustSpendScriptOutputWithMatchingDatumAndValue vh datumPred valuePred _ ->
        let findDatum NoOutputDatum        = Nothing
            findDatum (OutputDatumHash dh) = PV2.findDatum dh scriptContextTxInfo
            findDatum (OutputDatum d)      = PV2.findDatumHash d scriptContextTxInfo >> Just d
            isMatch (TxOut (Ledger.Address (ScriptCredential vh') _) val (findDatum -> Just d) _refScript) =
                vh == vh' && valuePred val && datumPred d
            isMatch _ = False
        in
        traceIfFalse "Le" -- "MustSpendScriptOutputWithMatchingDatumAndValue"
        $ any (isMatch . txInInfoResolved) (txInfoInputs scriptContextTxInfo)
