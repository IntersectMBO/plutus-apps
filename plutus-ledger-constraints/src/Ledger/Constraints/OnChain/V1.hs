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
import PlutusTx.Prelude (AdditiveSemigroup ((+)), Bool (False, True), Eq ((==)), Functor (fmap), Maybe (Just),
                         Ord ((<=), (>=)), all, any, elem, isJust, isNothing, maybe, snd, traceIfFalse, ($), (&&), (.))

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash))
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocReferenceScriptHash, ocValue),
                                         TxConstraint (MustBeSignedBy, MustIncludeDatumInTx, MustIncludeDatumInTxWithHash, MustMintValue, MustPayToOtherScript, MustPayToPubKeyAddress, MustProduceAtLeast, MustReferenceOutput, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustUseOutputAsCollateral, MustValidateIn),
                                         TxConstraintFun (MustSpendScriptOutputWithMatchingDatumAndValue),
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs),
                                         TxOutDatum (TxOutDatumHash, TxOutDatumInTx), getTxOutDatum)
import Ledger.Credential (Credential (ScriptCredential))
import Ledger.Value (geq, leq)
import Ledger.Value qualified as Value
import Plutus.V1.Ledger.Address qualified as Address
import Plutus.V1.Ledger.Contexts (ScriptContext (ScriptContext, scriptContextTxInfo),
                                  TxInInfo (TxInInfo, txInInfoOutRef, txInInfoResolved),
                                  TxInfo (txInfoData, txInfoInputs, txInfoMint, txInfoValidRange),
                                  TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue))
import Plutus.V1.Ledger.Contexts qualified as V
import Plutus.V1.Ledger.Interval (contains)

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
checkOwnOutputConstraint ctx@ScriptContext{scriptContextTxInfo} ScriptOutputConstraint{ocDatum, ocValue, ocReferenceScriptHash} =
    let d = fmap (Ledger.Datum . toBuiltinData) ocDatum
        hsh = V.findDatumHash (getTxOutDatum d) scriptContextTxInfo
        checkOutput (TxOutDatumHash _) TxOut{txOutValue, txOutDatumHash=Just _} =
            -- The datum is not added in the tx body with so we can't verify
            -- that the tx output's datum hash is the correct one w.r.t the
            -- provide datum.
               Ada.fromValue txOutValue >= Ada.fromValue ocValue
            && Ada.fromValue txOutValue <= Ada.fromValue ocValue + Ledger.maxMinAdaTxOut
            && Value.noAdaValue txOutValue == Value.noAdaValue ocValue
        checkOutput (TxOutDatumInTx _) TxOut{txOutValue, txOutDatumHash=Just svh} =
               Ada.fromValue txOutValue >= Ada.fromValue ocValue
            && Ada.fromValue txOutValue <= Ada.fromValue ocValue + Ledger.maxMinAdaTxOut
            && Value.noAdaValue txOutValue == Value.noAdaValue ocValue
            && hsh == Just svh
        checkOutput _ _ = False
    in traceIfFalse "L1" -- "Output constraint"
    $ any (checkOutput d) (V.getContinuingOutputs ctx)
        && isNothing ocReferenceScriptHash

{-# INLINABLE checkTxConstraint #-}
checkTxConstraint :: ScriptContext -> TxConstraint -> Bool
checkTxConstraint ctx@ScriptContext{scriptContextTxInfo} = \case
    MustIncludeDatumInTx dv ->
        traceIfFalse "L2" -- "Missing datum"
        $ dv `elem` fmap snd (txInfoData scriptContextTxInfo)
    MustValidateIn interval ->
        traceIfFalse "L3" -- "Wrong validation interval"
        $ interval `contains` txInfoValidRange scriptContextTxInfo
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
    MustPayToPubKeyAddress (PaymentPubKeyHash pk) _ mdv refScript vl ->
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
        traceIfFalse "La" -- "MustPayToPubKey"
        $ vl `leq` V.valuePaidTo scriptContextTxInfo pk
            && maybe True (\dv -> any (checkOutput dv) outs) mdv
            && isNothing refScript
    MustPayToOtherScript vlh _ dv refScript vl ->
        let outs = V.txInfoOutputs scriptContextTxInfo
            hsh d = V.findDatumHash d scriptContextTxInfo
            addr = Address.scriptHashAddress vlh
            checkOutput (TxOutDatumHash _) TxOut{txOutAddress, txOutValue, txOutDatumHash=Just _} =
                -- The datum is not added in the tx body with so we can't verify
                -- that the tx output's datum hash is the correct one w.r.t the
                -- provide datum.
                   Ada.fromValue txOutValue >= Ada.fromValue vl
                && Ada.fromValue txOutValue <= Ada.fromValue vl + Ledger.maxMinAdaTxOut
                && geq (Value.noAdaValue txOutValue) (Value.noAdaValue vl)
                && txOutAddress == addr
            checkOutput (TxOutDatumInTx d) TxOut{txOutAddress, txOutValue, txOutDatumHash=Just h} =
                   Ada.fromValue txOutValue >= Ada.fromValue vl
                && Ada.fromValue txOutValue <= Ada.fromValue vl + Ledger.maxMinAdaTxOut
                && geq (Value.noAdaValue txOutValue) (Value.noAdaValue vl)
                && hsh d == Just h
                && txOutAddress == addr
            -- By ledger rules, a script output with no datum is unspendable.
            -- Therefore, we always return False if there is no Datum with the
            -- script output.
            checkOutput _ _ = False
        in
        traceIfFalse "Lb" -- "MustPayToOtherScript"
        $ any (checkOutput dv) outs
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
