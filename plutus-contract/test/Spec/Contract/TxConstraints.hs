{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Spec.Contract.TxConstraints (tests) where

import Control.Lens hiding ((.>))
import Control.Monad (void)
import Control.Monad.Freer.Extras.Log (LogLevel (Debug))
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Void (Void)
import Test.Tasty (TestTree, testGroup)

import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as TC
import Ledger.Constraints.OnChain.V1 qualified as TCV1
import Ledger.Constraints.OnChain.V2 qualified as TCV2
import Ledger.Scripts (unitRedeemer)
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Plutus.Contract as Con
import Plutus.Contract.Test (TracePredicate, assertValidatedTransactionCount, assertValidatedTransactionCountOfTotal,
                             checkPredicate, checkPredicateOptions, defaultCheckOptions, minLogLevel, valueAtAddress,
                             w1, walletFundsChange, (.&&.))
import Plutus.Script.Utils.Typed (Any)
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V1.Typed.Scripts qualified as PV1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PV2
import Plutus.Trace.Emulator (ContractInstanceTag, EmulatorTrace, activateContract)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Address qualified as Addr
import Plutus.V1.Ledger.Api (Address, Datum (Datum), TxOutRef)
import Plutus.V1.Ledger.Api qualified as PV1
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import Prelude hiding (not)

run :: String -> TracePredicate -> EmulatorTrace () -> TestTree
run = checkPredicateOptions (defaultCheckOptions & minLogLevel .~ Debug)

check :: String -> Contract () EmptySchema ContractError () -> _ -> TestTree
check nm contract pred = run nm (pred contract) (void $ activateContract w1 contract tag)

tag :: ContractInstanceTag
tag = "instance 1"

tests :: TestTree
tests = testGroup "contract tx constraints"

    -- Testing package plutus-ledger-constraints

    [ checkPredicate "mustReferenceOutput returns False on-chain when used for unlocking funds in a PlutusV1 script"
        (walletFundsChange w1 (Ada.adaValueOf (-5))
        .&&. valueAtAddress mustReferenceOutputV1ValidatorAddress (== Ada.adaValueOf 5)
        .&&. assertValidatedTransactionCountOfTotal 1 2
        ) $ do
            void $ activateContract w1 mustReferenceOutputV1ConTest tag
            void $ Trace.waitNSlots 2

    , checkPredicate "mustReferenceOutput can be used on-chain to unlock funds in a PlutusV2 script"
        (walletFundsChange w1 (Ada.adaValueOf 0)
        .&&. valueAtAddress mustReferenceOutputV2ValidatorAddress (== Ada.adaValueOf 0)
        .&&. assertValidatedTransactionCount 2
        ) $ do
            void $ activateContract w1 mustReferenceOutputV2ConTest tag
            void $ Trace.waitNSlots 3

    -- Testing package plutus-tx-constraints

    , checkPredicate "Tx.Constraints.mustReferenceOutput fails when trying to unlock funds in a PlutusV1 script"
        (walletFundsChange w1 (Ada.adaValueOf (-5))
        .&&. valueAtAddress mustReferenceOutputV1ValidatorAddress (== Ada.adaValueOf 5)
        .&&. assertValidatedTransactionCountOfTotal 1 1
        ) $ do
            void $ activateContract w1 mustReferenceOutputTxV1ConTest tag
            void $ Trace.waitNSlots 2

    , checkPredicate "Tx.Constraints.mustReferenceOutput can be used on-chain to unlock funds in a PlutusV2 script"
        (walletFundsChange w1 (Ada.adaValueOf 0)
        .&&. valueAtAddress mustReferenceOutputV2ValidatorAddress (== Ada.adaValueOf 0)
        .&&. assertValidatedTransactionCount 2
        ) $ do
            void $ activateContract w1 mustReferenceOutputTxV2ConTest tag
            void $ Trace.waitNSlots 3
    ]

{-
Test case: mustReferenceOutput returns False on-chain when used for unlocking funds in a PlutusV1 script"
-}

{-# INLINABLE mkMustReferenceOutputV1Validator #-}
mkMustReferenceOutputV1Validator :: TxOutRef -> () -> PV1.ScriptContext -> Bool
mkMustReferenceOutputV1Validator txOutRef _ =
    TCV1.checkScriptContext @Void @Void (TC.mustReferenceOutput txOutRef)

{-# INLINABLE mustReferenceOutputV1Validator #-}
mustReferenceOutputV1Validator :: PV1.Validator
mustReferenceOutputV1Validator = PV1.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PV1.mkUntypedValidator mkMustReferenceOutputV1Validator

mustReferenceOutputV1ValidatorAddress :: Address
mustReferenceOutputV1ValidatorAddress =
    PV1.mkValidatorAddress mustReferenceOutputV1Validator

mustReferenceOutputV1ConTest :: Contract () EmptySchema ContractError ()
mustReferenceOutputV1ConTest = do

    utxos <- ownUtxos
    let ((utxoRef, utxo), (utxoRefForBalance1, _), (utxoRefForBalance2, _)) = get3 $ Map.toList utxos
        vh = fromJust $ Addr.toValidatorHash mustReferenceOutputV1ValidatorAddress
        lookups = TC.unspentOutputs utxos
        tx = TC.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 5)
          <> TC.mustSpendPubKeyOutput utxoRefForBalance1
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferenceOutputV1ValidatorAddress
    let
        addressMap = Map.singleton mustReferenceOutputV1ValidatorAddress scriptUtxos
        lookups = TC.unspentOutputs (Map.singleton utxoRef utxo <> scriptUtxos)
               <> TC.plutusV1OtherScript mustReferenceOutputV1Validator
               <> TC.unspentOutputs utxos
        tx = TC.mustReferenceOutput utxoRef
          <> TC.collectFromPlutusV1Script addressMap mustReferenceOutputV1Validator unitRedeemer
          <> TC.mustSpendPubKeyOutput utxoRefForBalance2
    mkTxConstraints @Any lookups tx >>= submitTxConfirmed

mustReferenceOutputTxV1ConTest :: Contract () EmptySchema ContractError ()
mustReferenceOutputTxV1ConTest = do
    let mkTx lookups constraints = either (error . show) id $ Tx.Constraints.mkTx @Any def lookups constraints

    utxos <- ownUtxos
    let ((utxoRef, utxo), (utxoRefForBalance1, _), (utxoRefForBalance2, _)) = get3 $ Map.toList utxos
        vh = fromJust $ Addr.toValidatorHash mustReferenceOutputV1ValidatorAddress
        lookups = Tx.Constraints.unspentOutputs utxos
        tx = Tx.Constraints.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 5)
          <> Tx.Constraints.mustSpendPubKeyOutput utxoRefForBalance1
          <> Tx.Constraints.mustUseOutputAsCollateral utxoRefForBalance1
    submitTxConfirmed $ mkTx lookups tx

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferenceOutputV1ValidatorAddress
    let
        scriptUtxo = fst . head . Map.toList $ scriptUtxos
        lookups = Tx.Constraints.unspentOutputs (Map.singleton utxoRef utxo <> scriptUtxos)
               <> Tx.Constraints.plutusV1OtherScript mustReferenceOutputV1Validator
               <> Tx.Constraints.unspentOutputs utxos
        tx = Tx.Constraints.mustReferenceOutput utxoRef
          <> Tx.Constraints.mustSpendScriptOutput scriptUtxo unitRedeemer
          <> Tx.Constraints.mustSpendPubKeyOutput utxoRefForBalance2
          <> Tx.Constraints.mustUseOutputAsCollateral utxoRefForBalance2
    submitTxConfirmed $ mkTx lookups tx

{-
Test case: mustReferenceOutput can be used on-chain to unlock funds in a PlutusV2 script
-}

{-# INLINABLE mkMustReferenceOutputV2Validator #-}
mkMustReferenceOutputV2Validator :: TxOutRef -> () -> PV2.ScriptContext -> Bool
mkMustReferenceOutputV2Validator txOutRef _ =
    TCV2.checkScriptContext @Void @Void (TC.mustReferenceOutput txOutRef)

{-# INLINABLE mustReferenceOutputV2Validator #-}
mustReferenceOutputV2Validator :: PV2.Validator
mustReferenceOutputV2Validator = PV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PV2.mkUntypedValidator mkMustReferenceOutputV2Validator

mustReferenceOutputV2ValidatorAddress :: Address
mustReferenceOutputV2ValidatorAddress =
    PV2.mkValidatorAddress mustReferenceOutputV2Validator

mustReferenceOutputV2ConTest :: Contract () EmptySchema ContractError ()
mustReferenceOutputV2ConTest = do

    utxos <- ownUtxos
    let ((utxoRef, utxo), (utxoRefForBalance1, _), (utxoRefForBalance2, _)) = get3 $ Map.toList utxos
        vh = fromJust $ Addr.toValidatorHash mustReferenceOutputV2ValidatorAddress
        lookups = TC.unspentOutputs utxos
        tx = TC.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 5)
          <> TC.mustSpendPubKeyOutput utxoRefForBalance1
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferenceOutputV2ValidatorAddress
    let
        addressMap = Map.singleton mustReferenceOutputV2ValidatorAddress scriptUtxos
        lookups = TC.unspentOutputs (Map.singleton utxoRef utxo <> scriptUtxos)
               <> TC.plutusV2OtherScript mustReferenceOutputV2Validator
               <> TC.unspentOutputs utxos
        tx = TC.mustReferenceOutput utxoRef
          <> TC.collectFromPlutusV2Script addressMap mustReferenceOutputV2Validator unitRedeemer
          <> TC.mustSpendPubKeyOutput utxoRefForBalance2
    mkTxConstraints @Any lookups tx >>= submitTxConfirmed

mustReferenceOutputTxV2ConTest :: Contract () EmptySchema ContractError ()
mustReferenceOutputTxV2ConTest = do
    let mkTx lookups constraints = either (error . show) id $ Tx.Constraints.mkTx @Any def lookups constraints

    utxos <- ownUtxos
    let ((utxoRef, utxo), (utxoRefForBalance1, _), (utxoRefForBalance2, _)) = get3 $ Map.toList utxos
        vh = fromJust $ Addr.toValidatorHash mustReferenceOutputV2ValidatorAddress
        lookups = Tx.Constraints.unspentOutputs utxos
        tx = Tx.Constraints.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 5)
          <> Tx.Constraints.mustSpendPubKeyOutput utxoRefForBalance1
          <> Tx.Constraints.mustUseOutputAsCollateral utxoRefForBalance1
    submitTxConfirmed $ mkTx lookups tx

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferenceOutputV2ValidatorAddress
    let
        scriptUtxo = fst . head . Map.toList $ scriptUtxos
        lookups = Tx.Constraints.unspentOutputs (Map.singleton utxoRef utxo <> scriptUtxos)
               <> Tx.Constraints.plutusV2OtherScript mustReferenceOutputV2Validator
               <> Tx.Constraints.unspentOutputs utxos
        tx = Tx.Constraints.mustReferenceOutput utxoRef
          <> Tx.Constraints.mustSpendScriptOutput scriptUtxo unitRedeemer
          <> Tx.Constraints.mustSpendPubKeyOutput utxoRefForBalance2
          <> Tx.Constraints.mustUseOutputAsCollateral utxoRefForBalance2
    submitTxConfirmed $ mkTx lookups tx

get3 :: [a] -> (a, a, a)
get3 (a:b:c:_) = (a, b, c)
get3 _         = error "Spec.Contract.TxConstraints.get3: not enough inputs"
