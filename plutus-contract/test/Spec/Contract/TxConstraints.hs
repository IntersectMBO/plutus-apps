{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE NumericUnderscores    #-}
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
import Ledger.Constraints.OffChain qualified as TC (plutusV2OtherScript, unspentOutputs)
import Ledger.Constraints.OffChain.V2 qualified as TC
import Ledger.Constraints.OnChain.V1 qualified as TCV1
import Ledger.Constraints.OnChain.V2 qualified as TCV2
import Ledger.Constraints.TxConstraints qualified as TC (mustPayToOtherScript, mustPayToPubKey,
                                                         mustReferencePubKeyOutput)
import Ledger.Scripts (unitRedeemer)
import Ledger.Tx (ciTxOutValue)
import Ledger.Tx.Constraints qualified as Tx.Constraints
import Plutus.Contract as Con
import Plutus.Contract.Request as Con (mkTxConstraints, ownFirstPaymentPubKeyHash, ownUtxos, submitTxConfirmed,
                                       submitUnbalancedTx, utxosAt)
import Plutus.Contract.Schema as Con (EmptySchema)
import Plutus.Contract.State qualified as State
import Plutus.Contract.Test (Shrinking (DoShrink, DontShrink), TracePredicate, assertAccumState, assertContractError,
                             assertDone, assertInstanceLog, assertNoFailedTransactions, assertResumableResult,
                             assertUserLog, assertValidatedTransactionCount, checkEmulatorFails, checkPredicate,
                             checkPredicateOptions, defaultCheckOptions, endpointAvailable, minLogLevel,
                             mockWalletPaymentPubKeyHash, not, valueAtAddress, w1, w2, w3, waitingForSlot,
                             walletFundsChange, (.&&.))
import Plutus.Contract.Types (ResumableResult (ResumableResult, _finalState), responses)
import Plutus.Contract.Types as Con (Contract, ContractError)
import Plutus.Contract.Util (loopM)
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.Script.Utils.V1.Address (mkValidatorAddress)
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V1.Typed.Scripts qualified as PV1
import Plutus.Script.Utils.V1.Typed.TypeUtils (Any)
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
    [ checkPredicate "mustReferencePubKeyOutput returns False on-chain when used for unlocking funds in a PlutusV1 script"
        (walletFundsChange w1 (Ada.adaValueOf (-5))
        .&&. valueAtAddress mustReferencePubKeyOutputV1ValidatorAddress (== Ada.adaValueOf 5)
        .&&. assertValidatedTransactionCount 2
        ) $ do
            void $ activateContract w1 mustReferencePubKeyOutputV1ConTest tag
            void $ Trace.waitNSlots 1

    , checkPredicate "mustReferencePubKeyOutput can be used on-chain to unlock funds in a PlutusV2 script"
        (walletFundsChange w1 (Ada.adaValueOf 0)
        .&&. valueAtAddress mustReferencePubKeyOutputV2ValidatorAddress (== Ada.adaValueOf 0)
        .&&. assertValidatedTransactionCount 3
        ) $ do
            void $ activateContract w1 mustReferencePubKeyOutputV2ConTest tag
            void $ Trace.waitNSlots 2

    -- Testing package plutus-tx-constraints

    , checkPredicate "Tx.Constraints.mustReferencePubKeyOutput returns False on-chain when used for unlocking funds in a PlutusV1 script"
        (walletFundsChange w1 (Ada.adaValueOf (-5))
        .&&. valueAtAddress mustReferencePubKeyOutputV1ValidatorAddress (== Ada.adaValueOf 5)
        .&&. assertValidatedTransactionCount 2
        ) $ do
            void $ activateContract w1 mustReferencePubKeyOutputTxV1ConTest tag
            void $ Trace.waitNSlots 1

    , checkPredicate "Tx.Constraints.mustReferencePubKeyOutput can be used on-chain to unlock funds in a PlutusV2 script"
        (walletFundsChange w1 (Ada.adaValueOf 0)
        .&&. valueAtAddress mustReferencePubKeyOutputV2ValidatorAddress (== Ada.adaValueOf 0)
        .&&. assertValidatedTransactionCount 3
        ) $ do
            void $ activateContract w1 mustReferencePubKeyOutputTxV2ConTest tag
            void $ Trace.waitNSlots 2
    ]

{-
Test case: mustReferencePubKeyOutput returns False on-chain when used for unlocking funds in a PlutusV1 script"
-}

{-# INLINABLE mkMustReferencePubKeyOutputV1Validator #-}
mkMustReferencePubKeyOutputV1Validator :: TxOutRef -> () -> PV1.ScriptContext -> Bool
mkMustReferencePubKeyOutputV1Validator txOutRef _ =
    TCV1.checkScriptContext @Void @Void (TC.mustReferencePubKeyOutput txOutRef)

{-# INLINABLE mustReferencePubKeyOutputV1Validator #-}
mustReferencePubKeyOutputV1Validator :: PV1.Validator
mustReferencePubKeyOutputV1Validator = PV1.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PV1.mkUntypedValidator mkMustReferencePubKeyOutputV1Validator

mustReferencePubKeyOutputV1ValidatorAddress :: Address
mustReferencePubKeyOutputV1ValidatorAddress =
    PV1.mkValidatorAddress mustReferencePubKeyOutputV1Validator

mustReferencePubKeyOutputV1ConTest :: Contract () EmptySchema ContractError ()
mustReferencePubKeyOutputV1ConTest = do
    -- Create a transaction which creates a least 2 utxos for the wallet
    -- We'll use one of those utxos for referencing
    -- We're supposing that the wallet contains at least 10ADA.
    let newUtxoValue = Ada.toValue 2_000_000
    pkh <- Con.ownFirstPaymentPubKeyHash
    let lookups = mempty
        tx = TC.mustPayToPubKey pkh newUtxoValue
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Locking some Ada in the script address
    (utxoRef, utxo) <-
          head
        . filter (\(_, txOut) -> txOut ^. ciTxOutValue == newUtxoValue)
        . Map.toList <$> ownUtxos
    let vh = fromJust $ Addr.toValidatorHash mustReferencePubKeyOutputV1ValidatorAddress
        lookups = mempty
        tx = TC.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 5)
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Trying to unlock the Ada in the script address
    let lookups = TC.unspentOutputs (Map.singleton utxoRef utxo)
        tx = TC.mustReferencePubKeyOutput utxoRef
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

mustReferencePubKeyOutputTxV1ConTest :: Contract () EmptySchema ContractError ()
mustReferencePubKeyOutputTxV1ConTest = do
    let mkTx lookups constraints = either (error . show) id $ Tx.Constraints.mkTx @Void def lookups constraints

    -- Create a transaction which creates a least 2 utxos for the wallet
    -- We'll use one of those utxos for referencing
    -- We're supposing that the wallet contains at least 10ADA.
    let newUtxoValue = Ada.toValue 2_000_000
    pkh <- Con.ownFirstPaymentPubKeyHash
    let lookups = mempty
        tx = TC.mustPayToPubKey pkh newUtxoValue
    void $ submitUnbalancedTx $ mkTx lookups tx

    -- Locking some Ada in the script address
    -- We find the utxo we created earlier
    (utxoRef, utxo) <-
          head
        . filter (\(_, txOut) -> txOut ^. ciTxOutValue == newUtxoValue)
        . Map.toList <$> ownUtxos
    let vh = fromJust $ Addr.toValidatorHash mustReferencePubKeyOutputV1ValidatorAddress
        lookups = mempty
        tx = Tx.Constraints.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 5)
    void $ submitUnbalancedTx $ mkTx lookups tx

    -- Trying to unlock the Ada in the script address
    let lookups = Tx.Constraints.unspentOutputs (Map.singleton utxoRef utxo)
        tx = Tx.Constraints.mustReferencePubKeyOutput utxoRef
    void $ submitUnbalancedTx $ mkTx lookups tx

{-
Test case: mustReferencePubKeyOutput can be used on-chain to unlock funds in a PlutusV2 script
-}

{-# INLINABLE mkMustReferencePubKeyOutputV2Validator #-}
mkMustReferencePubKeyOutputV2Validator :: TxOutRef -> () -> PV2.ScriptContext -> Bool
mkMustReferencePubKeyOutputV2Validator txOutRef _ =
    TCV2.checkScriptContext @Void @Void (TC.mustReferencePubKeyOutput txOutRef)

{-# INLINABLE mustReferencePubKeyOutputV2Validator #-}
mustReferencePubKeyOutputV2Validator :: PV2.Validator
mustReferencePubKeyOutputV2Validator = PV2.mkValidatorScript
    $$(PlutusTx.compile [|| wrap ||])
 where
     wrap = PV2.mkUntypedValidator mkMustReferencePubKeyOutputV2Validator

mustReferencePubKeyOutputV2ValidatorAddress :: Address
mustReferencePubKeyOutputV2ValidatorAddress =
    PV2.mkValidatorAddress mustReferencePubKeyOutputV2Validator

mustReferencePubKeyOutputV2ConTest :: Contract () EmptySchema ContractError ()
mustReferencePubKeyOutputV2ConTest = do
    -- Create a transaction which creates a least 2 utxos for the wallet
    -- We'll use one of those utxos for referencing
    -- We're supposing that the wallet contains at least 10ADA.
    let newUtxoValue = Ada.toValue 2_000_000
    pkh <- Con.ownFirstPaymentPubKeyHash
    let lookups = mempty
        tx = TC.mustPayToPubKey pkh newUtxoValue
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Locking some Ada in the script address
    -- We find the utxo we created earlier
    (utxoRef, utxo) <-
          head
        . filter (\(_, txOut) -> txOut ^. ciTxOutValue == newUtxoValue)
        . Map.toList <$> ownUtxos
    let vh = fromJust $ Addr.toValidatorHash mustReferencePubKeyOutputV2ValidatorAddress
        lookups = mempty
        tx = TC.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 5)
    mkTxConstraints @Void lookups tx >>= submitTxConfirmed

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferencePubKeyOutputV2ValidatorAddress
    let
        addressMap = Map.singleton mustReferencePubKeyOutputV2ValidatorAddress scriptUtxos
        lookups = TC.unspentOutputs (Map.singleton utxoRef utxo <> scriptUtxos)
               <> TC.plutusV2OtherScript mustReferencePubKeyOutputV2Validator
        tx = TC.mustReferencePubKeyOutput utxoRef
          <> TC.collectFromScript addressMap mustReferencePubKeyOutputV2Validator unitRedeemer
    mkTxConstraints @Any lookups tx >>= submitTxConfirmed

mustReferencePubKeyOutputTxV2ConTest :: Contract () EmptySchema ContractError ()
mustReferencePubKeyOutputTxV2ConTest = do
    let mkTx lookups constraints = either (error . show) id $ Tx.Constraints.mkTx @Any def lookups constraints

    -- Create a transaction which creates a least 2 utxos for the wallet
    -- We'll use one of those utxos for referencing
    -- We're supposing that the wallet contains at least 10ADA.
    let newUtxoValue = Ada.toValue 2_000_000
    pkh <- Con.ownFirstPaymentPubKeyHash
    let lookups = mempty
        tx = Tx.Constraints.mustPayToPubKey pkh newUtxoValue
    void $ submitUnbalancedTx $ mkTx lookups tx

    -- Locking some Ada in the script address
    -- We find the utxo we created earlier
    (utxoRef, utxo) <-
          head
        . filter (\(_, txOut) -> txOut ^. ciTxOutValue == newUtxoValue)
        . Map.toList <$> ownUtxos
    let vh = fromJust $ Addr.toValidatorHash mustReferencePubKeyOutputV2ValidatorAddress
        lookups = mempty
        tx = Tx.Constraints.mustPayToOtherScript vh (Datum $ PlutusTx.toBuiltinData utxoRef) (Ada.adaValueOf 5)
    void $ submitUnbalancedTx $ mkTx lookups tx

    -- Trying to unlock the Ada in the script address
    scriptUtxos <- utxosAt mustReferencePubKeyOutputV2ValidatorAddress
    let
        addressMap = Map.singleton mustReferencePubKeyOutputV2ValidatorAddress scriptUtxos
        lookups = Tx.Constraints.unspentOutputs (Map.singleton utxoRef utxo <> scriptUtxos)
               <> Tx.Constraints.plutusV2OtherScript mustReferencePubKeyOutputV2Validator
        tx = Tx.Constraints.mustReferencePubKeyOutput utxoRef
          <> TC.collectFromScript addressMap mustReferencePubKeyOutputV2Validator unitRedeemer
    void $ submitUnbalancedTx $ mkTx lookups tx
