{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustMint(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Data.Void
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (ScriptHashNotFound), plutusV1MintingPolicy,
                                                             typedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustMintCurrency,
                                                                  mustMintCurrencyWithRedeemer, mustMintValue,
                                                                  mustMintValueWithRedeemer, mustPayToTheScript)
import Ledger.Test (coinMintingPolicy, coinMintingPolicyCurrencySymbol, coinMintingPolicyHash)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (CurrencySymbol (CurrencySymbol), TokenName (TokenName))
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             checkPredicateOptions, defaultCheckOptions, w1, (.&&.))
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (MintingPolicyHash, Redeemer, ToData (toBuiltinData),
                             UnsafeFromData (unsafeFromBuiltinData))
import Plutus.V1.Ledger.Scripts (MintingPolicyHash (MintingPolicyHash), ScriptError (EvaluationError),
                                 ScriptHash (ScriptHash), unitRedeemer)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import Prelude hiding (not)

tests :: TestTree
tests =
    testGroup "MustMint"
        [ mustMintCurrencyWithRedeemerSuccessfulMint
        , mustMintCurrencyWithRedeemerMissingPolicyLookup
        , mustMintCurrencyWithRedeemerPhase2Failure
        , mustMintCurrencySuccessfulMint
        , mustMintValueWithRedeemerSuccessfulMint
        , mustMintValueSuccessfulMint
        -- todo: burn
        ]

trace ::  Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

data UnitTest
instance Scripts.ValidatorTypes UnitTest

tknName :: TokenName
tknName = "TokenB"

tknAmount :: Integer
tknAmount = 21_000_000

tknValue :: Ledger.Value
tknValue = Value.singleton coinMintingPolicyCurrencySymbol tknName tknAmount

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer to mint tokens
mustMintCurrencyWithRedeemerSuccessfulMint :: TestTree
mustMintCurrencyWithRedeemerSuccessfulMint =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintCurrencyWithRedeemer"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustMintCurrencyWithRedeemerContract tknName)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer but with a contract that is missing lookup for the minting policy, asserts contract error.
mustMintCurrencyWithRedeemerMissingPolicyLookup :: TestTree
mustMintCurrencyWithRedeemerMissingPolicyLookup =
    checkPredicateOptions
    defaultCheckOptions
    "Fail validation when minting policy is missing from lookup"
    (assertContractError
        mustMintCurrencyWithRedeemerMissingPolicyContract
        (Trace.walletInstanceTag w1)
        (\case
            ConstraintResolutionContractError (Constraints.ScriptHashNotFound (ScriptHash sh)) -> MintingPolicyHash sh == coinMintingPolicyHash
            _ -> False)
        "failed to throw error"
    .&&. assertValidatedTransactionCount 1)
    (void $ trace mustMintCurrencyWithRedeemerMissingPolicyContract)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer but with a token name mismatch, asserts script evaluation error.
mustMintCurrencyWithRedeemerPhase2Failure :: TestTree
mustMintCurrencyWithRedeemerPhase2Failure =
    checkPredicateOptions
    defaultCheckOptions
    "Fail validation when minting policy is missing from lookup"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L9":_) _) -> True; _ -> False }))
    (void $ trace $ mustMintCurrencyWithRedeemerContract $ TokenName "WrongToken")

-- | Valid Contract containing all required lookups. Uses mustMintCurrencyWithRedeemer constraint.
mustMintCurrencyWithRedeemerContract :: TokenName -> Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerContract onChainTokenName = do
    let onChainTypedValidator = mustMintCurrencyWithRedeemerTypedValidator onChainTokenName
        lookups1 = Constraints.typedValidatorLookups onChainTypedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash onChainTypedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups onChainTypedValidator <>
            Constraints.plutusV1MintingPolicy coinMintingPolicy <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos () <>
            Constraints.mustMintCurrencyWithRedeemer coinMintingPolicyHash unitRedeemer tknName tknAmount
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

-- | Contract without the required minting policy lookup. Uses mustMintCurrencyWithRedeemer constraint.
mustMintCurrencyWithRedeemerMissingPolicyContract :: Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerMissingPolicyContract = do
    let lookups1 = Constraints.typedValidatorLookups $ mustMintCurrencyWithRedeemerTypedValidator tknName
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash $ mustMintCurrencyWithRedeemerTypedValidator tknName)
    let lookups2 =
            Constraints.typedValidatorLookups (mustMintCurrencyWithRedeemerTypedValidator tknName) <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos () <>
            Constraints.mustMintCurrencyWithRedeemer coinMintingPolicyHash unitRedeemer tknName tknAmount
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

{-# INLINEABLE mustMintCurrencyWithRedeemerValidator #-}
mustMintCurrencyWithRedeemerValidator :: MintingPolicyHash -> Redeemer -> TokenName -> Integer -> () -> () -> Ledger.ScriptContext -> Bool
mustMintCurrencyWithRedeemerValidator mph r tn amt _ _ ctx = Constraints.checkScriptContext @Void @Void (Constraints.mustMintCurrencyWithRedeemer mph r tn amt) ctx

mustMintCurrencyWithRedeemerTypedValidator :: TokenName -> Scripts.TypedValidator UnitTest
mustMintCurrencyWithRedeemerTypedValidator tn = Scripts.mkTypedValidator @UnitTest
    ($$(PlutusTx.compile [||mustMintCurrencyWithRedeemerValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode coinMintingPolicyHash
        `PlutusTx.applyCode` PlutusTx.liftCode unitRedeemer
        `PlutusTx.applyCode` PlutusTx.liftCode tn
        `PlutusTx.applyCode` PlutusTx.liftCode tknAmount)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- | Uses onchain and offchain constraint mustMintCurrency to mint tokens
mustMintCurrencySuccessfulMint :: TestTree
mustMintCurrencySuccessfulMint =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintCurrency"
    (assertValidatedTransactionCount 2)
    (void $ trace mustMintCurrencyContract)

-- | Valid Contract containing all required lookups. Uses mustMintCurrency constraint.
mustMintCurrencyContract :: Contract () Empty ContractError ()
mustMintCurrencyContract = do
    let lookups1 = Constraints.typedValidatorLookups mustMintCurrencyTypedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustMintCurrencyTypedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups mustMintCurrencyTypedValidator <>
            Constraints.plutusV1MintingPolicy coinMintingPolicy <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos () <>
            Constraints.mustMintCurrency coinMintingPolicyHash tknName tknAmount
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

{-# INLINEABLE mustMintCurrencyValidator #-}
mustMintCurrencyValidator :: MintingPolicyHash -> TokenName -> Integer -> () -> () -> Ledger.ScriptContext -> Bool
mustMintCurrencyValidator mph tn amt _ _ ctx = Constraints.checkScriptContext @Void @Void (Constraints.mustMintCurrency mph tn amt) ctx

mustMintCurrencyTypedValidator :: Scripts.TypedValidator UnitTest
mustMintCurrencyTypedValidator = Scripts.mkTypedValidator @UnitTest
    ($$(PlutusTx.compile [||mustMintCurrencyValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode coinMintingPolicyHash
        `PlutusTx.applyCode` PlutusTx.liftCode tknName
        `PlutusTx.applyCode` PlutusTx.liftCode tknAmount)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- | Uses onchain and offchain constraint mustMintValueWithRedeemer to mint tokens
mustMintValueWithRedeemerSuccessfulMint :: TestTree
mustMintValueWithRedeemerSuccessfulMint =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintValueWithRedeemer"
    (assertValidatedTransactionCount 2)
    (void $ trace mustMintValueWithRedeemerContract)

-- | Valid Contract containing all required lookups. Uses mustMintValueWithRedeemer constraint.
mustMintValueWithRedeemerContract :: Contract () Empty ContractError ()
mustMintValueWithRedeemerContract = do
    let lookups1 = Constraints.typedValidatorLookups mustMintValueWithRedeemerTypedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustMintValueWithRedeemerTypedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups mustMintValueWithRedeemerTypedValidator <>
            Constraints.plutusV1MintingPolicy coinMintingPolicy <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos () <>
            Constraints.mustMintValueWithRedeemer unitRedeemer tknValue
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

{-# INLINEABLE mustMintValueWithRedeemerValidator #-}
mustMintValueWithRedeemerValidator :: Redeemer -> Ledger.Value -> () -> () -> Ledger.ScriptContext -> Bool
mustMintValueWithRedeemerValidator r v _ _ ctx = Constraints.checkScriptContext @Void @Void (Constraints.mustMintValueWithRedeemer r v) ctx

mustMintValueWithRedeemerTypedValidator :: Scripts.TypedValidator UnitTest
mustMintValueWithRedeemerTypedValidator = Scripts.mkTypedValidator @UnitTest
    ($$(PlutusTx.compile [||mustMintValueWithRedeemerValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode unitRedeemer
        `PlutusTx.applyCode` PlutusTx.liftCode tknValue)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- | Uses onchain and offchain constraint mustMintValue to mint tokens
mustMintValueSuccessfulMint :: TestTree
mustMintValueSuccessfulMint =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintValue"
    (assertValidatedTransactionCount 2)
    (void $ trace mustMintValueContract)

-- | Valid Contract containing all required lookups. Uses mustMintValue constraint.
mustMintValueContract :: Contract () Empty ContractError ()
mustMintValueContract = do
    let lookups1 = Constraints.typedValidatorLookups mustMintValueWithRedeemerTypedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustMintValueTypedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups mustMintValueTypedValidator <>
            Constraints.plutusV1MintingPolicy coinMintingPolicy <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos () <>
            Constraints.mustMintValue tknValue
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

{-# INLINEABLE mustMintValueValidator #-}
mustMintValueValidator :: Ledger.Value -> () -> () -> Ledger.ScriptContext -> Bool
mustMintValueValidator v _ _ ctx = Constraints.checkScriptContext @Void @Void (Constraints.mustMintValue v) ctx

mustMintValueTypedValidator :: Scripts.TypedValidator UnitTest
mustMintValueTypedValidator = Scripts.mkTypedValidator @UnitTest
    ($$(PlutusTx.compile [||mustMintValueValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode tknValue)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator
