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

import Data.Function ((&))
import Data.Map qualified as Map
import Data.Void
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (MintingPolicyNotFound, OwnPubKeyMissing),
                                                             ownPaymentPubKeyHash, plutusV1MintingPolicy,
                                                             plutusV1TypedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustIncludeDatum,
                                                                  mustMintCurrencyWithRedeemer, mustPayToTheScript,
                                                                  mustSpendAtLeast)
import Ledger.Generators (someTokenValue)
import Ledger.Test (coinMintingPolicy)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (TokenName (TokenName), tokenName)
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicateOptions, defaultCheckOptions, w1, (.&&.))
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (BuiltinByteString, BuiltinData (BuiltinData), Datum (Datum), MintingPolicyHash,
                             Redeemer (Redeemer), ScriptContext, Validator, ValidatorHash)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), ScriptHash)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Numeric qualified as Value
import PlutusTx.Prelude qualified as P
import Prelude hiding (not)

tests :: TestTree
tests =
    testGroup "MustMint"
        [ mustMintCurrencyWithRedeemer
        , missingPolicy
        --, mustMintCurrency
        --, mustMintValueWithRedeemer
        --, mustMintValue
        ]

trace ::  Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

data UnitTest
instance Scripts.ValidatorTypes UnitTest

redeemer :: Redeemer
redeemer = Redeemer $ PlutusTx.toBuiltinData ()

tknName :: TokenName
tknName = "TokenB"

tknAmount :: Integer
tknAmount = 21_000_000

coinMintingPolicyHash :: MintingPolicyHash
coinMintingPolicyHash = PSU.V1.mintingPolicyHash coinMintingPolicy

-- MustMintCurrencyWithRedeemer

mustMintCurrencyWithRedeemer :: TestTree
mustMintCurrencyWithRedeemer =
    let ee = Value.singleton (PSU.V1.scriptCurrencySymbol coinMintingPolicy) tknName 1
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (Value.scale tknAmount ee <>)
    in  checkPredicateOptions
            options
            "Successful spend of tokens using mustMintCurrencyWithRedeemer"
            (assertValidatedTransactionCount 2)
            (void $ trace mustMintCurrencyWithRedeemerContract)

missingPolicy :: TestTree
missingPolicy =
    let ee = Value.singleton (PSU.V1.scriptCurrencySymbol coinMintingPolicy) tknName 1
        options = defaultCheckOptions
            & changeInitialWalletValue w1 (Value.scale tknAmount ee <>)
    in  checkPredicateOptions
            options
            "Fail validation when minting policy is missing from lookup"
            (assertContractError missingPolicyContract (Trace.walletInstanceTag w1) (\case { ConstraintResolutionContractError (Constraints.MintingPolicyNotFound mph) -> mph == coinMintingPolicyHash; _ -> False }) "failed to throw error"
            .&&. assertValidatedTransactionCount 1)
            (void $ trace missingPolicyContract)

mustMintCurrencyWithRedeemerContract :: Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerContract = do
    let lookups1 = Constraints.plutusV1TypedValidatorLookups mustMintCurrencyWithRedeemerTypedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustMintCurrencyWithRedeemerTypedValidator)
    let lookups2 =
            Constraints.plutusV1TypedValidatorLookups mustMintCurrencyWithRedeemerTypedValidator <>
            Constraints.plutusV1MintingPolicy coinMintingPolicy <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos () <>
            Constraints.mustMintCurrencyWithRedeemer coinMintingPolicyHash redeemer tknName tknAmount
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

missingPolicyContract :: Contract () Empty ContractError ()
missingPolicyContract = do
    let lookups1 = Constraints.plutusV1TypedValidatorLookups mustMintCurrencyWithRedeemerTypedValidator
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash mustMintCurrencyWithRedeemerTypedValidator)
    let lookups2 =
            Constraints.plutusV1TypedValidatorLookups mustMintCurrencyWithRedeemerTypedValidator <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos () <>
            Constraints.mustMintCurrencyWithRedeemer coinMintingPolicyHash redeemer tknName tknAmount
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

{-# INLINEABLE mustMintCurrencyWithRedeemerValidator #-}
mustMintCurrencyWithRedeemerValidator :: MintingPolicyHash -> Redeemer -> TokenName -> Integer -> () -> () -> Ledger.ScriptContext -> Bool
mustMintCurrencyWithRedeemerValidator mph r tn amt _ _ ctx = Constraints.checkScriptContext @Void @Void (Constraints.mustMintCurrencyWithRedeemer mph r tn amt) ctx

mustMintCurrencyWithRedeemerTypedValidator :: Scripts.TypedValidator UnitTest
mustMintCurrencyWithRedeemerTypedValidator = Scripts.mkTypedValidator @UnitTest
    ($$(PlutusTx.compile [||mustMintCurrencyWithRedeemerValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode coinMintingPolicyHash
        `PlutusTx.applyCode` PlutusTx.liftCode redeemer
        `PlutusTx.applyCode` PlutusTx.liftCode tknName
        `PlutusTx.applyCode` PlutusTx.liftCode tknAmount)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator
