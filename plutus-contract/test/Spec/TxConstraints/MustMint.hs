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

import Control.Lens ((&))
import Data.Default (def)
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (ScriptHashNotFound), plutusV1MintingPolicy,
                                                             typedValidatorLookups, unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustMintCurrency,
                                                                  mustMintCurrencyWithRedeemer, mustMintValue,
                                                                  mustMintValueWithRedeemer, mustPayToTheScript)
import Ledger.Test (asRedeemer, coinMintingPolicy, coinMintingPolicyCurrencySymbol, coinMintingPolicyHash)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (TokenName (TokenName))
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicate, checkPredicateOptions, defaultCheckOptions, w1,
                             walletFundsChange, (.&&.))
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (MintingPolicyHash (MintingPolicyHash), Redeemer)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError), ScriptHash (ScriptHash), unitRedeemer)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import Prelude hiding (not)
import Wallet (WalletAPIError (InsufficientFunds))

tests :: TestTree
tests =
    testGroup "MustMint"
        [ mustMintCurrencyWithRedeemerSuccessfulMint
        , mustMintCurrencyWithRedeemerSuccessfulBurn
        , mustMintCurrencyWithRedeemerBurnTooMuch
        , mustMintCurrencyWithRedeemerMissingPolicyLookup
        , mustMintCurrencyWithRedeemerPhase2Failure
        , mustMintCurrencySuccessfulMint
        , mustMintValueWithRedeemerSuccessfulMint
        , mustMintValueWithRedeemerSuccessfulBurn
        , mustMintValueSuccessfulMint
        ]

trace ::  Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

data UnitTest
instance Scripts.ValidatorTypes UnitTest

tknName :: TokenName
tknName = "A"

tknAmount :: Integer
tknAmount = 21_000_000

tknValue :: Value.Value
tknValue = tknValue' tknAmount

tknValue' :: Integer -> Value.Value
tknValue' = Value.singleton coinMintingPolicyCurrencySymbol tknName

-- | Valid Contract using a minting policy with mustMintCurrencyWithRedeemer onchain constraint to check that tokens are correctly minted with the other policy
mustMintCurrencyWithRedeemerContract :: Integer -> TokenName -> Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerContract mintAmount onChainTokenName = do
    let redeemer = asRedeemer $ MustMintCurrencyWithRedeemer coinMintingPolicyHash unitRedeemer onChainTokenName mintAmount
        lookups1 = Constraints.plutusV1MintingPolicy mustMintPolicy
                <> Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHash redeemer tknName 1
           <> Constraints.mustMintCurrencyWithRedeemer coinMintingPolicyHash unitRedeemer tknName mintAmount
    ledgerTx1 <- submitTxConstraintsWith @UnitTest def lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintCurrency onchain constraint to check that tokens are correctly minted with the other policy
mustMintCurrencyContract :: Contract () Empty ContractError ()
mustMintCurrencyContract = do
    let redeemer = asRedeemer $ MustMintCurrency coinMintingPolicyHash tknName tknAmount
        lookups1 = Constraints.plutusV1MintingPolicy mustMintPolicy
                <> Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHash redeemer tknName 1
           <> Constraints.mustMintCurrency coinMintingPolicyHash tknName tknAmount
    ledgerTx1 <- submitTxConstraintsWith @UnitTest def lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintValueWithRedeemer onchain constraint to check that tokens are correctly minted with the other policy
mustMintValueWithRedeemerContract :: Value.Value -> Contract () Empty ContractError ()
mustMintValueWithRedeemerContract mintValue = do
    let redeemer = asRedeemer $ MustMintValueWithRedeemer unitRedeemer mintValue
        lookups1 = Constraints.plutusV1MintingPolicy mustMintPolicy
                <> Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHash redeemer tknName 1
           <> Constraints.mustMintValueWithRedeemer unitRedeemer mintValue
    ledgerTx1 <- submitTxConstraintsWith @UnitTest def lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintValue onchain constraint to check that tokens are correctly minted with the other policy
mustMintValueContract :: Contract () Empty ContractError ()
mustMintValueContract = do
    let redeemer = asRedeemer $ MustMintValue tknValue
        lookups1 = Constraints.plutusV1MintingPolicy mustMintPolicy
                 <> Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHash redeemer tknName 1
           <> Constraints.mustMintValue tknValue
    ledgerTx1 <- submitTxConstraintsWith @UnitTest def lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer to mint tokens
mustMintCurrencyWithRedeemerSuccessfulMint :: TestTree
mustMintCurrencyWithRedeemerSuccessfulMint =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintCurrencyWithRedeemer"
    (assertValidatedTransactionCount 1)
    (void $ trace $ mustMintCurrencyWithRedeemerContract tknAmount tknName)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer to burn tokens
mustMintCurrencyWithRedeemerSuccessfulBurn :: TestTree
mustMintCurrencyWithRedeemerSuccessfulBurn =
    let tknBurnAmount = (-1000)
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValue <>)
    in checkPredicateOptions
       options
       "Successful token burn using mustMintCurrencyWithRedeemer"
       (walletFundsChange w1 (tknValue' tknBurnAmount <> Value.singleton mustMintPolicyCurrencySymbol tknName 1) -- including mustMintPolicyCurrencySymbol is a workaround, test only cares about tknBurnAmount -- Fixed by PLT-909
       .&&. assertValidatedTransactionCount 1)
       (void $ trace $ mustMintCurrencyWithRedeemerContract tknBurnAmount tknName)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer to burn more tokens than the wallet holds, asserts script evaluation error.
mustMintCurrencyWithRedeemerBurnTooMuch :: TestTree
mustMintCurrencyWithRedeemerBurnTooMuch =
    let tknBurnAmount = negate (tknAmount + 1)
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValue <>)
        contract = mustMintCurrencyWithRedeemerContract tknBurnAmount tknName
    in checkPredicateOptions
       options
       "Contract error when burning more than total amount of tokens in wallet balance"
       (assertContractError contract (Trace.walletInstanceTag w1) (\case WalletContractError (InsufficientFunds _) -> True; _ -> False) "failed to throw error"
       .&&. assertValidatedTransactionCount 0)
       (void $ trace contract)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer but with a contract that is missing lookup for the minting policy, asserts contract error.
mustMintCurrencyWithRedeemerMissingPolicyLookup :: TestTree
mustMintCurrencyWithRedeemerMissingPolicyLookup =
    let contract :: Contract () Empty ContractError () = do
            let tx1 = Constraints.mustMintCurrencyWithRedeemer coinMintingPolicyHash unitRedeemer tknName tknAmount
            ledgerTx1 <- submitTx def tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicateOptions
    defaultCheckOptions
    "Fail validation when minting policy is missing from lookup"
    (assertContractError
        contract
        (Trace.walletInstanceTag w1)
        (\case
            ConstraintResolutionContractError (Constraints.ScriptHashNotFound (ScriptHash sh)) -> MintingPolicyHash sh == coinMintingPolicyHash
            _ -> False)
        "failed to throw error"
    .&&. assertValidatedTransactionCount 0)
    (void $ trace contract)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer but with a token name mismatch, asserts script evaluation error.
mustMintCurrencyWithRedeemerPhase2Failure :: TestTree
mustMintCurrencyWithRedeemerPhase2Failure =
    checkPredicate
    "Phase 2 failure when policy mints with unexpected token name"
    (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L9":_) _) -> True; _ -> False }))
    (void $ trace $ mustMintCurrencyWithRedeemerContract tknAmount $ TokenName "WrongToken")

-- | Contract without the required minting policy lookup. Uses mustMintCurrencyWithRedeemer constraint.
mustMintCurrencyWithRedeemerMissingPolicyContract :: Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerMissingPolicyContract = do
    let lookups1 = Constraints.typedValidatorLookups $ mustMintCurrencyWithRedeemerTypedValidator tknName
        tx1 = Constraints.mustPayToTheScript () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith def lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash $ mustMintCurrencyWithRedeemerTypedValidator tknName)
    let lookups2 =
            Constraints.typedValidatorLookups (mustMintCurrencyWithRedeemerTypedValidator tknName) <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos () <>
            Constraints.mustMintCurrencyWithRedeemer coinMintingPolicyHash unitRedeemer tknName tknAmount
    ledgerTx2 <- submitTxConstraintsWith @UnitTest def lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

{-# INLINEABLE mustMintCurrencyWithRedeemerValidator #-}
mustMintCurrencyWithRedeemerValidator :: MintingPolicyHash -> Redeemer -> TokenName -> Integer -> () -> () -> Ledger.ScriptContext -> Bool
mustMintCurrencyWithRedeemerValidator mph r tn amt _ _ =
  Constraints.checkScriptContext @Void @Void (Constraints.mustMintCurrencyWithRedeemer mph r tn amt)

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
    (assertValidatedTransactionCount 1)
    (void $ trace mustMintCurrencyContract)

-- | Uses onchain and offchain constraint mustMintValueWithRedeemer to mint tokens
mustMintValueWithRedeemerSuccessfulMint :: TestTree
mustMintValueWithRedeemerSuccessfulMint =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintValueWithRedeemer"
    (assertValidatedTransactionCount 1)
    (void $ trace $ mustMintValueWithRedeemerContract tknValue)

-- | Uses onchain and offchain constraint mustMintValueWithRedeemer to burn tokens
mustMintValueWithRedeemerSuccessfulBurn :: TestTree
mustMintValueWithRedeemerSuccessfulBurn =
    let tknBurnValue = tknValue' (-1000)
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValue <>)
    in checkPredicateOptions
       options
       "Successful token burn using mustMintValueWithRedeemer"
       (walletFundsChange w1 (tknBurnValue <> Value.singleton mustMintPolicyCurrencySymbol tknName 1) -- including mustMintPolicyCurrencySymbol is a workaround, test only cares about tknBurnValue -- Fixed by PLT-909
       .&&. assertValidatedTransactionCount 1)
       (void $ trace $ mustMintValueWithRedeemerContract tknBurnValue)

-- | Uses onchain and offchain constraint mustMintValue to mint tokens
mustMintValueSuccessfulMint :: TestTree
mustMintValueSuccessfulMint =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintValue"
    (assertValidatedTransactionCount 1)
    (void $ trace mustMintValueContract)

{-# INLINEABLE mkMustMintPolicy #-}
mkMustMintPolicy :: ConstraintParams -> Ledger.ScriptContext -> Bool
mkMustMintPolicy t = case t of
    MustMintCurrencyWithRedeemer mph r tn i -> Constraints.checkScriptContext @() @() (Constraints.mustMintCurrencyWithRedeemer mph r tn i)
    MustMintCurrency mph tn i               -> Constraints.checkScriptContext @() @() (Constraints.mustMintCurrency mph tn i)
    MustMintValueWithRedeemer r v           -> Constraints.checkScriptContext @() @() (Constraints.mustMintValueWithRedeemer r v)
    MustMintValue v                         -> Constraints.checkScriptContext @() @() (Constraints.mustMintValue v)

mustMintPolicy :: Scripts.MintingPolicy
mustMintPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = Scripts.mkUntypedMintingPolicy mkMustMintPolicy

mustMintPolicyHash :: Ledger.MintingPolicyHash
mustMintPolicyHash = PSU.V1.mintingPolicyHash mustMintPolicy

mustMintPolicyCurrencySymbol :: Value.CurrencySymbol
mustMintPolicyCurrencySymbol = Value.mpsSymbol mustMintPolicyHash

data ConstraintParams = MustMintCurrencyWithRedeemer Ledger.MintingPolicyHash Redeemer TokenName Integer
                      | MustMintCurrency Ledger.MintingPolicyHash TokenName Integer
                      | MustMintValueWithRedeemer Redeemer Value.Value
                      | MustMintValue Value.Value
    deriving (Show)

PlutusTx.unstableMakeIsData ''ConstraintParams
