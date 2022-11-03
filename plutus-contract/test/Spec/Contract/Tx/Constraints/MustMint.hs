{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustMint(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Control.Lens (_Just, has, (&))
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as TC
import Ledger.Constraints.OffChain qualified as Constraints (MkTxError (ScriptHashNotFound), plutusV1MintingPolicy,
                                                             plutusV2MintingPolicy, typedValidatorLookups,
                                                             unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.OnChain.V2 qualified as TCV2
import Ledger.Constraints.TxConstraints qualified as Constraints
import Ledger.Scripts (ScriptHash (ScriptHash), unitRedeemer)
import Ledger.Test (asRedeemer, coinMintingPolicy, coinMintingPolicyCurrencySymbol, coinMintingPolicyCurrencySymbolV2,
                    coinMintingPolicyHash, coinMintingPolicyHashV2, coinMintingPolicyV2)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (TokenName (TokenName))
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertFailedTransaction, assertValidatedTransactionCount,
                             changeInitialWalletValue, checkPredicate, checkPredicateOptions, defaultCheckOptions, w1,
                             walletFundsChange, (.&&.))
import Plutus.Script.Utils.Typed (Any)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PV2
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies qualified as MPS2
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Address, MintingPolicyHash (MintingPolicyHash), Redeemer, TxOutRef)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError))
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
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
        , mustMintWithReferenceV1Failure
        , mustMintWithReferencePhase2Failure
        , mustMintWithReferenceSuccessful
        ]

trace ::  Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

data UnitTest
instance Scripts.ValidatorTypes UnitTest

nonExistentTxoRef :: TxOutRef
nonExistentTxoRef = Tx.TxOutRef "abcd" 123

tknName :: TokenName
tknName = "A"

tknAmount :: Integer
tknAmount = 21_000_000

tknValueV1 :: Value.Value
tknValueV1 = tknValueV1' tknAmount

tknValueV1' :: Integer -> Value.Value
tknValueV1' = Value.singleton coinMintingPolicyCurrencySymbol tknName

tknValueV2 :: Value.Value
tknValueV2 = tknValueV2' tknAmount

tknValueV2' :: Integer -> Value.Value
tknValueV2' = Value.singleton coinMintingPolicyCurrencySymbolV2 tknName

-- | Valid Contract using a minting policy with mustMintCurrencyWithRedeemer onchain constraint to check that tokens are correctly minted with the other policy
mustMintCurrencyWithRedeemerContract :: Integer -> TokenName -> Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerContract mintAmount onChainTokenName = do
    let redeemer = asRedeemer $ MustMintCurrencyWithRedeemer coinMintingPolicyHash unitRedeemer onChainTokenName mintAmount
        lookups1 = Constraints.plutusV1MintingPolicy mustMintPolicy
                <> Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHash redeemer tknName 1
           <> Constraints.mustMintCurrencyWithRedeemer coinMintingPolicyHash unitRedeemer tknName mintAmount
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintCurrency onchain constraint to check that tokens are correctly minted with the other policy
mustMintCurrencyContract :: Contract () Empty ContractError ()
mustMintCurrencyContract = do
    let redeemer = asRedeemer $ MustMintCurrency coinMintingPolicyHash tknName tknAmount
        lookups1 = Constraints.plutusV1MintingPolicy mustMintPolicy
                <> Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHash redeemer tknName 1
           <> Constraints.mustMintCurrency coinMintingPolicyHash tknName tknAmount
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

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

mustMintValueWithReferenceContract :: Bool -> Contract () Empty ContractError ()
mustMintValueWithReferenceContract failPhase2 = do
    utxos <- ownUtxos
    myAddr <- Con.ownAddress
    let (utxoRef, utxo) = head $ drop 5 $ Map.toList utxos
        MintingPolicyHash mph = coinMintingPolicyHashV2
        lookups0 = Constraints.plutusV2MintingPolicy coinMintingPolicyV2
        tx0 = Constraints.mustPayToAddressWithReferenceScript
                myAddr
                (ScriptHash mph)
                Nothing
                (Ada.adaValueOf 35)
    ledgerTx0 <- submitTxConstraintsWith @UnitTest lookups0 tx0
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx0

    utxos' <- ownUtxos
    let refScriptUtxo = head . Map.keys . Map.filter (has $ Tx.decoratedTxOutReferenceScript. _Just) $ utxos'
        redeemerRefUtxo = if failPhase2 then nonExistentTxoRef else refScriptUtxo
        redeemer = asRedeemer $ MustMintValueWithReference redeemerRefUtxo tknValueV2
        lookups1 = Constraints.unspentOutputs (Map.singleton utxoRef utxo <> utxos')
                <> Constraints.plutusV2MintingPolicy mustMintPolicyV2
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHashV2 redeemer tknName 1
           <> Constraints.mustMintValueWithReference refScriptUtxo tknValueV2
    ledgerTx1 <- submitTxConstraintsWith @Any lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

mustMintValueWithReferenceContractV1Failure  :: Contract () Empty ContractError ()
mustMintValueWithReferenceContractV1Failure = do
    utxos <- ownUtxos
    myAddr <- Con.ownAddress
    let (utxoRef, utxo) = head $ drop 5 $ Map.toList utxos
        MintingPolicyHash mph = coinMintingPolicyHash
        lookups0 = Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx0 = Constraints.mustPayToAddressWithReferenceScript
                myAddr
                (ScriptHash mph)
                Nothing
                (Ada.adaValueOf 30)
    ledgerTx0 <- submitTxConstraintsWith @UnitTest lookups0 tx0
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx0

    utxos' <- ownUtxos
    let
        refScriptUtxo = head . Map.keys . Map.filter (has $ Tx.decoratedTxOutReferenceScript . _Just) $ utxos'
        lookups1 = Constraints.unspentOutputs (Map.singleton utxoRef utxo <> utxos')
        tx1 = Constraints.mustMintCurrencyWithReference refScriptUtxo coinMintingPolicyHash tknName tknAmount
    ledgerTx1 <- submitTxConstraintsWith @Any lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintValueWithRedeemer onchain constraint to check that tokens are correctly minted with the other policy
mustMintValueWithRedeemerContract :: Value.Value -> Contract () Empty ContractError ()
mustMintValueWithRedeemerContract mintValue = do
    let redeemer = asRedeemer $ MustMintValueWithRedeemer unitRedeemer mintValue
        lookups1 = Constraints.plutusV1MintingPolicy mustMintPolicy
                <> Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHash redeemer tknName 1
           <> Constraints.mustMintValueWithRedeemer unitRedeemer mintValue
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintValue onchain constraint to check that tokens are correctly minted with the other policy
mustMintValueContract :: Contract () Empty ContractError ()
mustMintValueContract = do
    let redeemer = asRedeemer $ MustMintValue tknValueV1
        lookups1 = Constraints.plutusV1MintingPolicy mustMintPolicy
                 <> Constraints.plutusV1MintingPolicy coinMintingPolicy
        tx1 = Constraints.mustMintCurrencyWithRedeemer mustMintPolicyHash redeemer tknName 1
           <> Constraints.mustMintValue tknValueV1
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
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
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValueV1 <>)
    in checkPredicateOptions
       options
       "Successful token burn using mustMintCurrencyWithRedeemer"
       (walletFundsChange w1 (tknValueV1' tknBurnAmount <> Value.singleton mustMintPolicyCurrencySymbol tknName 1) -- including mustMintPolicyCurrencySymbol is a workaround, test only cares about tknBurnAmount -- Fixed by PLT-909
       .&&. assertValidatedTransactionCount 1)
       (void $ trace $ mustMintCurrencyWithRedeemerContract tknBurnAmount tknName)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer to burn more tokens than the wallet holds, asserts script evaluation error.
mustMintCurrencyWithRedeemerBurnTooMuch :: TestTree
mustMintCurrencyWithRedeemerBurnTooMuch =
    let tknBurnAmount = negate (tknAmount + 1)
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValueV1 <>)
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
            ledgerTx1 <- submitTx tx1
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
    (void $ trace $ mustMintValueWithRedeemerContract tknValueV1)

-- | Uses onchain and offchain constraint mustMintValueWithRedeemer to burn tokens
mustMintValueWithRedeemerSuccessfulBurn :: TestTree
mustMintValueWithRedeemerSuccessfulBurn =
    let tknBurnValue = tknValueV1' (-1000)
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValueV1 <>)
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

mustMintWithReferenceV1Failure :: TestTree
mustMintWithReferenceV1Failure =
    checkPredicateOptions
    defaultCheckOptions
    "MustMintValue with reference fails because v1 is not supported"
    (assertFailedTransaction (\_ err -> case err of {Ledger.CardanoLedgerValidationError msg -> Text.isPrefixOf "ReferenceInputsNotSupported" msg; _ -> False }))
    (void $ trace mustMintValueWithReferenceContractV1Failure)

mustMintWithReferencePhase2Failure :: TestTree
mustMintWithReferencePhase2Failure =
    checkPredicateOptions
    defaultCheckOptions
    "MustMintValue with reference fails phase 2 validation error"
    (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L9":_) _) -> True; _ -> False }))
    (void $ trace $ mustMintValueWithReferenceContract True)

mustMintWithReferenceSuccessful :: TestTree
mustMintWithReferenceSuccessful =
    checkPredicateOptions
    defaultCheckOptions
    "Successful mustMintValue with reference"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustMintValueWithReferenceContract False)

{-# INLINEABLE mkMustMintPolicy #-}
mkMustMintPolicy :: ConstraintParams -> Ledger.ScriptContext -> Bool
mkMustMintPolicy t = case t of
    MustMintCurrencyWithRedeemer mph r tn i -> Constraints.checkScriptContext @() @() (Constraints.mustMintCurrencyWithRedeemer mph r tn i)
    MustMintCurrency mph tn i               -> Constraints.checkScriptContext @() @() (Constraints.mustMintCurrency mph tn i)
    MustMintValueWithRedeemer r v           -> Constraints.checkScriptContext @() @() (Constraints.mustMintValueWithRedeemer r v)
    MustMintValue v                         -> Constraints.checkScriptContext @() @() (Constraints.mustMintValue v)
    MustMintCurrencyWithReference ref mph tn i  -> Constraints.checkScriptContext @() @() (Constraints.mustMintCurrencyWithReference ref mph tn i)
    MustMintValueWithReference ref v                         -> Constraints.checkScriptContext @() @() (Constraints.mustMintValueWithReference ref v)
    MustMintValueWithRedeemerAndReference r mref v           -> Constraints.checkScriptContext @() @() (Constraints.mustMintValueWithRedeemerAndReference r mref v)
    MustMintCurrencyWithRedeemerAndReference mref mph r tn i -> Constraints.checkScriptContext @() @() (Constraints.mustMintCurrencyWithRedeemerAndReference mref mph r tn i)

{-# INLINEABLE mkMustMintPolicyV2 #-}
mkMustMintPolicyV2 :: ConstraintParams -> PV2.ScriptContext -> Bool
mkMustMintPolicyV2 t = case t of
    MustMintCurrencyWithRedeemer mph r tn i -> TCV2.checkScriptContext @() @() (Constraints.mustMintCurrencyWithRedeemer mph r tn i)
    MustMintCurrency mph tn i               -> TCV2.checkScriptContext @() @() (Constraints.mustMintCurrency mph tn i)
    MustMintValueWithRedeemer r v           -> TCV2.checkScriptContext @() @() (Constraints.mustMintValueWithRedeemer r v)
    MustMintValue v                         -> TCV2.checkScriptContext @() @() (Constraints.mustMintValue v)
    MustMintCurrencyWithReference ref mph tn i  -> TCV2.checkScriptContext @() @() (Constraints.mustMintCurrencyWithReference ref mph tn i)
    MustMintValueWithReference ref v                         -> TCV2.checkScriptContext @() @() (Constraints.mustMintValueWithReference ref v)
    MustMintValueWithRedeemerAndReference r mref v           -> TCV2.checkScriptContext @() @() (Constraints.mustMintValueWithRedeemerAndReference r mref v)
    MustMintCurrencyWithRedeemerAndReference mref mph r tn i -> TCV2.checkScriptContext @() @() (Constraints.mustMintCurrencyWithRedeemerAndReference mref mph r tn i)


mustMintPolicy :: Scripts.MintingPolicy
mustMintPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = Scripts.mkUntypedMintingPolicy mkMustMintPolicy

mustMintPolicyV2 :: Scripts.MintingPolicy
mustMintPolicyV2 = PV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = MPS2.mkUntypedMintingPolicy mkMustMintPolicyV2

mustMintPolicyHash :: Ledger.MintingPolicyHash
mustMintPolicyHash = PSU.V1.mintingPolicyHash mustMintPolicy

mustMintPolicyHashV2 :: Ledger.MintingPolicyHash
mustMintPolicyHashV2 = PSU.V2.mintingPolicyHash mustMintPolicyV2

mustMintPolicyCurrencySymbol :: Value.CurrencySymbol
mustMintPolicyCurrencySymbol = Value.mpsSymbol mustMintPolicyHash

data ConstraintParams = MustMintCurrencyWithRedeemer Ledger.MintingPolicyHash Redeemer TokenName Integer
                      | MustMintCurrency Ledger.MintingPolicyHash TokenName Integer
                      | MustMintValueWithRedeemer Redeemer Value.Value
                      | MustMintValue Value.Value
                      | MustMintCurrencyWithReference TxOutRef Ledger.MintingPolicyHash TokenName Integer
                      | MustMintValueWithReference TxOutRef Value.Value
                      | MustMintValueWithRedeemerAndReference Redeemer (Maybe TxOutRef) Value.Value
                      | MustMintCurrencyWithRedeemerAndReference (Maybe TxOutRef) Ledger.MintingPolicyHash Redeemer TokenName Integer
    deriving (Show)

PlutusTx.unstableMakeIsData ''ConstraintParams
