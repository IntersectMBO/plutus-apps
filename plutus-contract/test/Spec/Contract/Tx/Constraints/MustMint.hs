{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustMint(tests) where

import Control.Monad (void)
import Spec.Contract.Error (cardanoLedgerErrorContaining, insufficientFundsError)
import Test.Tasty (TestTree, testGroup)

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Internal.Node.Params qualified as Params
import Control.Lens (_Just, has, (&), (??))
import Data.Map qualified as Map
import Data.Void (Void)
import Ledger qualified
import Ledger.Scripts (ScriptHash (ScriptHash), unitRedeemer)
import Ledger.Test (asRedeemer, coinMintingPolicy, coinMintingPolicyHash, coinMintingPolicyId)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (fromCardanoAssetName, fromCardanoValue)
import Ledger.Tx.Constraints qualified as TC
import Ledger.Tx.Constraints.OffChain qualified as Constraints (MkTxError (ScriptHashNotFound), mintingPolicy,
                                                                typedValidatorLookups, unspentOutputs)
import Ledger.Tx.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Tx.Constraints.OnChain.V2 qualified as TCV2
import Ledger.Tx.Constraints.TxConstraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI (assetIdValue)
import Plutus.Contract as Con
import Plutus.Contract.Test (assertContractError, assertEvaluationError, assertFailedTransaction,
                             assertValidatedTransactionCount, changeInitialWalletValue, checkPredicate,
                             checkPredicateOptions, defaultCheckOptions, w1, walletFundsAssetClassChange, (.&&.))
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Typed qualified as Typed
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.Script.Utils.V2.Scripts qualified as PSU.V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PV2
import Plutus.Script.Utils.Value (TokenName (TokenName))
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (Address, MintingPolicyHash (MintingPolicyHash), Redeemer, TxOutRef)
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx qualified
import Prelude hiding (not)

tests :: TestTree
tests = testGroup "MustMint" [v1Tests, v2Tests]

v1Tests :: TestTree
v1Tests = testGroup "Plutus V1" $
   [ v1FeaturesTests
   , v2FeaturesNotAvailableTests
   ] ?? Ledger.PlutusV1


v2Tests :: TestTree
v2Tests = testGroup "Plutus V2 features" $
     [ v1FeaturesTests
     ] ?? Ledger.PlutusV2

v1FeaturesTests :: Ledger.Language -> TestTree
v1FeaturesTests lang =
    testGroup "Plutus V1 features" $
        [ mustMintCurrencyWithRedeemerSuccessfulMint
        , mustMintCurrencyWithRedeemerSuccessfulBurn
        , mustMintCurrencyWithRedeemerBurnTooMuch
        , mustMintCurrencyWithRedeemerMissingPolicyLookup
        , mustMintCurrencyWithRedeemerPhase2Failure
        , mustMintCurrencySuccessfulMint
        , mustMintValueWithRedeemerSuccessfulMint
        , mustMintValueWithRedeemerSuccessfulBurn
        , mustMintValueSuccessfulMint
        ] ?? lang

v2FeaturesNotAvailableTests :: Ledger.Language -> TestTree
v2FeaturesNotAvailableTests lang = testGroup "Plutus V2 features not available in V1" $
    [ mustMintWithReferenceV1Failure
    ] ?? lang

v2FeaturesTests :: Ledger.Language -> TestTree
v2FeaturesTests lang = testGroup "Plutus V2 features" $
    [ mustMintWithReferenceSuccessful
    , mustMintWithReferencePhase2Failure
    ] ?? lang

trace ::  Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void Trace.nextSlot

data UnitTest
instance Scripts.ValidatorTypes UnitTest

nonExistentTxoRef :: TxOutRef
nonExistentTxoRef = Tx.TxOutRef "abcd" 123

assetName :: C.AssetName
assetName = "A"

tknName :: TokenName
tknName = fromCardanoAssetName assetName

tknAmount :: Integer
tknAmount = 21_000_000

tknValue :: Ledger.Language -> C.Value
tknValue = flip tknValue' tknAmount

tknValue' :: Ledger.Language -> Integer -> C.Value
tknValue' = assetIdValue . tknAssetClass

tknAssetClass :: Ledger.Language -> C.AssetId
tknAssetClass lang = C.AssetId (coinMintingPolicyId lang) assetName

-- | Valid Contract using a minting policy with mustMintCurrencyWithRedeemer onchain constraint to check that tokens are correctly minted with the other policy
mustMintCurrencyWithRedeemerContract
    :: Ledger.Language
    -> Integer
    -> TokenName
    -> Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerContract lang mintAmount onChainTokenName = do
    let redeemer = asRedeemer $ MustMintCurrencyWithRedeemer (coinMintingPolicyHash lang) unitRedeemer onChainTokenName mintAmount
        lookups1 = Constraints.mintingPolicy (mustMintPolicy lang)
                <> Constraints.mintingPolicy (coinMintingPolicy lang)
        tx1 = Constraints.mustMintCurrencyWithRedeemer (mustMintPolicyHash lang) redeemer tknName 1
           <> Constraints.mustMintCurrencyWithRedeemer (coinMintingPolicyHash lang) unitRedeemer tknName mintAmount
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintCurrency onchain constraint to check that tokens are correctly minted with the other policy
mustMintCurrencyContract :: Ledger.Language -> Contract () Empty ContractError ()
mustMintCurrencyContract lang = do
    let redeemer = asRedeemer $ MustMintCurrency (coinMintingPolicyHash lang) tknName tknAmount
        lookups1 = Constraints.mintingPolicy (mustMintPolicy lang)
                <> Constraints.mintingPolicy (coinMintingPolicy lang)
        tx1 = Constraints.mustMintCurrencyWithRedeemer (mustMintPolicyHash lang) redeemer tknName 1
           <> Constraints.mustMintCurrency (coinMintingPolicyHash lang) tknName tknAmount
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
     wrap = Scripts.mkUntypedValidator mkMustReferenceOutputV2Validator

mustReferenceOutputV2ValidatorAddress :: Address
mustReferenceOutputV2ValidatorAddress =
    PV2.mkValidatorAddress mustReferenceOutputV2Validator

mustMintValueWithReferenceContract :: Ledger.Language -> Bool -> Contract () Empty ContractError ()
mustMintValueWithReferenceContract lang failPhase2 = do
    utxos <- ownUtxos
    myAddr <- Con.ownAddress
    let (utxoRef, utxo) = Map.toList utxos !! 5
        MintingPolicyHash mph = coinMintingPolicyHash lang
        lookups0 = Constraints.mintingPolicy (coinMintingPolicy lang)
        tx0 = Constraints.mustPayToAddressWithReferenceScript
                (Ledger.toPlutusAddress myAddr)
                (ScriptHash mph)
                Nothing
                (Ada.adaValueOf 35)
    ledgerTx0 <- submitTxConstraintsWith @UnitTest lookups0 tx0
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx0

    utxos' <- ownUtxos
    let refScriptUtxo = head . Map.keys . Map.filter (has $ Tx.decoratedTxOutReferenceScript. _Just) $ utxos'
        redeemerRefUtxo = if failPhase2 then nonExistentTxoRef else refScriptUtxo
        redeemer = asRedeemer $ MustMintValueWithReference redeemerRefUtxo (fromCardanoValue $ tknValue lang)
        lookups1 = Constraints.unspentOutputs (Map.singleton utxoRef utxo <> utxos')
                <> Constraints.mintingPolicy (mustMintPolicy lang)
        tx1 = Constraints.mustMintCurrencyWithRedeemer (mustMintPolicyHash lang) redeemer tknName 1
           <> Constraints.mustMintValueWithReference refScriptUtxo (fromCardanoValue $ tknValue lang)
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

mustMintValueWithReferenceContractV1Failure  :: Ledger.Language -> Contract () Empty ContractError ()
mustMintValueWithReferenceContractV1Failure lang = do
    utxos <- ownUtxos
    myAddr <- Con.ownAddress
    let (utxoRef, utxo) = Map.toList utxos !! 5
        MintingPolicyHash mph = coinMintingPolicyHash lang
        lookups0 = Constraints.mintingPolicy (coinMintingPolicy lang)
        tx0 = Constraints.mustPayToAddressWithReferenceScript
                (Ledger.toPlutusAddress myAddr)
                (ScriptHash mph)
                Nothing
                (Ada.adaValueOf 30)
    ledgerTx0 <- submitTxConstraintsWith @UnitTest lookups0 tx0
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx0

    utxos' <- ownUtxos
    let
        refScriptUtxo = head . Map.keys . Map.filter (has $ Tx.decoratedTxOutReferenceScript . _Just) $ utxos'
        lookups1 = Constraints.unspentOutputs (Map.singleton utxoRef utxo <> utxos')
        tx1 = Constraints.mustMintCurrencyWithReference refScriptUtxo (coinMintingPolicyHash lang) tknName tknAmount
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintValueWithRedeemer onchain constraint to check that tokens are correctly minted with the other policy
mustMintValueWithRedeemerContract :: Ledger.Language -> Value.Value -> Contract () Empty ContractError ()
mustMintValueWithRedeemerContract lang mintValue = do
    let redeemer = asRedeemer $ MustMintValueWithRedeemer unitRedeemer mintValue
        lookups1 = Constraints.mintingPolicy (mustMintPolicy lang)
                <> Constraints.mintingPolicy (coinMintingPolicy lang)
        tx1 = Constraints.mustMintCurrencyWithRedeemer (mustMintPolicyHash lang) redeemer tknName 1
           <> Constraints.mustMintValueWithRedeemer unitRedeemer mintValue
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintValue onchain constraint to check that tokens are correctly minted with the other policy
mustMintValueContract :: Ledger.Language -> Contract () Empty ContractError ()
mustMintValueContract lang = do
    let redeemer = asRedeemer $ MustMintValue (fromCardanoValue $ tknValue lang)
        lookups1 = Constraints.mintingPolicy (mustMintPolicy lang)
                 <> Constraints.mintingPolicy (coinMintingPolicy lang)
        tx1 = Constraints.mustMintCurrencyWithRedeemer (mustMintPolicyHash lang) redeemer tknName 1
           <> Constraints.mustMintValue (fromCardanoValue $ tknValue lang)
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer to mint tokens
mustMintCurrencyWithRedeemerSuccessfulMint :: Ledger.Language -> TestTree
mustMintCurrencyWithRedeemerSuccessfulMint lang =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintCurrencyWithRedeemer"
    (assertValidatedTransactionCount 1)
    (void $ trace $ mustMintCurrencyWithRedeemerContract lang tknAmount tknName)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer to burn tokens
mustMintCurrencyWithRedeemerSuccessfulBurn :: Ledger.Language -> TestTree
mustMintCurrencyWithRedeemerSuccessfulBurn lang =
    let tknBurnAmount = -1000
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValue lang <>)
    in checkPredicateOptions
       options
       "Successful token burn using mustMintCurrencyWithRedeemer"
       (walletFundsAssetClassChange w1 (tknAssetClass lang) tknBurnAmount
       .&&. assertValidatedTransactionCount 1)
       (void $ trace $ mustMintCurrencyWithRedeemerContract lang tknBurnAmount tknName)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer to burn more tokens than the wallet holds, asserts script evaluation error.
mustMintCurrencyWithRedeemerBurnTooMuch :: Ledger.Language -> TestTree
mustMintCurrencyWithRedeemerBurnTooMuch lang =
    let tknBurnAmount = negate (tknAmount + 1)
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValue lang <>)
        contract = mustMintCurrencyWithRedeemerContract lang tknBurnAmount tknName
    in checkPredicateOptions
       options
       "Contract error when burning more than total amount of tokens in wallet balance"
       (assertContractError contract (Trace.walletInstanceTag w1) insufficientFundsError "failed to throw error"
       .&&. assertValidatedTransactionCount 0)
       (void $ trace contract)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer but with a contract that is missing lookup for the minting policy, asserts contract error.
mustMintCurrencyWithRedeemerMissingPolicyLookup :: Ledger.Language -> TestTree
mustMintCurrencyWithRedeemerMissingPolicyLookup lang =
    let contract :: Contract () Empty ContractError () = do
            let tx1 = Constraints.mustMintCurrencyWithRedeemer (coinMintingPolicyHash lang) unitRedeemer tknName tknAmount
            ledgerTx1 <- submitTxConstraintsWith @UnitTest mempty tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicateOptions
    defaultCheckOptions
    "Fail validation when minting policy is missing from lookup"
    (assertContractError
        contract
        (Trace.walletInstanceTag w1)
        (\case
            ConstraintResolutionContractError (Constraints.ScriptHashNotFound (ScriptHash sh)) -> MintingPolicyHash sh == coinMintingPolicyHash lang
            _ -> False)
        "failed to throw error"
    .&&. assertValidatedTransactionCount 0)
    (void $ trace contract)

-- | Uses onchain and offchain constraint mustMintCurrencyWithRedeemer but with a token name mismatch, asserts script evaluation error.
mustMintCurrencyWithRedeemerPhase2Failure :: Ledger.Language -> TestTree
mustMintCurrencyWithRedeemerPhase2Failure lang =
    checkPredicate
    "Phase 2 failure when policy mints with unexpected token name"
    (assertEvaluationError "L9")
    (void $ trace $ mustMintCurrencyWithRedeemerContract lang tknAmount $ TokenName "WrongToken")

-- | Contract without the required minting policy lookup. Uses mustMintCurrencyWithRedeemer constraint.
mustMintCurrencyWithRedeemerMissingPolicyContract :: Ledger.Language -> Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerMissingPolicyContract lang = do
    networkId <- Params.pNetworkId <$> getParams
    let lookups1 = Constraints.typedValidatorLookups $ mustMintCurrencyWithRedeemerTypedValidator tknName
        tx1 = Constraints.mustPayToTheScriptWithDatumHash () (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Typed.validatorCardanoAddress networkId $ mustMintCurrencyWithRedeemerTypedValidator tknName)
    let lookups2 =
            Constraints.typedValidatorLookups (mustMintCurrencyWithRedeemerTypedValidator tknName) <>
            Constraints.unspentOutputs utxos
        tx2 =
            Constraints.spendUtxosFromTheScript utxos () <>
            Constraints.mustMintCurrencyWithRedeemer (coinMintingPolicyHash lang) unitRedeemer tknName tknAmount
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

{-# INLINEABLE mustMintCurrencyWithRedeemerValidator #-}
mustMintCurrencyWithRedeemerValidator :: MintingPolicyHash -> Redeemer -> TokenName -> Integer -> () -> () -> Ledger.ScriptContext -> Bool
mustMintCurrencyWithRedeemerValidator mph r tn amt _ _ =
  Constraints.checkScriptContext @Void @Void (Constraints.mustMintCurrencyWithRedeemer mph r tn amt)

mustMintCurrencyWithRedeemerTypedValidator :: TokenName -> Scripts.TypedValidator UnitTest
mustMintCurrencyWithRedeemerTypedValidator tn = Scripts.mkTypedValidator @UnitTest
    ($$(PlutusTx.compile [||mustMintCurrencyWithRedeemerValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode (coinMintingPolicyHash Ledger.PlutusV1)
        `PlutusTx.applyCode` PlutusTx.liftCode unitRedeemer
        `PlutusTx.applyCode` PlutusTx.liftCode tn
        `PlutusTx.applyCode` PlutusTx.liftCode tknAmount)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- | Uses onchain and offchain constraint mustMintCurrency to mint tokens
mustMintCurrencySuccessfulMint :: Ledger.Language -> TestTree
mustMintCurrencySuccessfulMint lang =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintCurrency"
    (assertValidatedTransactionCount 1)
    (void $ trace $ mustMintCurrencyContract lang)

-- | Uses onchain and offchain constraint mustMintValueWithRedeemer to mint tokens
mustMintValueWithRedeemerSuccessfulMint :: Ledger.Language -> TestTree
mustMintValueWithRedeemerSuccessfulMint lang =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintValueWithRedeemer"
    (assertValidatedTransactionCount 1)
    (void $ trace $ mustMintValueWithRedeemerContract lang $ fromCardanoValue $ tknValue lang)

-- | Uses onchain and offchain constraint mustMintValueWithRedeemer to burn tokens
mustMintValueWithRedeemerSuccessfulBurn :: Ledger.Language -> TestTree
mustMintValueWithRedeemerSuccessfulBurn lang =
    let tknBurnAmount = -1000
        options = defaultCheckOptions & changeInitialWalletValue w1 (tknValue lang <>)
    in checkPredicateOptions
       options
       "Successful token burn using mustMintValueWithRedeemer"
       (walletFundsAssetClassChange w1 (tknAssetClass lang) tknBurnAmount
       .&&. assertValidatedTransactionCount 1)
       (void $ trace $ mustMintValueWithRedeemerContract lang (fromCardanoValue $ tknValue' lang tknBurnAmount))

-- | Uses onchain and offchain constraint mustMintValue to mint tokens
mustMintValueSuccessfulMint :: Ledger.Language -> TestTree
mustMintValueSuccessfulMint lang =
    checkPredicateOptions
    defaultCheckOptions
    "Successful spend of tokens using mustMintValue"
    (assertValidatedTransactionCount 1)
    (void $ trace $ mustMintValueContract lang)

mustMintWithReferenceV1Failure :: Ledger.Language -> TestTree
mustMintWithReferenceV1Failure lang =
    checkPredicateOptions
    defaultCheckOptions
    "MustMintValue with reference fails because v1 is not supported"
    (assertFailedTransaction (const $ cardanoLedgerErrorContaining "ReferenceInputsNotSupported"))
    (void $ trace $ mustMintValueWithReferenceContractV1Failure lang)

mustMintWithReferencePhase2Failure :: Ledger.Language -> TestTree
mustMintWithReferencePhase2Failure lang =
    checkPredicateOptions
    defaultCheckOptions
    "MustMintValue with reference fails phase 2 validation error"
    (assertEvaluationError "L9")
    (void $ trace $ mustMintValueWithReferenceContract lang True)

mustMintWithReferenceSuccessful :: Ledger.Language -> TestTree
mustMintWithReferenceSuccessful lang =
    checkPredicateOptions
    defaultCheckOptions
    "Successful mustMintValue with reference"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustMintValueWithReferenceContract lang False)


-- | Valid Contract using a minting policy with mustMintCurrencyWithRedeemer onchain constraint to check that tokens are correctly minted with the other policy
mustMintCurrencyWithRedeemerTxContract
    :: Ledger.Language
    -> Integer
    -> TokenName
    -> Contract () Empty ContractError ()
mustMintCurrencyWithRedeemerTxContract lang mintAmount onChainTokenName = do
    utxos <- ownUtxos
    let utxoRefForBalance1 = fst $ Map.toList utxos !! 2
        redeemer = asRedeemer $ MustMintCurrencyWithRedeemer (coinMintingPolicyHash lang) unitRedeemer onChainTokenName mintAmount
        lookups1 = Constraints.mintingPolicy (mustMintPolicy lang)
                <> Constraints.mintingPolicy (coinMintingPolicy lang)
                <> Constraints.unspentOutputs utxos
        tx1 = Constraints.mustMintCurrencyWithRedeemer (mustMintPolicyHash lang) redeemer tknName 1
           <> Constraints.mustMintCurrencyWithRedeemer (coinMintingPolicyHash lang) unitRedeemer tknName mintAmount
           <> Constraints.mustSpendPubKeyOutput utxoRefForBalance1
           <> Constraints.mustUseOutputAsCollateral utxoRefForBalance1
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

-- | Valid Contract using a minting policy with mustMintCurrency onchain constraint to check that tokens are correctly minted with the other policy
mustMintCurrencyTxContract :: Ledger.Language -> Contract () Empty ContractError ()
mustMintCurrencyTxContract lang = do
    utxos <- ownUtxos
    let utxoRefForBalance1 = fst $ Map.toList utxos !! 2
        redeemer = asRedeemer $ MustMintCurrency (coinMintingPolicyHash lang) tknName tknAmount
        lookups1 = Constraints.mintingPolicy (mustMintPolicy lang)
                <> Constraints.mintingPolicy (coinMintingPolicy lang)
        tx1 = Constraints.mustMintCurrencyWithRedeemer (mustMintPolicyHash lang) redeemer tknName 1
           <> Constraints.mustMintCurrency (coinMintingPolicyHash lang) tknName tknAmount
           <> Constraints.mustSpendPubKeyOutput utxoRefForBalance1
           <> Constraints.mustUseOutputAsCollateral utxoRefForBalance1
    ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1





{-# INLINEABLE mkMustMintPolicy #-}
mkMustMintPolicy :: (Constraints.TxConstraints () () -> sc -> Bool) -> ConstraintParams -> sc -> Bool
mkMustMintPolicy checkScriptContext t = case t of
    MustMintCurrencyWithRedeemer mph r tn i -> checkScriptContext (Constraints.mustMintCurrencyWithRedeemer mph r tn i)
    MustMintCurrency mph tn i               -> checkScriptContext (Constraints.mustMintCurrency mph tn i)
    MustMintValueWithRedeemer r v           -> checkScriptContext (Constraints.mustMintValueWithRedeemer r v)
    MustMintValue v                         -> checkScriptContext (Constraints.mustMintValue v)
    MustMintCurrencyWithReference ref mph tn i  -> checkScriptContext (Constraints.mustMintCurrencyWithReference ref mph tn i)
    MustMintValueWithReference ref v                         -> checkScriptContext (Constraints.mustMintValueWithReference ref v)
    MustMintValueWithRedeemerAndReference r mref v           -> checkScriptContext (Constraints.mustMintValueWithRedeemerAndReference r mref v)
    MustMintCurrencyWithRedeemerAndReference mref mph r tn i -> checkScriptContext (Constraints.mustMintCurrencyWithRedeemerAndReference mref mph r tn i)

{-# INLINEABLE mustMintPolicyV1 #-}
mustMintPolicyV1 :: Scripts.MintingPolicy
mustMintPolicyV1 = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustMintPolicy = mkMustMintPolicy Constraints.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustMintPolicy

{-# INLINEABLE mustMintPolicyV2 #-}
mustMintPolicyV2 :: Scripts.MintingPolicy
mustMintPolicyV2 = PV2.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        checkedMkMustMintPolicy = mkMustMintPolicy TCV2.checkScriptContext
        wrap = Scripts.mkUntypedMintingPolicy checkedMkMustMintPolicy

mustMintPolicy :: Ledger.Language -> Ledger.Versioned Scripts.MintingPolicy
mustMintPolicy lang = case lang of
  Ledger.PlutusV1 -> Ledger.Versioned mustMintPolicyV1 lang
  Ledger.PlutusV2 -> Ledger.Versioned mustMintPolicyV2 lang

mustMintPolicyHash :: Ledger.Language -> Ledger.MintingPolicyHash
mustMintPolicyHash l = case l of
  Ledger.PlutusV1 -> PSU.V1.mintingPolicyHash $ Scripts.unversioned $ mustMintPolicy l
  Ledger.PlutusV2 -> PSU.V2.mintingPolicyHash $ Scripts.unversioned $ mustMintPolicy l

mustMintPolicyCurrencySymbol :: Ledger.Language -> Value.CurrencySymbol
mustMintPolicyCurrencySymbol = Value.mpsSymbol . mustMintPolicyHash

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
