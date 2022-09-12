{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.TxConstraints.MustIncludeDatum(tests) where

import Control.Monad (void)
import Test.Tasty (TestTree, testGroup)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain qualified as Constraints (plutusV1MintingPolicy, typedValidatorLookups,
                                                             unspentOutputs)
import Ledger.Constraints.OnChain.V1 qualified as Constraints (checkScriptContext)
import Ledger.Constraints.TxConstraints qualified as Constraints (collectFromTheScript, mustIncludeDatum,
                                                                  mustMintValueWithRedeemer, mustPayToOtherScript,
                                                                  mustPayToTheScript, mustPayWithDatumToPubKey)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract as Con
import Plutus.Contract.Test (assertFailedTransaction, assertValidatedTransactionCount, checkPredicate,
                             mockWalletPaymentPubKeyHash, w1)
import Plutus.Script.Utils.V1.Scripts qualified as PSU.V1
import Plutus.Trace qualified as Trace
import Plutus.V1.Ledger.Api (CurrencySymbol (CurrencySymbol), Datum (Datum), Redeemer (Redeemer),
                             ScriptContext (scriptContextTxInfo), ToData (toBuiltinData), TxInfo (txInfoData),
                             UnsafeFromData (unsafeFromBuiltinData), Validator, ValidatorHash)
import Plutus.V1.Ledger.Scripts (ScriptError (EvaluationError))
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx qualified
import PlutusTx.Prelude qualified as P

tests :: TestTree
tests =
    testGroup "MustIncludeDatum"
        [ mustIncludeDatumForRequiredDatum                                  -- offchain uses the datum that is required to witness spend from script, onchain expects only the required datum in witness set
        , mustIncludeDatumForOptionalDatum                                  -- offchain uses optional datum (not required for witnessing spending script), onchain expects both required and optional datums in witness set
        , withoutOffChainConstraintRequiredDatumIsStillncludedInWitnessSet  -- no offchain constraint, onchain expects only the required datum for witnessing spending script
        --FAILING, withoutOffChainConstraintDatumIsNotIncludedInTxBodyByDefault      -- no offchain constraint, onchain (minting policy) expects no datum when there's no spending script to witness.
        --FAILING, mustIncludeDatumForOptionalDatumWithoutOutputDoesNotIncludeDatum  -- offchain uses optional datum without datum hash at output, onchain expects no datums in witness set
        , mustIncludeDatumToPubKeyAddress                                   -- offchain uses optional datum being sent to pubkey address instead of script address (no required datum), onchain expects optional datum only
        --FAILING, phase2FailureWhenDatumIsNotInWitnessSet                           -- no offchain constraint, onchain expects some datum, asserts for phase-2 error
        ]

validatorDatumBs :: P.BuiltinByteString
validatorDatumBs = "datum"

validatorDatum :: Datum
validatorDatum = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData validatorDatumBs

tknValue :: Value.Value
tknValue = Value.singleton mustIncludeDatumPolicyCurrencySymbol "mint-me" 1

mustIncludeDatumWhenPayingToScriptContract :: [Datum] -> [Datum] -> Contract () Empty ContractError ()
mustIncludeDatumWhenPayingToScriptContract offChainDatums onChainDatums = do
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScript validatorDatumBs (Ada.lovelaceValueOf 25_000_000)
    ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash typedValidator)
    let lookups2 =
            Constraints.typedValidatorLookups typedValidator
            <> Constraints.unspentOutputs utxos
        tx2 =
            Constraints.collectFromTheScript utxos onChainDatums
            <> mustPayToTheScriptAndIncludeDatumsIfUsingOffChainConstraint
    ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
    awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    where
        mustPayToTheScriptAndIncludeDatumsIfUsingOffChainConstraint =
            if null offChainDatums
            then Constraints.mustPayToOtherScript valHash validatorDatum (Ada.lovelaceValueOf 2_000_000)
            else mconcat $ fmap (\datum -> Constraints.mustPayToOtherScript valHash validatorDatum (Ada.lovelaceValueOf 2_000_000) <> Constraints.mustIncludeDatum datum) offChainDatums

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

-- | Uses onchain and offchain constraint mustIncludeDatum to include and verify that the datum required for script execution is included in the witness map
mustIncludeDatumForRequiredDatum :: TestTree
mustIncludeDatumForRequiredDatum =
    let constraintDatums = [validatorDatum]
    in checkPredicate
    "Successful use of mustIncludeDatum constraint where datum is already required for spending from script"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustIncludeDatumWhenPayingToScriptContract constraintDatums constraintDatums)

-- | Uses onchain and offchain constraint mustIncludeDatum to include and verify that additional optional datum that is not required for script execution is included in the witness map
mustIncludeDatumForOptionalDatum :: TestTree
mustIncludeDatumForOptionalDatum =
    let otherDatumBs1 = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ("otherDatum1" :: P.BuiltinByteString)
        otherDatumBs2 = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ("otherDatum2" :: P.BuiltinByteString)
        offChainConstraintDatums = [otherDatumBs1, otherDatumBs2]
        onChainConstraintDatums  = [validatorDatum, otherDatumBs1, otherDatumBs2]
    in checkPredicate
    "Successful use of mustIncludeDatum constraint when including optional datums that are not required for spending from script"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustIncludeDatumWhenPayingToScriptContract offChainConstraintDatums onChainConstraintDatums)

-- | Uses onchain constraint mustIncludeDatum to verify that the datum required for script execution is included in the witness map
withoutOffChainConstraintRequiredDatumIsStillncludedInWitnessSet ::TestTree
withoutOffChainConstraintRequiredDatumIsStillncludedInWitnessSet =
    let offChainConstraintDatums = []
        onChainConstraintDatums  = [validatorDatum]
    in checkPredicate
    "Successful use of onchain mustIncludeDatum (no offchain constraint) when required datum is automatically included to witness spending from script"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustIncludeDatumWhenPayingToScriptContract offChainConstraintDatums onChainConstraintDatums)

-- FAILS: to be fixed by PLT-807
-- | Uses onchain constraint mustIncludeDatum to verify that no datum is included in txbody when sending funds to script address but not to witness spending from script
withoutOffChainConstraintDatumIsNotIncludedInTxBodyByDefault :: TestTree
withoutOffChainConstraintDatumIsNotIncludedInTxBodyByDefault =
    let onChainConstraintDatumsAsRedeemer = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ([] :: [Datum])
        contract = do
            let lookups1 = Constraints.typedValidatorLookups typedValidator <>
                           Constraints.plutusV1MintingPolicy mustIncludeDatumPolicy
                tx1 = Constraints.mustPayToTheScript validatorDatumBs (Ada.lovelaceValueOf 25_000_000) <>
                      Constraints.mustMintValueWithRedeemer onChainConstraintDatumsAsRedeemer tknValue
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Successful use of onchain mustIncludeDatum (no offchain constraint) to assert that datum is not redundantly included in txbody when sending funds to script but not to witness spending from script"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- FAILS: to be fixed by PLT-807
-- | Offchain constraint attempts to include optional datum without an output to hold its hash. Onchain constraint expects only the required datum.
mustIncludeDatumForOptionalDatumWithoutOutputDoesNotIncludeDatum :: TestTree
mustIncludeDatumForOptionalDatumWithoutOutputDoesNotIncludeDatum =
    let offChainConstraintDatum = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ("otherDatum" :: P.BuiltinByteString)
        onChainConstraintDatums = [validatorDatum]
        contract = do
            let lookups1 = Constraints.typedValidatorLookups typedValidator
                tx1 = Constraints.mustPayToTheScript validatorDatumBs (Ada.lovelaceValueOf 25_000_000)
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash typedValidator)
            let lookups2 =
                    Constraints.typedValidatorLookups typedValidator
                    <> Constraints.unspentOutputs utxos
                tx2 =
                    Constraints.collectFromTheScript utxos onChainConstraintDatums
                    <> Constraints.mustIncludeDatum offChainConstraintDatum -- without producing any outputs with datum hash
            ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    in checkPredicate
    "Use of offchain mustIncludeDatum without an output to hold the hash results in only the required datum being included in the witness set"
    (assertValidatedTransactionCount 2)
    (void $ trace contract)

-- | Offchain constraint includes optional datum and stores its hash in an output at pubkey address. No spending scripts involved. Onchain constraint expects only the optional datum in witness set.
mustIncludeDatumToPubKeyAddress :: TestTree
mustIncludeDatumToPubKeyAddress =
    let onChainConstraintDatumsAsRedeemer = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ([validatorDatum] :: [Datum])
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustIncludeDatumPolicy
                tx1 = Constraints.mustPayWithDatumToPubKey (mockWalletPaymentPubKeyHash w1) validatorDatum (Ada.lovelaceValueOf 25_000_000)
                   <> Constraints.mustIncludeDatum validatorDatum
                   <> Constraints.mustMintValueWithRedeemer onChainConstraintDatumsAsRedeemer tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Use of offchain mustIncludeDatum with a pubkey output results in only the optional datum being included in the witness set"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- FAILS: to be fixed by PLT-807
-- | Onchain constraint fails validation when checking for datum in witness set that is not there. Asserts phase-2 error occurs.
phase2FailureWhenDatumIsNotInWitnessSet :: TestTree
phase2FailureWhenDatumIsNotInWitnessSet =
    let onChainConstraintDatumsAsRedeemer = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ([validatorDatum] :: [Datum])
        contract = do
            let lookups1 = Constraints.typedValidatorLookups typedValidator
                        <> Constraints.plutusV1MintingPolicy mustIncludeDatumPolicy
                tx1 = Constraints.mustPayToTheScript validatorDatumBs (Ada.lovelaceValueOf 25_000_000)
                   <> Constraints.mustMintValueWithRedeemer onChainConstraintDatumsAsRedeemer tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-2 validation failure occurs when onchain constraints checks for datum that is not in the witness set"
    (assertFailedTransaction (\_ err _ -> case err of {Ledger.ScriptFailure (EvaluationError ("L2":_) _) -> True; _ -> False }))
    (void $ trace contract)

-----

{-# INLINEABLE mkMustIncludeDatumValidator #-}
mkMustIncludeDatumValidator :: P.BuiltinByteString -> [Datum] -> ScriptContext -> Bool
mkMustIncludeDatumValidator datum expectedDatums ctx = P.traceIfFalse "datum is not 'datum'" (datum P.== "datum") P.&&
                                       P.traceIfFalse "mustIncludeDatum not satisfied" (Constraints.checkScriptContext @() @() (P.mconcat mustIncludeDatums) ctx) P.&&
                                       P.traceIfFalse "unexpected number of datums in witness set" checkDatumMapLength
    where
        mustIncludeDatums   = P.fmap Constraints.mustIncludeDatum expectedDatums
        checkDatumMapLength = P.length (txInfoData P.$ scriptContextTxInfo ctx) P.== P.length expectedDatums

data UnitTest
instance Scripts.ValidatorTypes UnitTest where
    type instance DatumType UnitTest = P.BuiltinByteString
    type instance RedeemerType UnitTest = [Datum]

typedValidator :: Scripts.TypedValidator UnitTest
typedValidator = Scripts.mkTypedValidator @UnitTest
    $$(PlutusTx.compile [||mkMustIncludeDatumValidator||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorScript :: Validator
validatorScript = Scripts.validatorScript typedValidator

valHash :: ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = Ledger.scriptHashAddress valHash

-----

{-# INLINEABLE mkMustIncludeDatumPolicy #-}
mkMustIncludeDatumPolicy :: [Datum] -> ScriptContext -> Bool
mkMustIncludeDatumPolicy expectedDatums ctx = Constraints.checkScriptContext @() @() (P.mconcat mustIncludeDatums) ctx P.&&
                                              P.traceIfFalse "unexpected number of datums in witness set" checkDatumMapLength
    where
        mustIncludeDatums   = P.fmap Constraints.mustIncludeDatum expectedDatums
        checkDatumMapLength = P.length (txInfoData P.$ scriptContextTxInfo ctx) P.== P.length expectedDatums

mustIncludeDatumPolicy :: Scripts.MintingPolicy
mustIncludeDatumPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = Scripts.mkUntypedMintingPolicy mkMustIncludeDatumPolicy

mustIncludeDatumPolicyHash :: Ledger.MintingPolicyHash
mustIncludeDatumPolicyHash = PSU.V1.mintingPolicyHash mustIncludeDatumPolicy

mustIncludeDatumPolicyCurrencySymbol :: CurrencySymbol
mustIncludeDatumPolicyCurrencySymbol = CurrencySymbol $ unsafeFromBuiltinData $ toBuiltinData mustIncludeDatumPolicyHash
