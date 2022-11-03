{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
module Spec.Contract.Tx.Constraints.MustIncludeDatum(tests) where

import Test.Tasty (TestTree, testGroup)

import Control.Monad (void)
import Data.Text qualified as T
import Data.Void (Void)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OnChain.V1 qualified as Constraints
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
    testGroup "MustIncludeDatumInTx"
        [ -- offchain uses the datum that is required to witness spend from
          -- script, onchain expects only the required datum in witness set
          mustIncludeDatumInTxForRequiredDatum
          -- mustIncludeDatumInTx constraint called before other constraints
          -- should be sucessfull, i.e.
          -- `otherConstraint .. <> mustIncludeDatumInTx ..`
          -- should be the same as
          -- `mustIncludeDatumInTx .. <> otherConstraint ..`
        , mustIncludeDatumInTxCalledBeforeOtherConstraints
          -- offchain uses optional datum (not required for witnessing spending
          -- script), onchain expects both required and optional datums in
          -- witness set
        , mustIncludeDatumInTxForOptionalDatum
          -- no offchain constraint, onchain expects only the required datum
          -- for witnessing spending script
        , withoutOffChainConstraintRequiredDatumIsStillncludedInWitnessSet
          -- no offchain constraint, onchain (minting policy) expects no datum
          -- when there's no spending script to witness.
        , withoutOffChainConstraintDatumIsNotIncludedInTxBodyByDefault
          -- offchain uses optional datum without datum hash at output, onchain
          -- expects no datums in witness set
        , mustIncludeDatumInTxForOptionalDatumWithoutOutputDoesNotIncludeDatum
          -- offchain uses optional datum being sent to pubkey address instead
          -- of script address (no required datum), onchain expects optional
          -- datum only
        , mustIncludeDatumInTxToPubKeyAddress
          -- no offchain constraint, onchain expects some datum, asserts for
          -- phase-2 error
        , phase2FailureWhenDatumIsNotInWitnessSet
        ]

validatorDatumBs :: P.BuiltinByteString
validatorDatumBs = "datum"

validatorDatum :: Datum
validatorDatum = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData validatorDatumBs

tknValue :: Value.Value
tknValue = Value.singleton mustIncludeDatumInTxPolicyCurrencySymbol "mint-me" 1

mustIncludeDatumInTxWhenPayingToScriptContract :: [Datum] -> [Datum] -> Contract () Empty ContractError ()
mustIncludeDatumInTxWhenPayingToScriptContract offChainDatums onChainDatums = do
    let lookups1 = Constraints.typedValidatorLookups typedValidator
        tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                validatorDatumBs
                (Ada.lovelaceValueOf 25_000_000)
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
            else mconcat $ fmap (\datum -> Constraints.mustPayToOtherScriptWithDatumInTx valHash datum (Ada.lovelaceValueOf 2_000_000)
                                        <> Constraints.mustIncludeDatumInTx datum) offChainDatums

trace :: Contract () Empty ContractError () -> Trace.EmulatorTrace ()
trace contract = do
    void $ Trace.activateContractWallet w1 contract
    void $ Trace.waitNSlots 1

-- | Uses onchain and offchain constraint mustIncludeDatumInTx to include and
-- verify that the datum required for script execution is included in the
-- witness map
mustIncludeDatumInTxForRequiredDatum :: TestTree
mustIncludeDatumInTxForRequiredDatum =
    let constraintDatums = [validatorDatum]
    in checkPredicate
    "Successful use of mustIncludeDatumInTx constraint where datum is already required for spending from script"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustIncludeDatumInTxWhenPayingToScriptContract constraintDatums constraintDatums)

mustIncludeDatumInTxCalledBeforeOtherConstraints :: TestTree
mustIncludeDatumInTxCalledBeforeOtherConstraints =
    checkPredicate
        "Successful use of mustIncludeDatumInTx constraint where the constraint is called before other constraints."
        (assertValidatedTransactionCount 2)
        (void $ trace contract)
 where
    contract = do
        let otherDatumBs = Datum
                         $ PlutusTx.dataToBuiltinData
                         $ PlutusTx.toData ("otherDatum" :: P.BuiltinByteString)
        let lookups1 = Constraints.typedValidatorLookups typedValidator
            tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                    validatorDatumBs
                    (Ada.lovelaceValueOf 25_000_000)
        ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
        awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

        utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash typedValidator)
        let lookups2 =
                Constraints.typedValidatorLookups typedValidator
                <> Constraints.unspentOutputs utxos
            tx2 =
                Constraints.collectFromTheScript utxos [validatorDatum, otherDatumBs]
                <> Constraints.mustPayToOtherScriptWithDatumInTx
                     valHash
                     otherDatumBs
                     (Ada.lovelaceValueOf 2_000_000)
                <> Constraints.mustIncludeDatumInTx otherDatumBs
        ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
        awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

-- | Uses onchain and offchain constraint mustIncludeDatumInTx to include and
-- verify that additional optional datum that is not required for script
-- execution is included in the witness map
mustIncludeDatumInTxForOptionalDatum :: TestTree
mustIncludeDatumInTxForOptionalDatum =
    let otherDatumBs1 = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ("otherDatum1" :: P.BuiltinByteString)
        otherDatumBs2 = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ("otherDatum2" :: P.BuiltinByteString)
        offChainConstraintDatums = [otherDatumBs1, otherDatumBs2]
        onChainConstraintDatums  = [validatorDatum, otherDatumBs1, otherDatumBs2]
    in checkPredicate
    "Successful use of mustIncludeDatumInTx constraint when including optional datums that are not required for spending from script"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustIncludeDatumInTxWhenPayingToScriptContract offChainConstraintDatums onChainConstraintDatums)

-- | Uses onchain constraint mustIncludeDatumInTx to verify that the datum
-- required for script execution is included in the witness map
withoutOffChainConstraintRequiredDatumIsStillncludedInWitnessSet :: TestTree
withoutOffChainConstraintRequiredDatumIsStillncludedInWitnessSet =
    let offChainConstraintDatums = []
        onChainConstraintDatums  = [validatorDatum]
    in checkPredicate
    "Successful use of onchain mustIncludeDatumInTx (no offchain constraint) when required datum is automatically included to witness spending from script"
    (assertValidatedTransactionCount 2)
    (void $ trace $ mustIncludeDatumInTxWhenPayingToScriptContract offChainConstraintDatums onChainConstraintDatums)

-- | Uses onchain constraint mustIncludeDatumInTx to verify that no datum is
-- included in txbody when sending funds to script address but not to witness
-- spending from script
withoutOffChainConstraintDatumIsNotIncludedInTxBodyByDefault :: TestTree
withoutOffChainConstraintDatumIsNotIncludedInTxBodyByDefault =
    let onChainConstraintDatumsAsRedeemer = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ([] :: [Datum])
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustIncludeDatumInTxPolicy
                tx1 =
                    Constraints.mustPayToOtherScript
                        valHash
                        (Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData validatorDatumBs)
                        (Ada.lovelaceValueOf 25_000_000)
                 <> Constraints.mustMintValueWithRedeemer onChainConstraintDatumsAsRedeemer tknValue
            mkTxConstraints @Void lookups1 tx1 >>= submitTxConfirmed

    in checkPredicate
    "Successful use of onchain mustIncludeDatumInTx (no offchain constraint) to assert that datum is not redundantly included in txbody when sending funds to script but not to witness spending from script"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Offchain constraint attempts to include optional datum without an output
-- to hold its hash. Onchain constraint expects only the required datum.
mustIncludeDatumInTxForOptionalDatumWithoutOutputDoesNotIncludeDatum :: TestTree
mustIncludeDatumInTxForOptionalDatumWithoutOutputDoesNotIncludeDatum =
    let offChainConstraintDatum = Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ("otherDatum" :: P.BuiltinByteString)
        onChainConstraintDatums = [validatorDatum]
        contract = do
            let lookups1 = Constraints.typedValidatorLookups typedValidator
                tx1 = Constraints.mustPayToTheScriptWithDatumInTx
                        validatorDatumBs
                        (Ada.lovelaceValueOf 25_000_000)
            ledgerTx1 <- submitTxConstraintsWith lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

            utxos <- utxosAt (Ledger.scriptHashAddress $ Scripts.validatorHash typedValidator)
            let lookups2 =
                    Constraints.typedValidatorLookups typedValidator
                    <> Constraints.unspentOutputs utxos
                tx2 =
                    Constraints.collectFromTheScript utxos onChainConstraintDatums
                    <> Constraints.mustIncludeDatumInTx offChainConstraintDatum -- without producing any outputs with datum hash
            handleError (\err -> logError $ "Caught error: " ++ T.unpack err) $ do
                -- Should fail with error 'DatumNotFoundInTx'
                ledgerTx2 <- submitTxConstraintsWith @UnitTest lookups2 tx2
                awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx2

    in checkPredicate
    "Use of offchain mustIncludeDatumInTx without an output to hold the hash results in only the required datum being included in the witness set"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Offchain constraint includes optional datum and stores its hash in an
-- output at pubkey address. No spending scripts involved. Onchain constraint
-- expects only the optional datum in witness set.
mustIncludeDatumInTxToPubKeyAddress :: TestTree
mustIncludeDatumInTxToPubKeyAddress =
    let onChainConstraintDatumsAsRedeemer = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ([validatorDatum] :: [Datum])
        contract = do
            let lookups1 = Constraints.plutusV1MintingPolicy mustIncludeDatumInTxPolicy
                tx1 = Constraints.mustPayWithDatumInTxToPubKey (mockWalletPaymentPubKeyHash w1) validatorDatum (Ada.lovelaceValueOf 25_000_000)
                   <> Constraints.mustIncludeDatumInTx validatorDatum
                   <> Constraints.mustMintValueWithRedeemer onChainConstraintDatumsAsRedeemer tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Use of offchain mustIncludeDatumInTx with a pubkey output results in only the optional datum being included in the witness set"
    (assertValidatedTransactionCount 1)
    (void $ trace contract)

-- | Onchain constraint fails validation when checking for datum in witness set
-- that is not there. Asserts phase-2 error occurs.
phase2FailureWhenDatumIsNotInWitnessSet :: TestTree
phase2FailureWhenDatumIsNotInWitnessSet =
    let onChainConstraintDatumsAsRedeemer = Redeemer $ PlutusTx.dataToBuiltinData $ PlutusTx.toData ([validatorDatum] :: [Datum])
        contract = do
            let lookups1 = Constraints.typedValidatorLookups typedValidator
                        <> Constraints.plutusV1MintingPolicy mustIncludeDatumInTxPolicy
                tx1 =
                    Constraints.mustPayToOtherScript
                        valHash
                        (Datum $ PlutusTx.dataToBuiltinData $ PlutusTx.toData validatorDatumBs)
                        (Ada.lovelaceValueOf 25_000_000)
                 <> Constraints.mustMintValueWithRedeemer onChainConstraintDatumsAsRedeemer tknValue
            ledgerTx1 <- submitTxConstraintsWith @UnitTest lookups1 tx1
            awaitTxConfirmed $ Tx.getCardanoTxId ledgerTx1

    in checkPredicate
    "Phase-2 validation failure occurs when onchain constraints checks for datum that is not in the witness set"
    (assertFailedTransaction (\_ err -> case err of {Ledger.ScriptFailure (EvaluationError ("L2":_) _) -> True; _ -> False }))
    (void $ trace contract)

-----

{-# INLINEABLE mkMustIncludeDatumValidator #-}
mkMustIncludeDatumValidator :: P.BuiltinByteString -> [Datum] -> ScriptContext -> Bool
mkMustIncludeDatumValidator datum expectedDatums ctx = P.traceIfFalse "datum is not 'datum'" (datum P.== "datum") P.&&
                                       P.traceIfFalse "mustIncludeDatumInTx not satisfied" (Constraints.checkScriptContext @() @() (P.mconcat mustIncludeDatumInTxs) ctx) P.&&
                                       P.traceIfFalse "unexpected number of datums in witness set" checkDatumMapLength
    where
        mustIncludeDatumInTxs = Constraints.mustIncludeDatumInTx P.<$> expectedDatums
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
mkMustIncludeDatumPolicy expectedDatums ctx = Constraints.checkScriptContext @() @() (P.mconcat mustIncludeDatumInTxs) ctx P.&&
                                              P.traceIfFalse "unexpected number of datums in witness set" checkDatumMapLength
    where
        mustIncludeDatumInTxs   = P.fmap Constraints.mustIncludeDatumInTx expectedDatums
        checkDatumMapLength = P.length (txInfoData P.$ scriptContextTxInfo ctx) P.== P.length expectedDatums

mustIncludeDatumInTxPolicy :: Scripts.MintingPolicy
mustIncludeDatumInTxPolicy = Ledger.mkMintingPolicyScript $$(PlutusTx.compile [||wrap||])
    where
        wrap = Scripts.mkUntypedMintingPolicy mkMustIncludeDatumPolicy

mustIncludeDatumInTxPolicyHash :: Ledger.MintingPolicyHash
mustIncludeDatumInTxPolicyHash = PSU.V1.mintingPolicyHash mustIncludeDatumInTxPolicy

mustIncludeDatumInTxPolicyCurrencySymbol :: CurrencySymbol
mustIncludeDatumInTxPolicyCurrencySymbol = CurrencySymbol $ unsafeFromBuiltinData $ toBuiltinData mustIncludeDatumInTxPolicyHash
