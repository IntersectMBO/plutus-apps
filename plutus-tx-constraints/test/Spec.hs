{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Main(main) where

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Generators qualified as Gen hiding (someTokenValue)
import Control.Lens (preview, view)
import Control.Monad (forM_, guard)
import Control.Monad.Except (runExcept)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalStateT)
import Data.Default (def)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Void (Void)
import Gen.Cardano.Api.Typed (genTxId)
import Hedgehog (Property, annotateShow, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger qualified hiding (TxId (..))
import Ledger.Address (StakePubKeyHash (StakePubKeyHash), stakePubKeyHashCredential, stakeValidatorHashCredential)
import Ledger.Credential (StakingCredential)
import Ledger.Scripts (WitCtx (WitCtxStake), examplePlutusScriptAlwaysSucceedsHash)
import Ledger.Slot qualified as Slot
import Ledger.Test (asRedeemer)
import Ledger.Tx (TxOut (TxOut))
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Tx.Constraints.OffChain qualified as OC
import Ledger.Tx.Constraints.ValidityInterval qualified as Interval
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.V1.Generators qualified as Gen
import Plutus.Script.Utils.Value (Value)
import Plutus.Script.Utils.Value qualified as Value
import Plutus.V1.Ledger.Api qualified as Ledger
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap
import Spec.Balancing qualified as Balancing
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = defaultMain $ testGroup "all-tests"
    [ tests
    , Balancing.tests
    ]

tests :: TestTree
tests = testGroup "plutus-tx-constraints"
    [ testPropertyNamed "missing value spent"
        "missingValueSpentProp" missingValueSpentProp
    , testPropertyNamed "mustPayToPubKeyAddress should create output addresses with stake pub key hash"
                        "mustPayToPubKeyAddressStakePubKeyNotNothingProp"
                        mustPayToPubKeyAddressStakePubKeyNotNothingProp
    , testPropertyNamed "mustPayToOtherScriptAddressWithDatumHash should create output addresses with stake validator hash"
         "mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp"
         mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp
    , testPropertyNamed "mustUseOutputAsCollateral should add a collateral input"
        "mustUseOutputAsCollateralProp" mustUseOutputAsCollateralProp
    , testPropertyNamed "prepareConstraints keep only one duplicated mustSpendPubKeyOutput constraints"
        "mustSpendPubKeyOutputDuplicate" mustSpendPubKeyOutputDuplicate
    , testPropertyNamed "prepareConstraints keep only one duplicated mustSpendScriptOutput constraints"
        "mustSpendScriptOutputDuplicate" mustSpendScriptOutputDuplicate
    , testPropertyNamed "prepareConstraints keep the constraitnt with a reference script on duplicate"
        "mustSpendScriptOutputKeepTheOneWithAReferenceScript"
        mustSpendScriptOutputKeepTheOneWithAReferenceScript
    , testPropertyNamed "prepareConstraints fails if the same utxo is spent with different redeemers"
        "mustSpendScriptOutputFailsWithDifferentRedeemers"
        mustSpendScriptOutputFailsWithDifferentRedeemers
    , testPropertyNamed "prepareConstraints fails if the same utxo is spent with a different referenceScript"
        "mustSpendScriptOutputFailsWithDifferentReferenceScript"
        mustSpendScriptOutputFailsWithDifferentReferenceScript
    , testPropertyNamed "ValidityInterval roundTrip" "intervalRoundTripProp" intervalRoundTripProp
    ]

intervalRoundTripProp :: Property
intervalRoundTripProp = Hedgehog.property $ do
    interval <- Hedgehog.forAll intervalGen
    Hedgehog.tripping interval Interval.toPlutusInterval (Just . Interval.fromPlutusInterval)

intervalGen :: Hedgehog.MonadGen m => m (Interval.ValidityInterval Slot.Slot)
intervalGen = do
    someSlot <- (Slot.Slot . toInteger) <$> Gen.int (Range.linear 0 100)
    someSlot2 <- (Slot.Slot . toInteger) <$> Gen.int (Range.linear 0 100)
    Gen.element [ Interval.ValidityInterval Nothing Nothing
                , Interval.ValidityInterval Nothing (Just someSlot)
                , Interval.ValidityInterval (Just someSlot) Nothing
                , Interval.ValidityInterval (Just someSlot) (Just $ someSlot + someSlot2)
                ]

-- | Reduce one of the elements in a 'Value' by one.
--   Returns 'Nothing' if the value contains no positive
--   elements.
reduceByOne :: Hedgehog.MonadGen m => Value -> m (Maybe Value)
reduceByOne (Value.Value value) = do
    let flat = do
            (currency, rest) <- AMap.toList value
            (tokenName, amount) <- AMap.toList rest
            guard (amount > 0)
            pure (currency, tokenName, pred amount)
    if null flat
        then pure Nothing
        else (\(cur, tok, amt) -> Just $ Value.singleton cur tok amt) <$> Gen.element flat

-- | A 'Value' with non-negative entries taken from a relatively
--   small pool of MPS hashes and token names.
nonNegativeValue :: Hedgehog.MonadGen m => m Value
nonNegativeValue =
    let tokenNames = ["a", "b", "c", "d"]
    in Gen.someTokenValue
        <$> Gen.element tokenNames
        <*> Gen.integral (Range.linear 0 10000)

-- | Check that 'missingValueSpent' is the smallest value needed to
--   meet the requirements.
missingValueSpentProp :: Property
missingValueSpentProp = property $ do
    let valueSpentBalances = Gen.choice
            [ OC.provided <$> nonNegativeValue
            , OC.required <$> nonNegativeValue
            ]
        empty = OC.ValueSpentBalances mempty mempty
    balances <- foldl (<>) empty <$> forAll (Gen.list (Range.linear 0 10000) valueSpentBalances)
    let missing = OC.missingValueSpent balances
        actual = OC.vbsProvided balances
    Hedgehog.annotateShow missing
    Hedgehog.annotateShow actual
    Hedgehog.assert (OC.vbsRequired balances `Value.leq` (actual <> missing))

    -- To make sure that this is indeed the smallest value meeting
    -- the requirements, we reduce it by one and check that the property doesn't
    -- hold anymore.
    smaller <- forAll (reduceByOne missing)
    forM_ smaller $ \smaller' ->
        Hedgehog.assert (not (OC.vbsRequired balances `Value.leq` (actual <> smaller')))

-- | The 'mustPayToPubKeyAddress' should be able to set the stake public key hash to some value.
mustPayToPubKeyAddressStakePubKeyNotNothingProp :: Property
mustPayToPubKeyAddressStakePubKeyNotNothingProp = property $ do
    pkh <- forAll $ Ledger.paymentPubKeyHash <$> Gen.element Gen.knownPaymentPublicKeys
    let sc = stakePubKeyHashCredential $ StakePubKeyHash $ Ledger.pubKeyHash $ Ledger.PubKey "00000000000000000000000000000000000000000000000000000000"
        txE = Constraints.mkTx @Void def mempty (Constraints.mustPayToPubKeyAddress pkh sc (Ada.toValue Ledger.minAdaTxOutEstimated))
    case txE of
        Left err -> do
            Hedgehog.annotateShow err
            Hedgehog.failure
        Right utx -> do
            let tx = fromMaybe (error "Unexpected emulator tx") (preview OC.tx utx)
                outputs = view OC.txOuts tx
                stakingCreds = mapMaybe stakePaymentPubKeyHash outputs
            Hedgehog.assert $ not $ null stakingCreds
            forM_ stakingCreds ((===) sc)

-- | The 'mustPayToOtherScriptAddressWithDatumHash' should be able to set the stake validator hash to some value.
mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp :: Property
mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp = property $ do
    let sc = stakeValidatorHashCredential $ Ledger.StakeValidatorHash $ examplePlutusScriptAlwaysSucceedsHash WitCtxStake
        txE = Constraints.mkTxWithParams @Void def mempty (Constraints.mustPayToOtherScriptAddressWithDatumHash alwaysSucceedValidatorHash sc Ledger.unitDatum (Ada.toValue Ledger.minAdaTxOutEstimated))
    case txE of
      Left err -> do
          Hedgehog.annotateShow err
          Hedgehog.failure
      Right utx -> do
          let tx = fromMaybe (error "Unexpected emulator tx") (preview OC.tx utx)
              outputs = view OC.txOuts tx
              stakingCreds = mapMaybe stakePaymentPubKeyHash outputs
          Hedgehog.assert $ not $ null stakingCreds
          forM_ stakingCreds ((===) sc)

mustUseOutputAsCollateralProp :: Property
mustUseOutputAsCollateralProp = property $ do
    txId <- forAll genTxId
    let txOutRefForCollateral = Ledger.TxOutRef (C.fromCardanoTxId txId) 0
        txE = Constraints.mkTxWithParams @Void def mempty (Constraints.mustUseOutputAsCollateral txOutRefForCollateral)
    case txE of
        Left e -> do
            Hedgehog.annotateShow e
            Hedgehog.failure
        Right utx -> do
            let coll = view (OC.tx . OC.txInsCollateral) utx
            Hedgehog.assert $ length coll == 1
            Hedgehog.assert $ Right (head coll) == C.toCardanoTxIn txOutRefForCollateral

mustSpendPubKeyOutputDuplicate :: Property
mustSpendPubKeyOutputDuplicate = property $ do
    let con = Constraints.mustSpendPubKeyOutput @Void @Void (txOutRef 1)
    let dup = con <> con
    Hedgehog.assert $ prepFromTxConstraints dup == prepFromTxConstraints con

mustSpendScriptOutputDuplicate :: Property
mustSpendScriptOutputDuplicate = property $ do
    let con = Constraints.mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 0)
    let dup = con <> con
    Hedgehog.assert $ prepFromTxConstraints dup == prepFromTxConstraints con

mustSpendScriptOutputKeepTheOneWithAReferenceScript :: Property
mustSpendScriptOutputKeepTheOneWithAReferenceScript = property $ do
    let con = Constraints.mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 0)
    let dup = Constraints.mustSpendScriptOutput @Void @Void (txOutRef 1) (asRedeemer ()) <> con
    Hedgehog.assert $ prepFromTxConstraints dup == prepFromTxConstraints con

mustSpendScriptOutputFailsWithDifferentRedeemers :: Property
mustSpendScriptOutputFailsWithDifferentRedeemers = property $ do
    let con  = Constraints.mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 0)
    let con2 = Constraints.mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer (5 :: Integer)) (txOutRef 0)
    Hedgehog.assert $ case prepFromTxConstraints (con <> con2) of
        Left (OC.AmbiguousRedeemer _ _) -> True
        _                               -> False

mustSpendScriptOutputFailsWithDifferentReferenceScript :: Property
mustSpendScriptOutputFailsWithDifferentReferenceScript = property $ do
    let con  = Constraints.mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 0)
    let con2 = Constraints.mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 1)
    Hedgehog.assert $ case prepFromTxConstraints (con <> con2) of
        Left (OC.AmbiguousReferenceScript _ _) -> True
        _                                      -> False

prepFromTxConstraints
    :: Constraints.TxConstraints Void Void
    -> Either Constraints.MkTxError OC.SortedConstraints
prepFromTxConstraints txCons = runExcept $ flip evalStateT (OC.initialState def) $
        OC.prepareConstraints @Void (Constraints.txOwnInputs txCons) (Constraints.txOwnOutputs txCons) (Constraints.txConstraints txCons)
        `runReaderT` mempty

txOutRef :: Integer -> Ledger.TxOutRef
txOutRef = Ledger.TxOutRef (Ledger.TxId "")

stakePaymentPubKeyHash :: TxOut -> Maybe StakingCredential
stakePaymentPubKeyHash (TxOut (C.TxOut addr _ _ _)) = Ledger.cardanoStakingCredential addr

data UnitTest
instance Scripts.ValidatorTypes UnitTest

alwaysSucceedValidator :: Scripts.TypedValidator UnitTest
alwaysSucceedValidator = Scripts.mkTypedValidator
    $$(PlutusTx.compile [|| \_ _ _ -> True ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

alwaysSucceedValidatorHash :: Ledger.ValidatorHash
alwaysSucceedValidatorHash = Scripts.validatorHash alwaysSucceedValidator
