{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Main(main) where

import Control.Lens (toListOf, view)
import Control.Monad (forM_, guard, replicateM, void)
import Control.Monad.Except (runExcept)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask, runReaderT)
import Data.ByteString qualified as BS
import Data.Default (def)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Language.Haskell.TH.Syntax
import Ledger qualified (DatumFromQuery (DatumInBody), DecoratedTxOut (ScriptDecoratedTxOut), inputs, paymentPubKeyHash,
                         scriptTxInputs, toTxOut, txInputRef, unitDatum, unitRedeemer)
import Ledger.Ada qualified as Ada
import Ledger.Address (StakePubKeyHash (StakePubKeyHash), addressStakingCredential, xprvToPaymentPubKeyHash,
                       xprvToStakePubKeyHash)
import Ledger.Constraints (MkTxError, mustSpendPubKeyOutput, mustSpendScriptOutput, mustSpendScriptOutputWithReference)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain (prepareConstraints)
import Ledger.Constraints.OffChain qualified as OC
import Ledger.Constraints.OnChain.V2 qualified as ConstraintsV2
import Ledger.Credential (Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash))
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Ledger.Generators qualified as Gen
import Ledger.Index qualified as Ledger
import Ledger.Params (Params (pNetworkId))
import Ledger.Scripts (WitCtx (WitCtxStake), examplePlutusScriptAlwaysSucceedsHash)
import Ledger.Test (asRedeemer)
import Ledger.Tx (Tx (txCollateralInputs, txOutputs), TxOut (TxOut), txOutAddress)
import Ledger.Tx.CardanoAPI (toCardanoTxOut, toCardanoTxOutDatumHash)
import Ledger.Value (CurrencySymbol, Value (Value))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V2.Generators qualified as Gen
import Plutus.Script.Utils.V2.Scripts qualified as Ledger
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Scripts
import Plutus.V2.Ledger.Api qualified as Ledger
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import PlutusTx.Prelude qualified as Pl
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all tests"
    [ testPropertyNamed "missing value spent"
        "missingValueSpentProp" missingValueSpentProp
    , testPropertyNamed "mustPayToPubKeyAddress should create output addresses with stake pub key hash"
        "mustPayToPubKeyAddressStakePubKeyNotNothingProp"
        mustPayToPubKeyAddressStakePubKeyNotNothingProp
    , testPropertyNamed "mustPayToOtherScriptAddress should create output addresses with stake validator hash"
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
    [x,y] <- Hedgehog.forAllWith (const "A known key") $ take 2 <$> Gen.shuffle Gen.knownXPrvs
    let pkh = xprvToPaymentPubKeyHash x
        skh = xprvToStakePubKeyHash y
        txE = Constraints.mkTxWithParams @Void def mempty (Constraints.mustPayToPubKeyAddress pkh skh (Ada.toValue Ledger.minAdaTxOut))
    case txE of
      Left err -> do
          Hedgehog.annotateShow err
          Hedgehog.failure
      Right utx -> do
          let outputs = txOutputs (view OC.tx utx)
          let stakingCreds = mapMaybe stakePaymentPubKeyHash outputs
          Hedgehog.assert $ not $ null stakingCreds
          forM_ stakingCreds ((===) skh)
  where
      stakePaymentPubKeyHash :: TxOut -> Maybe StakePubKeyHash
      stakePaymentPubKeyHash tx = do
          stakeCred <- addressStakingCredential (txOutAddress tx)
          case stakeCred of
            StakingHash (PubKeyCredential pkh) -> Just $ StakePubKeyHash pkh
            _                                  -> Nothing

-- | The 'mustPayToOtherScriptAddress' should be able to set the stake validator hash to some value.
mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp :: Property
mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp = property $ do
    pkh <- forAll $ Ledger.paymentPubKeyHash <$> Gen.element Gen.knownPaymentPublicKeys
    let svh = Ledger.StakeValidatorHash $ examplePlutusScriptAlwaysSucceedsHash WitCtxStake
        txE = Constraints.mkTxWithParams @Void def mempty (Constraints.mustPayToOtherScriptAddress alwaysSucceedValidatorHash svh Ledger.unitDatum (Ada.toValue Ledger.minAdaTxOut))
    case txE of
      Left err -> do
          Hedgehog.annotateShow err
          Hedgehog.failure
      Right utx -> do
          let outputs = txOutputs (view OC.tx utx)
          let stakingCreds = mapMaybe stakeValidatorHash outputs
          Hedgehog.assert $ not $ null stakingCreds
          forM_ stakingCreds ((===) svh)
  where
      stakeValidatorHash :: TxOut -> Maybe Ledger.StakeValidatorHash
      stakeValidatorHash tx = do
          stakeCred <- addressStakingCredential (txOutAddress tx)
          case stakeCred of
            StakingHash (ScriptCredential (Ledger.ValidatorHash svh)) -> Just $ Ledger.StakeValidatorHash svh
            _                                                         -> Nothing

mustUseOutputAsCollateralProp :: Property
mustUseOutputAsCollateralProp = property $ do
    pkh <- forAll $ Ledger.paymentPubKeyHash <$> Gen.element Gen.knownPaymentPublicKeys
    let txOutRef = Ledger.TxOutRef (Ledger.TxId "123") 0
        txE = Constraints.mkTxWithParams @Void def mempty (Constraints.mustUseOutputAsCollateral txOutRef)
    case txE of
        Left e -> do
            Hedgehog.annotateShow e
            Hedgehog.failure
        Right utx -> do
            let coll = txCollateralInputs (view OC.tx utx)
            Hedgehog.assert $ length coll == 1
            Hedgehog.assert $ Ledger.txInputRef (head coll) == txOutRef

mustSpendScriptOutputDuplicate :: Property
mustSpendScriptOutputDuplicate = property $ do
    let con = mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 0)
    let dup = con <> con
    Hedgehog.assert $ prepFromTxConstraints dup == prepFromTxConstraints con

mustSpendPubKeyOutputDuplicate :: Property
mustSpendPubKeyOutputDuplicate = property $ do
    let con = mustSpendPubKeyOutput @Void @Void (txOutRef 1)
    let dup = con <> con
    Hedgehog.assert $ prepFromTxConstraints dup == prepFromTxConstraints con

mustSpendScriptOutputKeepTheOneWithAReferenceScript :: Property
mustSpendScriptOutputKeepTheOneWithAReferenceScript = property $ do
    let con = mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 0)
    let dup = mustSpendScriptOutput @Void @Void (txOutRef 1) (asRedeemer ()) <> con
    Hedgehog.assert $ prepFromTxConstraints dup == prepFromTxConstraints con

mustSpendScriptOutputFailsWithDifferentRedeemers :: Property
mustSpendScriptOutputFailsWithDifferentRedeemers = property $ do
    let con  = mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 0)
    let con2 = mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer (5 :: Integer)) (txOutRef 0)
    Hedgehog.assert $ case prepFromTxConstraints (con <> con2) of
        Left (OC.AmbiguousRedeemer _ _) -> True
        _                               -> False

mustSpendScriptOutputFailsWithDifferentReferenceScript :: Property
mustSpendScriptOutputFailsWithDifferentReferenceScript = property $ do
    let con  = mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 0)
    let con2 = mustSpendScriptOutputWithReference @Void @Void (txOutRef 1) (asRedeemer ()) (txOutRef 1)
    Hedgehog.assert $ case prepFromTxConstraints (con <> con2) of
        Left (OC.AmbiguousReferenceScript _ _) -> True
        _                                      -> False

prepFromTxConstraints
    :: Constraints.TxConstraints Void Void
    -> Either MkTxError ([Constraints.TxConstraint], [Constraints.TxConstraint])
prepFromTxConstraints txCons = runExcept $
        prepareConstraints @Void (Constraints.txOwnOutputs txCons) (Constraints.txConstraints txCons)
        `runReaderT` mempty

txOut0 :: Ledger.DecoratedTxOut
txOut0 =
    Ledger.ScriptDecoratedTxOut
        alwaysSucceedValidatorHash
        Nothing
        mempty
        (Ledger.datumHash Ledger.unitDatum, Ledger.DatumInBody Ledger.unitDatum)
        Nothing
        Nothing

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

validator1 :: Scripts.TypedValidator UnitTest
validator1 = Scripts.mkTypedValidator
    ($$(PlutusTx.compile [|| \vh _ _ -> ConstraintsV2.checkScriptContext @() @() (constraints1 vh) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode alwaysSucceedValidatorHash)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorHash1 :: Ledger.ValidatorHash
validatorHash1 = Scripts.validatorHash validator1

txOut1 :: Ledger.DecoratedTxOut
txOut1 =
    Ledger.ScriptDecoratedTxOut
        validatorHash1
        Nothing
        mempty
        (Ledger.datumHash Ledger.unitDatum, Ledger.DatumInBody Ledger.unitDatum)
        Nothing
        Nothing

txOutRef :: Integer -> Ledger.TxOutRef
txOutRef = Ledger.TxOutRef (Ledger.TxId "")

utxo1 :: Map.Map Ledger.TxOutRef Ledger.DecoratedTxOut
utxo1 = Map.fromList [(txOutRef 0, txOut0), (txOutRef 1, txOut1)]

{-# INLINABLE constraints1 #-}
constraints1 :: Ledger.ValidatorHash -> Constraints.TxConstraints () ()
constraints1 vh =
    Constraints.mustSpendScriptOutputWithMatchingDatumAndValue
        vh
        (Pl.== Ledger.unitDatum)
        (Pl.const True)
        Ledger.unitRedeemer
    <> Constraints.mustSpendScriptOutput (txOutRef 1) Ledger.unitRedeemer

lookups1 :: Constraints.ScriptLookups UnitTest
lookups1
    = Constraints.unspentOutputs utxo1
    <> Constraints.otherScript (Scripts.vValidatorScript alwaysSucceedValidator)
    <> Constraints.otherScript (Scripts.vValidatorScript validator1)
