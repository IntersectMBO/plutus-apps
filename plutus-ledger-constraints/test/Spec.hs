{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Main(main) where

import Control.Lens (toListOf, view)
import Control.Monad (forM_, guard, replicateM, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ask)
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
import Ledger qualified (ChainIndexTxOut (ScriptChainIndexTxOut), inputs, paymentPubKeyHash, scriptTxInputs, toTxOut,
                         txInputRef, unitDatum, unitRedeemer)
import Ledger.Ada qualified as Ada
import Ledger.Address (StakePubKeyHash (StakePubKeyHash), addressStakingCredential, xprvToPaymentPubKeyHash,
                       xprvToStakePubKeyHash)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain qualified as OC
import Ledger.Constraints.OnChain.V2 qualified as ConstraintsV2
import Ledger.Credential (Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash))
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Ledger.Generators qualified as Gen
import Ledger.Index qualified as Ledger
import Ledger.Params (Params (pNetworkId))
import Ledger.Scripts (WitCtx (WitCtxStake), examplePlutusScriptAlwaysSucceedsHash)
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
    [ testPropertyNamed "missing value spent" "missingValueSpentProp" missingValueSpentProp
    , testPropertyNamed "mustPayToPubKeyAddress should create output addresses with stake pub key hash" "mustPayToPubKeyAddressStakePubKeyNotNothingProp" mustPayToPubKeyAddressStakePubKeyNotNothingProp
    , testPropertyNamed "mustPayToOtherScriptAddress should create output addresses with stake validator hash" "mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp" mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp
    , testPropertyNamed "mustUseOutputAsCollateral should add a collateral input" "mustUseOutputAsCollateralProp" mustUseOutputAsCollateralProp
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
        txE = Constraints.mkTx @Void mempty (Constraints.mustPayToPubKeyAddress pkh skh (Ada.toValue Ledger.minAdaTxOut))
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
        txE = Constraints.mkTx @Void mempty (Constraints.mustPayToOtherScriptAddress alwaysSucceedValidatorHash svh Ledger.unitDatum (Ada.toValue Ledger.minAdaTxOut))
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
        txE = Constraints.mkTx @Void mempty (Constraints.mustUseOutputAsCollateral txOutRef)
    case txE of
        Left e -> do
            Hedgehog.annotateShow e
            Hedgehog.failure
        Right utx -> do
            let coll = txCollateralInputs (view OC.tx utx)
            Hedgehog.assert $ length coll == 1
            Hedgehog.assert $ Ledger.txInputRef (head coll) == txOutRef

txOut0 :: Ledger.ChainIndexTxOut
txOut0 =
    Ledger.ScriptChainIndexTxOut
        (Ledger.Address (ScriptCredential alwaysSucceedValidatorHash) Nothing)
        mempty
        (Ledger.datumHash Ledger.unitDatum, Just Ledger.unitDatum)
        Nothing
        (alwaysSucceedValidatorHash, Nothing)

txOutRef0 :: Ledger.TxOutRef
txOutRef0 = Ledger.TxOutRef (Ledger.TxId "") 0

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

txOut1 :: Ledger.ChainIndexTxOut
txOut1 =
    Ledger.ScriptChainIndexTxOut
        (Ledger.Address (ScriptCredential validatorHash1) Nothing)
        mempty
        (Ledger.datumHash Ledger.unitDatum, Just Ledger.unitDatum)
        Nothing
        (validatorHash1, Nothing)

txOutRef1 :: Ledger.TxOutRef
txOutRef1 = Ledger.TxOutRef (Ledger.TxId "") 1

utxo1 :: Map.Map Ledger.TxOutRef Ledger.ChainIndexTxOut
utxo1 = Map.fromList [(txOutRef0, txOut0), (txOutRef1, txOut1)]

{-# INLINABLE constraints1 #-}
constraints1 :: Ledger.ValidatorHash -> Constraints.TxConstraints () ()
constraints1 vh =
    Constraints.mustSpendScriptOutputWithMatchingDatumAndValue
        vh
        (Pl.== Ledger.unitDatum)
        (Pl.const True)
        Ledger.unitRedeemer
    <> Constraints.mustSpendScriptOutput txOutRef1 Ledger.unitRedeemer

lookups1 :: Constraints.ScriptLookups UnitTest
lookups1
    = Constraints.unspentOutputs utxo1
    <> Constraints.otherScript (Scripts.vValidatorScript alwaysSucceedValidator)
    <> Constraints.otherScript (Scripts.vValidatorScript validator1)
