{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Main(main) where

import Control.Lens (toListOf)
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
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (StakePubKeyHash (StakePubKeyHash), addressStakingCredential)
import Ledger.Constraints as Constraints
import Ledger.Constraints.OffChain qualified as OC
import Ledger.Credential (Credential (PubKeyCredential, ScriptCredential), StakingCredential (StakingHash))
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Ledger.Generators qualified as Gen
import Ledger.Params ()
import Ledger.Tx (Tx (txOutputs), TxOut (TxOut, txOutAddress))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (CurrencySymbol, Value (Value))
import Ledger.Value qualified as Value
import Plutus.Script.Utils.V1.Generators qualified as Gen
import Plutus.V1.Ledger.Api qualified as Ledger
import Plutus.V1.Ledger.Scripts qualified as Ledger
import PlutusTx qualified
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import PlutusTx.Prelude qualified as Pl
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all tests"
    [ testProperty "missing value spent" missingValueSpentProp
    , testProperty "mustPayToPubKeyAddress should create output addresses with stake pub key hash" mustPayToPubKeyAddressStakePubKeyNotNothingProp
    , testProperty "mustSpendScriptOutputWithMatchingDatumAndValue" testMustSpendScriptOutputWithMatchingDatumAndValue
    , testProperty "mustPayToOtherScriptAddress should create output addresses with stake validator hash" mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp
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
    let skh = StakePubKeyHash $ PubKeyHash "00000000000000000000000000000000000000000000000000000000"
        txE = mkTx @Void mempty (Constraints.mustPayToPubKeyAddress pkh skh (Ada.toValue Ledger.minAdaTxOut))
    case txE of
      Left _ ->
          Hedgehog.failure
      Right utx -> do
          let outputs = txOutputs (OC.unBalancedTxTx utx)
          let stakingCreds = mapMaybe stakePaymentPubKeyHash outputs
          Hedgehog.assert $ not $ null stakingCreds
          forM_ stakingCreds ((===) skh)
  where
      stakePaymentPubKeyHash :: TxOut -> Maybe StakePubKeyHash
      stakePaymentPubKeyHash TxOut { txOutAddress } = do
          stakeCred <- addressStakingCredential txOutAddress
          case stakeCred of
            StakingHash (PubKeyCredential pkh) -> Just $ StakePubKeyHash pkh
            _                                  -> Nothing

-- | The 'mustPayToOtherScriptAddress' should be able to set the stake validator hash to some value.
mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp :: Property
mustPayToOtherScriptAddressStakeValidatorHashNotNothingProp = property $ do
    pkh <- forAll $ Ledger.paymentPubKeyHash <$> Gen.element Gen.knownPaymentPublicKeys
    let svh = Ledger.StakeValidatorHash "00000000000000000000000000000000000000000000000000000000"
        txE = mkTx @Void mempty (Constraints.mustPayToOtherScriptAddress alwaysSucceedValidatorHash svh Ledger.unitDatum (Ada.toValue Ledger.minAdaTxOut))
    case txE of
      Left _ ->
          Hedgehog.failure
      Right utx -> do
          let outputs = txOutputs (OC.unBalancedTxTx utx)
          let stakingCreds = mapMaybe stakeValidatorHash outputs
          Hedgehog.assert $ not $ null stakingCreds
          forM_ stakingCreds ((===) svh)
  where
      stakeValidatorHash :: TxOut -> Maybe Ledger.StakeValidatorHash
      stakeValidatorHash TxOut { txOutAddress } = do
          stakeCred <- addressStakingCredential txOutAddress
          case stakeCred of
            StakingHash (ScriptCredential (Ledger.ValidatorHash svh)) -> Just $ Ledger.StakeValidatorHash svh
            _                                                         -> Nothing

-- | Make a transaction with the given constraints and check the validity of the inputs of that transaction.
testScriptInputs
    :: ( PlutusTx.FromData (Scripts.DatumType a)
       , PlutusTx.ToData (Scripts.DatumType a)
       , PlutusTx.ToData (Scripts.RedeemerType a))
    => ScriptLookups a
    -> TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
    -> Property
testScriptInputs lookups txc = property $ do
    tx <- either (\err -> do Hedgehog.annotateShow err; Hedgehog.failure)
                 (pure . unBalancedTxTx)
                 $ mkTx lookups txc
    let valM = do
            Ledger.checkValidInputs (toListOf (Ledger.inputs . Ledger.scriptTxIns)) tx
            idx <- Ledger.vctxIndex <$> ask
            pure (Nothing, idx)
    case Ledger.runValidation valM (Ledger.ValidationCtx (Ledger.UtxoIndex (Ledger.toTxOut <$> Constraints.slTxOutputs lookups)) def) of
        ((Nothing, _), _) -> pure ()
        ((Just err, _), _) -> do
            Hedgehog.annotateShow err
            Hedgehog.failure


txOut0 :: Ledger.ChainIndexTxOut
txOut0 = Ledger.ScriptChainIndexTxOut (Ledger.Address (ScriptCredential alwaysSucceedValidatorHash) Nothing) (Left alwaysSucceedValidatorHash) (Right Ledger.unitDatum) mempty

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
    ($$(PlutusTx.compile [|| \vh _ _ -> checkScriptContext @() @() (constraints1 vh) ||])
        `PlutusTx.applyCode` PlutusTx.liftCode alwaysSucceedValidatorHash)
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

validatorHash1 :: Ledger.ValidatorHash
validatorHash1 = Scripts.validatorHash validator1

txOut1 :: Ledger.ChainIndexTxOut
txOut1 = Ledger.ScriptChainIndexTxOut (Ledger.Address (ScriptCredential validatorHash1) Nothing) (Left validatorHash1) (Right Ledger.unitDatum) mempty

txOutRef1 :: Ledger.TxOutRef
txOutRef1 = Ledger.TxOutRef (Ledger.TxId "") 1

utxo1 :: Map.Map Ledger.TxOutRef Ledger.ChainIndexTxOut
utxo1 = Map.fromList [(txOutRef0, txOut0), (txOutRef1, txOut1)]

{-# INLINABLE constraints1 #-}
constraints1 :: Ledger.ValidatorHash -> TxConstraints () ()
constraints1 vh =
    Constraints.mustSpendScriptOutputWithMatchingDatumAndValue
        vh
        (Pl.== Ledger.unitDatum)
        (Pl.const True)
        Ledger.unitRedeemer
    <> Constraints.mustSpendScriptOutput txOutRef1 Ledger.unitRedeemer

lookups1 :: ScriptLookups UnitTest
lookups1
    = Constraints.unspentOutputs utxo1
    <> Constraints.otherScript (Scripts.validatorScript alwaysSucceedValidator)
    <> Constraints.otherScript (Scripts.validatorScript validator1)

testMustSpendScriptOutputWithMatchingDatumAndValue :: Property
testMustSpendScriptOutputWithMatchingDatumAndValue = testScriptInputs lookups1 (constraints1 alwaysSucceedValidatorHash)
