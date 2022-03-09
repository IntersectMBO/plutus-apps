{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main(main) where

import Control.Monad (forM_, guard, replicateM, void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (mapMaybe)
import Data.Void (Void)
import Hedgehog (Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Address (StakePubKeyHash (StakePubKeyHash), addressStakingCredential)
import Ledger.Constraints as Constraints
import Ledger.Constraints.OffChain qualified as OC
import Ledger.Credential (Credential (PubKeyCredential), StakingCredential (StakingHash))
import Ledger.Crypto (PubKeyHash (PubKeyHash))
import Ledger.Generators qualified as Gen
import Ledger.Tx (Tx (txOutputs), TxOut (TxOut, txOutAddress))
import Ledger.Value (CurrencySymbol, Value (Value))
import Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AMap
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all tests"
    [ testProperty "missing value spent" missingValueSpentProp
    , testProperty "mustPayToPubKeyAddress should create output addresses with stake pub key hash" mustPayToPubKeyAddressStakePubKeyNotNothingProp
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
          Hedgehog.assert False
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
