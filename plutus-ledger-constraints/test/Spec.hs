{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Control.Monad (forM_, guard, replicateM, void)
import Hedgehog (Property, forAll, property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger.Constraints.OffChain qualified as OC
import Ledger.Value (CurrencySymbol, Value (Value))
import Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AMap
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all tests" [
    testProperty "missing value spent" missingValueSpentProp
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
    let mpsHashes = ["ffff", "dddd", "cccc", "eeee", "1010"]
        tokenNames = ["a", "b", "c", "d"]
    in Value.singleton
        <$> Gen.element mpsHashes
        <*> Gen.element tokenNames
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
