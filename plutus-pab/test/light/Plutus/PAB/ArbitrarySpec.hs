{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Plutus.PAB.ArbitrarySpec where

import Hedgehog (MonadGen, Property, forAll, property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.QuickCheck qualified as Gen
import Hedgehog.Range qualified as Range
import Plutus.PAB.Arbitrary ()
import PlutusTx (Data (..))
import PlutusTx qualified
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Plutus.PAB.ArbitrarySpec"
  [ testPropertyNamed "arbitrary data is bounded by size parameter" "dataBoundedBySizeProp" dataBoundedBySizeProp ]

dataBoundedBySizeProp :: Property
dataBoundedBySizeProp = property $ do
  maxNodes <- forAll $ Gen.integral_ $ Range.linear 0 10000
  d <- forAll $ Gen.resize (fromInteger maxNodes) Gen.arbitrary
  Hedgehog.assert $ countDataNodes d <= maxNodes + 1

countDataNodes :: Data -> Integer
countDataNodes = \case
  Constr _ ds -> 1 + foldr ((+) . countDataNodes) 0 ds
  Map pairs   -> 1 + foldr ((+) . (\(a, b) -> countDataNodes a + countDataNodes b)) 0 pairs
  List ds     -> 1 + foldr ((+) . countDataNodes) 0 ds
  I _         -> 1
  B _         -> 1
