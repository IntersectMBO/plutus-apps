import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.Maybe            (isJust, isNothing)

import           Model

tests :: TestTree
tests = testGroup "Utxo index" [hfProperties]

hfProperties :: TestTree
hfProperties = testGroup "Historical fold"
  [ testProperty "New: Negative or zero depth" $ prop_hfNewReturnsNothing @Int @Int
  , testProperty "New: Positive depth" $ prop_hfNewReturnsSomething @Int @Int
  , testProperty "History length is always smaller than the max depth" $
      prop_historyLengthLEDepth @Int @Int
  , testProperty "Rewind: Connection with `hfDepth`" $
      prop_rewindWithDepth @Int @Int
  ]

prop_hfNewReturnsNothing
  :: Fun (a, b) a
  -> NonPositive Int
  -> a
  -> Bool
prop_hfNewReturnsNothing
  fn2 
  (NonPositive depth)
  acc = isNothing $ new (applyFun2 fn2) depth acc

prop_hfNewReturnsSomething
  :: Fun (a, b) a
  -> Positive Int
  -> a
  -> Bool
prop_hfNewReturnsSomething
  fn2
  (Positive depth)
  acc = isJust $ new (applyFun2 fn2) depth acc

prop_rewindWithDepth
  :: HistoricalFold a b
  -> Property
prop_rewindWithDepth hf =
  forAll (frequency [ (20, chooseInt (hfDepth hf + 1, (hfDepth hf + 1) * 2))
                    , (30, chooseInt (historyLength hf + 1, hfDepth hf))
                    , (50, chooseInt (1, historyLength hf)) ]) $
  \depth ->
    cover 15 (depth > hfDepth hf) "Depth is larger than max depth." $
    cover 15 (depth <= hfDepth hf && depth > historyLength hf)
          "Depth is lower than max but there is not enough data."   $
    cover 40 (depth <= hfDepth hf && depth <= historyLength hf)
          "Depth is properly set."                                  $
    let newHF = rewind depth hf
    in  if depth > hfDepth hf || (depth > historyLength hf)
        then property $ isNothing newHF
        else property $ isJust    newHF

prop_historyLengthLEDepth
  :: HistoricalFold a b
  -> Property
prop_historyLengthLEDepth hf =
  property $ historyLength hf <= hfDepth hf

main :: IO ()
main = defaultMain tests
