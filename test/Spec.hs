import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.Maybe            (isJust, isNothing)

import           Model

tests :: TestTree
tests = testGroup "Utxo index" [hfProperties]

hfProperties :: TestTree
hfProperties = testGroup "Historical fold"
  [ testProperty "New: Positive or non-positive depth" $ prop_hfNewReturn @Int @Int
  , testProperty "History length is always smaller than the max depth" $
      prop_historyLengthLEDepth @Int @Int
  , testProperty "Rewind: Connection with `hfDepth`" $
      prop_rewindWithDepth @Int @Int
  ]

prop_hfNewReturn
  :: Fun (a, b) a
  -> a
  -> Property
prop_hfNewReturn f acc =
  forAll (frequency [ (50, chooseInt (-100, 0))
                    , (50, chooseInt (1, 100)) ]) $
  \depth ->
    cover 30 (depth <  0) "Negative depth" $
    cover 30 (depth >= 0) "Non negative depth" $
    let newHF = new (applyFun2 f) depth acc
    in  property $ if depth < 0
                   then isNothing newHF
                   else isJust    newHF

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
