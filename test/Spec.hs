import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.List             (foldl')
import           Data.Maybe            (fromJust, isJust, isNothing)

import           Model

import qualified Debug.Trace           as Debug

tests :: TestTree
tests = testGroup "Utxo index" [hfProperties]

hfProperties :: TestTree
hfProperties = testGroup "Historical fold"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ prop_hfNewReturn @Int @Int
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ prop_historyLengthLEDepth @Int @Int
  , testProperty "Rewind: Connection with `hfDepth`" $
      withMaxSuccess 10000 $ prop_rewindWithDepth @Int @Int
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 10000 $ prop_InsertRewindInverse @Int @Int
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 10000 $ prop_InsertFolds @Int @Int
  , testProperty "Insert is increasing the length unless overflowing" $
      withMaxSuccess 10000 $ prop_InsertHistoryLength @Int @Int
  ]

-- | Properties of the `new` operation.
prop_hfNewReturn
  :: Fun (a, b) a
  -> a
  -> Property
prop_hfNewReturn f acc =
  forAll (frequency [ (10, pure 0)
                    , (50, chooseInt (-100, 0))
                    , (50, chooseInt (1, 100)) ]) $
  \depth ->
    cover 30 (depth <  0) "Negative depth" $
    cover 30 (depth >= 0) "Non negative depth" $
    let newHF = new (applyFun2 f) depth acc
    in  property $ if depth <= 0
                   then isNothing newHF
                   else isJust    newHF

-- | Properties of the connection between rewind and depth
--   Note: Cannot rewind if (hfDepth hf == 1)
prop_rewindWithDepth
  :: HistoricalFold a b
  -> Property
prop_rewindWithDepth hf =
  hfDepth hf >= 2 ==>
  forAll (frequency [ (20, chooseInt (hfDepth hf, hfDepth hf * 2))
                    , (30, chooseInt (historyLength hf + 1, hfDepth hf - 1))
                    , (50, chooseInt (1, historyLength hf)) ]) $
  \depth ->
    cover 15 (depth > hfDepth hf) "Depth is larger than max depth." $
    cover 15 (depth <= hfDepth hf && depth > historyLength hf)
          "Depth is lower than max but there is not enough data."   $
    cover 40 (depth <= hfDepth hf && depth <= historyLength hf)
          "Depth is properly set."                                  $
    let newHF = rewind depth hf
    in  if depth > (hfDepth hf - 1) || (depth > historyLength hf)
        then property $ isNothing newHF
        else property $ isJust    newHF

-- | Property that validates the HF data structure.
prop_historyLengthLEDepth
  :: HistoricalFold a b
  -> Property
prop_historyLengthLEDepth hf =
  property $ historyLength hf <= hfDepth hf

-- | Relation between Rewind and Inverse
prop_InsertRewindInverse
  :: (Show a, Show b, Arbitrary b, Eq a)
  => HistoricalFold a b
  -> Property
prop_InsertRewindInverse hf =
  -- rewind does not make sense for lesser depths.
  hfDepth hf >= 2 ==>
  -- if the history is not fully re-written, then we can get a common
  -- prefix after the insert/rewind play. We need input which is less
  -- than `hfDepth hf`
  forAll (resize (hfDepth hf - 1) arbitrary) $
  \bs ->
      let hf'   = rewind (length bs) $ insertL bs hf
       in property $ isJust hf' && fromJust hf' `matchesHistory` hf

-- | Generally this would not be a good property since it is very coupled
--   to the implementation, but it will be useful when trying to certify that
--   another implmentation is confirming.
prop_InsertFolds
  :: (Eq a, Show a)
  => HistoricalFold a b
  -> [b]
  -> Property
prop_InsertFolds hf bs =
  view (insertL bs hf) ===
    foldl' (hfFunction hf) (view hf) bs

prop_InsertHistoryLength
  :: HistoricalFold a b
  -> b
  -> Property
prop_InsertHistoryLength hf b =
  let initialLength = historyLength hf
      finalLength   = historyLength (insert b hf)
   in cover 10 (initialLength == hfDepth hf) "Overflowing" $
      cover 30 (initialLength < hfDepth hf)  "Not filled" $
      if initialLength == hfDepth hf
      then finalLength === initialLength
      else finalLength === initialLength + 1

main :: IO ()
main = defaultMain tests
