import           QuickSpec
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.List             (foldl')
import           Data.Maybe            (fromJust, isJust, isNothing)

import           Index

tests :: TestTree
tests = testGroup "Index" [hfProperties]

hfProperties :: TestTree
hfProperties = testGroup "Basic model"
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
  :: ObservedIndex a b
  -> Property
prop_rewindWithDepth (ObservedIndex ix) =
  let v = view ix in
  ixDepth v >= 2 ==>
  forAll (frequency [ (20, chooseInt (ixDepth v, ixDepth v * 2))
                    , (30, chooseInt (ixSize v + 1, ixDepth v - 1))
                    , (50, chooseInt (1, ixSize v)) ]) $
  \depth ->
    cover 15 (depth >  ixDepth v) "Depth is larger than max depth." $
    cover 15 (depth <= ixDepth v && depth > ixSize v)
          "Depth is lower than max but there is not enough data."   $
    cover 40 (depth <= ixDepth v && depth <= ixSize v)
          "Depth is properly set."                                  $
    let newIx = rewind depth ix
    in  if depth > (ixDepth v - 1) || (depth > ixSize v)
        then property $ isNothing newIx
        else property $ isJust    newIx

-- | Property that validates the HF data structure.
prop_historyLengthLEDepth
  :: ObservedIndex a b
  -> Property
prop_historyLengthLEDepth (ObservedIndex ix) =
  let v = view ix
   in property $ ixSize v <= ixDepth v

-- | Relation between Rewind and Inverse
prop_InsertRewindInverse
  :: (Show a, Show b, Arbitrary b, Eq a)
  => ObservedIndex a b
  -> Property
prop_InsertRewindInverse (ObservedIndex ix) =
  let v = view ix
  -- rewind does not make sense for lesser depths.
   in ixDepth v >= 2 ==>
  -- if the history is not fully re-written, then we can get a common
  -- prefix after the insert/rewind play. We need input which is less
  -- than `hfDepth hf`
  forAll (resize (ixDepth v - 1) arbitrary) $
  \bs ->
      let ix'   = rewind (length bs) $ insertL bs ix
          v'    = view (fromJust ix')
       in property $ isJust ix' && fromJust ix' `matches` ix

-- | Generally this would not be a good property since it is very coupled
--   to the implementation, but it will be useful when trying to certify that
--   another implmentation is confirming.
prop_InsertFolds
  :: (Eq a, Show a)
  => ObservedIndex a b
  -> [b]
  -> Property
prop_InsertFolds (ObservedIndex ix) bs =
  ixView (view (insertL bs ix)) ===
    foldl' (getFunction ix) (ixView $ view ix) bs

prop_InsertHistoryLength
  :: ObservedIndex a b
  -> b
  -> Property
prop_InsertHistoryLength (ObservedIndex ix) b =
  let v             = view ix
      initialLength = ixSize v
      finalLength   = ixSize . view $ insert b ix
   in cover 10 (initialLength == ixDepth v) "Overflowing" $
      cover 30 (initialLength <  ixDepth v)  "Not filled" $
      if initialLength == ixDepth v
      then finalLength === initialLength
      else finalLength === initialLength + 1

main :: IO ()
main = do
  quickSpec ixSignature
  defaultMain tests
