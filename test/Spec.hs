import           QuickSpec
import           Test.Tasty
import           Test.Tasty.QuickCheck

import           Data.List             (foldl', isInfixOf)
import           Data.Maybe            (fromJust, isJust, isNothing)

import           Index

import qualified Debug.Trace           as Debug

tests :: TestTree
tests = testGroup "Index" [ixProperties]

ixProperties :: TestTree
ixProperties = testGroup "Basic model"
  [ testProperty "New: Positive or non-positive depth" $
      withMaxSuccess 10000 $ prop_newReturn @Int @Int
  , testProperty "History length is always smaller than the max depth" $
      withMaxSuccess 10000 $ prop_sizeLEDepth @Int @Int
  , testProperty "Rewind: Connection with `hfDepth`" $
      withMaxSuccess 10000 $ prop_rewindWithDepth @Int @Int
  , testProperty "Relationship between Insert/Rewind" $
      withMaxSuccess 10000 $ prop_insertRewindInverse @Int @Int
  , testProperty "Insert is folding the structure" $
      withMaxSuccess 10000 $ prop_insertFolds @Int @Int
  , testProperty "Insert is increasing the length unless overflowing" $
      withMaxSuccess 10000 $ prop_insertHistoryLength @Int @Int
  ]

{- | Properties of the `new` operation.
       view (new f d a) =
         | d > 0     = IndexView [d a 0]
         | otherwise = Nothing
       getHistory (new f d a) = []
-}
prop_newReturn
  :: Eq a
  => Fun (a, b) a
  -> a
  -> Property
prop_newReturn f acc =
  forAll (frequency [ (10, pure 0)
                    , (50, chooseInt (-100, 0))
                    , (50, chooseInt (1, 100)) ]) $
  \depth ->
    cover 30 (depth <  0) "Negative depth" $
    cover 30 (depth >= 0) "Non negative depth" $
    let newIx = new (applyFun2 f) depth acc
    in  property $ if depth <= 0
                   then isNothing newIx
                   else view (fromJust newIx) == IndexView { ixDepth = depth
                                                           , ixView  = acc
                                                           , ixSize  = 0
                                                           }

-- | Properties of the connection between rewind and depth
--   Note: Cannot rewind if (hfDepth hf == 1)
prop_rewindWithDepth
  :: ObservedBuilder a b
  -> Property
prop_rewindWithDepth (ObservedBuilder ix) =
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
prop_sizeLEDepth
  :: ObservedBuilder a b
  -> Property
prop_sizeLEDepth (ObservedBuilder ix) =
  let v = view ix
   in property $ ixSize v <= ixDepth v

-- | Relation between Rewind and Inverse
prop_insertRewindInverse
  :: (Show a, Show b, Arbitrary b, Eq a)
  => ObservedBuilder a b
  -> Property
prop_insertRewindInverse (ObservedBuilder ix) =
  let v = view ix
  -- rewind does not make sense for lesser depths.
   in ixDepth v >= 2 ==>
  -- if the history is not fully re-written, then we can get a common
  -- prefix after the insert/rewind play. We need input which is less
  -- than `hfDepth hf`
  forAll (resize (ixDepth v - 1) arbitrary) $
  \bs ->
      let mix' = rewind (length bs) $ insertL bs ix
          -- This should always be Just.. because of the resize of `bs`
          ix'  = fromJust mix'
          v'   = view ix'
       in property $ ix `matches` ix'
                  -- && getHistory ix == getHistory ix'

-- | Generally this would not be a good property since it is very coupled
--   to the implementation, but it will be useful when trying to certify that
--   another implmentation is confirming.
prop_insertFolds
  :: (Eq a, Show a)
  => ObservedBuilder a b
  -> [b]
  -> Property
prop_insertFolds (ObservedBuilder ix) bs =
  ixView (view (insertL bs ix)) ===
    foldl' (getFunction ix) (ixView $ view ix) bs

prop_insertHistoryLength
  :: ObservedBuilder a b
  -> b
  -> Property
prop_insertHistoryLength (ObservedBuilder ix) b =
  let v             = view ix
      initialLength = ixSize v
      finalLength   = ixSize . view $ insert b ix
   in cover 10 (initialLength == ixDepth v) "Overflowing" $
      cover 30 (initialLength <  ixDepth v)  "Not filled" $
      if initialLength == ixDepth v
      then finalLength === initialLength
      else finalLength === initialLength + 1

matches :: Eq a => Index a e -> Index a e -> Bool
matches hl hr =
  let hlAccumulator = getHistory hl
      hrAccumulator = getHistory hr
  in     hlAccumulator `isInfixOf` hrAccumulator
      || hrAccumulator `isInfixOf` hlAccumulator
      || hrAccumulator     ==      hlAccumulator

main :: IO ()
main = do
  -- quickSpec ixSignature
  defaultMain tests
