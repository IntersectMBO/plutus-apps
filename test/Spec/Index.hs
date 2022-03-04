module Spec.Index where

import           QuickSpec
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Data.Maybe            (fromJust, isJust, isNothing)
import           Data.List             (foldl')

import Index

{- | Properties of the `new` operation.
       view (new f d a) =
         | d > 0     = IndexView [d a 0]
         | otherwise = Nothing
       getHistory (new f d a) = []
-}
prop_observeNew
  :: (Eq a, Show a)
  => Fun (a, b) a
  -> a
  -> Property
prop_observeNew f acc =
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
                                                           , ixSize  = 1
                                                           }
                     && getHistory (fromJust newIx) == [acc]

-- | Properties of the connection between rewind and depth
--   Note: Cannot rewind if (ixDepth ix == 1)
prop_rewindDepth
  :: ObservedBuilder a b
  -> Property
prop_rewindDepth (ObservedBuilder ix) =
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
          ix' = fromJust mix'
          h   = take (ixDepth v - length bs) $ getHistory ix
          h'  = getHistory ix'
       in property $ h == h'

-- | Generally this would not be a good property since it is very coupled
--   to the implementation, but it will be useful when trying to certify that
--   another implmentation is confirming.
prop_observeInsert
  :: (Eq a, Show a)
  => ObservedBuilder a b
  -> [b]
  -> Property
prop_observeInsert (ObservedBuilder ix) bs =
  let v = view ix
   in view (insertL bs ix) ===
        IndexView { ixDepth = ixDepth v
                  , ixSize  = min (ixDepth v) (length bs + ixSize v)
                  , ixView  = foldl' (getFunction ix) (ixView $ view ix) bs
                  }

prop_insertSize
  :: ObservedBuilder a b
  -> b
  -> Property
prop_insertSize (ObservedBuilder ix) b =
  let v             = view ix
      initialLength = ixSize v
      finalLength   = ixSize . view $ insert b ix
   in cover 10 (initialLength == ixDepth v) "Overflowing" $
      cover 30 (initialLength <  ixDepth v)  "Not filled" $
      if initialLength == ixDepth v
      then finalLength === initialLength
      else finalLength === initialLength + 1

