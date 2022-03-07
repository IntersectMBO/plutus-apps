module Spec.Index where

import           QuickSpec
import           Test.Tasty
import           Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic
import           Data.Maybe            (fromJust, isJust, isNothing)
import           Data.List             (foldl')
import Data.Functor.Identity (Identity, runIdentity)

import Index

import qualified Debug.Trace as Debug

data Conversion m a e = Conversion
  { cView    :: Index a e -> m (Maybe (IndexView a))
  , cHistory :: Index a e -> m (Maybe [a])
  , cMonadic :: m Property -> Property
  }

conversion :: Conversion Identity a e
conversion = Conversion
  { cView = pure . view
  , cHistory = pure . getHistory
  , cMonadic = runIdentity
  }

prop_observeNew
  :: forall e a m. (Eq a, Monad m)
  => Conversion m a e
  -> Fun (a, e) a
  -> a
  -> Property
prop_observeNew c f a =
  forAll (frequency [ (10, pure 0)
                    , (50, chooseInt (-100, 0))
                    , (50, chooseInt (1, 100)) ]) $
  \depth ->
    cover 30 (depth <  0) "Negative depth" $
    cover 30 (depth >= 0) "Non-negative depth" $
    let ix = new (applyFun2 f) depth a
    in  monadic (cMonadic c) $ do
      if depth <= 0
      then do
        mv <- run $ cView c ix
        assert $ isNothing mv
      else do
        v <- run $ cView c ix
        h <- run $ cHistory c ix
        assert $ v == pure (IndexView { ixDepth = depth
                                      , ixView  = a
                                      , ixSize  = 1
                                      })
              && h == Just [a]

-- | Properties of the connection between rewind and depth
--   Note: Cannot rewind if (ixDepth ix == 1)
prop_rewindDepth
  :: Show a => ObservedBuilder a b
  -> Property
prop_rewindDepth (ObservedBuilder ix) =
  let v = fromJust $ view ix in
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
    let mv' = view $ rewind depth ix
    in  if depth >= ixSize v
        then property $ isNothing mv'
        else property $ isJust    mv'

-- | Property that validates the HF data structure.
prop_sizeLEDepth
  :: ObservedBuilder a b
  -> Property
prop_sizeLEDepth (ObservedBuilder ix) =
  let v = fromJust $ view ix
   in property $ ixSize v <= ixDepth v

-- | Relation between Rewind and Inverse
prop_insertRewindInverse
  :: (Show a, Show b, Arbitrary b, Eq a)
  => ObservedBuilder a b
  -> Property
prop_insertRewindInverse (ObservedBuilder ix) =
  let v = fromJust $ view ix
  -- rewind does not make sense for lesser depths.
   in ixDepth v >= 2 ==>
  -- if the history is not fully re-written, then we can get a common
  -- prefix after the insert/rewind play. We need input which is less
  -- than `hfDepth hf`
  forAll (resize (ixDepth v - 1) arbitrary) $
  \bs ->
      let ix' = rewind (length bs) $ insertL bs ix
          -- This should always be Just.. because of the resize of `bs`
          h   = take (ixDepth v - length bs) $ fromJust $ getHistory ix
          h'  = fromJust $ getHistory ix'
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
  let v = fromJust $ view ix
   in view (insertL bs ix) ===
        Just (IndexView { ixDepth = ixDepth v
                        , ixSize  = min (ixDepth v) (length bs + ixSize v)
                        , ixView  = foldl' (getFunction ix) (ixView v) bs
                        })

prop_insertSize
  :: ObservedBuilder a b
  -> b
  -> Property
prop_insertSize (ObservedBuilder ix) b =
  let v             = fromJust $ view ix
      initialLength = ixSize v
      finalLength   = ixSize . fromJust . view $ insert b ix
   in cover 10 (initialLength == ixDepth v) "Overflowing" $
      cover 30 (initialLength <  ixDepth v)  "Not filled" $
      if initialLength == ixDepth v
      then finalLength === initialLength
      else finalLength === initialLength + 1
