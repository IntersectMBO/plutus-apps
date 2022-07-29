module Plutus.HystericalScreams.Spec.Index where

import Data.Functor.Identity (Identity, runIdentity)
import Data.List (foldl', isPrefixOf, scanl')
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe)
import Test.QuickCheck.Monadic
import Test.Tasty.QuickCheck

import Plutus.HystericalScreams.Index

data Conversion m a e n = Conversion
  { cView          :: Index a e n -> m (Maybe (IndexView a))
  , cHistory       :: Index a e n -> m (Maybe [a])
  , cNotifications :: Index a e n -> m [n]
  , cMonadic       :: m Property -> Property
  }

conversion :: Conversion Identity a e n
conversion = Conversion
  { cView          = pure . view
  , cHistory       = pure . getHistory
  , cNotifications = pure . fromJust . getNotifications
  , cMonadic       = runIdentity
  }

prop_observeNew
  :: forall e a n m. (Eq a, Monad m)
  => Show a
  => Conversion m a e n
  -> Fun (a, e) (a, Maybe n)
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
  :: forall e a n m. (Monad m)
  => Conversion m a e n
  -> ObservedBuilder a e n
  -> Property
prop_rewindDepth c (ObservedBuilder ix) =
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
    monadic (cMonadic c) $ do
      mv <- run $ cView c (rewind depth ix)
      if depth >= ixSize v
        then assert $ isNothing mv
        else assert $ isJust    mv

-- | Property that validates the HF data structure.
prop_sizeLEDepth
  :: forall e a n m. (Monad m)
  => Show a
  => Conversion m a e n
  -> ObservedBuilder a e n
  -> Property
prop_sizeLEDepth c (ObservedBuilder ix) =
  monadic (cMonadic c) $ do
    (Just v) <- run $ cView c ix
    assert $ ixSize v <= ixDepth v

-- | Relation between Rewind and Inverse
prop_insertRewindInverse
  :: forall e a n m. (Monad m, Show e, Show a, Arbitrary e, Eq a)
  => Conversion m a e n
  -> ObservedBuilder a e n
  -> Property
prop_insertRewindInverse c (ObservedBuilder ix) =
  let v = fromJust $ view ix
  -- rewind does not make sense for lesser depths.
   in ixDepth v >= 2 ==>
  -- if the history is not fully re-written, then we can get a common
  -- prefix after the insert/rewind play. We need input which is less
  -- than `hfDepth hf`
  forAll (resize (ixDepth v - 1) arbitrary) $
  \bs -> monadic (cMonadic c) $ do
    let ix' = rewind (length bs) $ insertL bs ix
    Just v' <- run $ cView c ix
    h  <- take (ixDepth v' - length bs) . fromJust <$> run (cHistory c ix)
    -- h  <- fromJust <$> run (cHistory c ix)
    h' <- fromJust <$> run (cHistory c ix')
    assert $ h == h'

-- | Generally this would not be a good property since it is very coupled
--   to the implementation, but it will be useful when trying to certify that
--   another implmentation is confirming.
prop_observeInsert
  :: forall e a n m. (Monad m, Eq a, Show a)
  => Conversion m a e n
  -> ObservedBuilder a e n
  -> [e]
  -> Property
prop_observeInsert c (ObservedBuilder ix) es =
  monadic (cMonadic c) $ do
    Just v  <- run $ cView c ix
    let ix' = insertL es ix
    Just v' <- run $ cView c ix'
    let v'' = IndexView { ixDepth = ixDepth v
                        , ixSize  = min (ixDepth v) (length es + ixSize v)
                        , ixView  = foldl' ((fst .) . getFunction ix) (ixView v) es
                        }
    assert $ v' == v''

-- | Notifications are accumulated as the folding function runs.
prop_observeNotifications
  :: forall e a n m. (Monad m, Show n, Show e, Eq n)
  => Conversion m a e n
  -> ObservedBuilder a e n
  -> [e]
  -> Property
prop_observeNotifications c (ObservedBuilder ix) es =
  monadic (cMonadic c) $ do
    Just v  <- run $ cView c ix
    let f        = getFunction ix
        ix'      = insertL es ix
        ns'      = mapMaybe snd $ scanl' (\(a, _) e -> f a e) (ixView v, Nothing) es
    ns <- run $ cNotifications c ix'
    assert $ reverse ns' `isPrefixOf` ns

-- | Relation between Rewind and Inverse
prop_insertRewindNotifications
  :: forall e a n m. (Monad m, Show e, Show a, Arbitrary e, Show n, Eq n)
  => Conversion m a e n
  -> ObservedBuilder a e n
  -> Property
prop_insertRewindNotifications c (ObservedBuilder ix) =
  let v = fromJust $ view ix
  -- rewind does not make sense for lesser depths.
   in ixDepth v >= 2 ==>
  -- if the history is not fully re-written, then we can get a common
  -- prefix after the insert/rewind play. We need input which is less
  -- than `hfDepth hf`
  forAll (resize (ixDepth v - 1) arbitrary) $
  \bs -> monadic (cMonadic c) $ do
    let ix'  = insertL bs ix
        ix'' = rewind (length bs) ix'
    ns  <- run $ cNotifications c ix'
    ns' <- run $ cNotifications c ix''
    assert $ ns == ns'
