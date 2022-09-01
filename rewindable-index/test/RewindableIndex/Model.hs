module RewindableIndex.Model
  ( Index(..)
  -- * Constructors
  , new
  , insert
  , rewind
  -- * Projections
  , IndexView(..)
  , view
  , getFunction
  , getHistory
  , getNotifications
  -- * Helpers
  , insertL
  -- * Testing
  , ObservedBuilder (..)
  , GrammarBuilder (..)
  -- * Conversions
  , Conversion(..)
  , conversion
  -- * Properties
  , prop_observeNew
  , prop_rewindDepth
  , prop_sizeLEDepth
  , prop_insertRewindInverse
  , prop_observeInsert
  , prop_observeNotifications
  , prop_insertRewindNotifications
  ) where

import Control.Monad (replicateM)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import Data.Functor.Identity (Identity, runIdentity)
import Data.List (foldl', isPrefixOf, scanl')
import Data.Maybe (fromJust, isJust, isNothing, mapMaybe, maybeToList)
import Test.QuickCheck.Monadic (assert, monadic, run)
import Test.Tasty.QuickCheck (Arbitrary (arbitrary, shrink), CoArbitrary, Fun, Gen, Property, applyFun2,
                              arbitrarySizedIntegral, chooseInt, cover, forAll, frequency, resize, shrinkNothing, sized,
                              (==>))


{- | Laws
  Constructors: new, insert, rewind
  Observations: view [depth, view, size], getHistory, getFunction

  Laws derived by constructors/observations:

    view (new f d a) =
      | d > 0     = IndexView [d a 0]
      | otherwise = Nothing
    getHistory (new f d a) = [a]
    view (insertL bs (new f d a)) = IndexView [d (foldl' f a bs) (max (length bs) d)]
    getHistory (insertL bs (new f d a)) = take d bs
    rewind n (new f d a) = Nothing
    view <$> (rewind n (insertL bs (new f d a))) =
      | n <= length bs = IndexView [d a' ((max (length bs) d) - n)]
        where a' = head $ drop n $ scanl' f a bs
      | otherwise = nothing
    getHistory (rewind n (insertL bs (new f d a))) = drop n $ scanl' f a bs

  Laws derived by interplay of constructors:

    d >= length bs =>
      obs (rewind (length bs) (insertL bs (new f d a))) = obs (new f d a)
    rewind _ (new f d a) = Nothing
    depth >= size
-}

data Index a e n = New (a -> e -> (a, Maybe n)) Int a
                 | Insert e (Index a e n)
                 | Rewind Int (Index a e n)

instance (Show a, Show e) => Show (Index a e n) where
  show (New _ depth acc) = "New <f> " <> show depth <> " " <> show acc
  show (Insert b ix)     = "Insert " <> show b <> " (" <> show ix <> ")"
  show (Rewind n ix)     = "Rewind " <> show n <> " (" <> show ix <> ")"

newtype GrammarBuilder a e n = GrammarBuilder (Index a e n)
  deriving (Show)

newtype ObservedBuilder a e n = ObservedBuilder (Index a e n)
  deriving (Show)

data IndexView a = IndexView
  { ixDepth :: Int
  , ixView  :: a
  , ixSize  :: Int -- ^ Size represents the stored history elements
  } deriving (Show, Ord, Eq, Typeable, Generic)

-- | Constructors

new :: (a -> e -> (a, Maybe n)) -> Int -> a -> Index a e n
new = New

insert :: e -> Index a e n -> Index a e n
insert = Insert

rewind :: Int -> Index a e n -> Index a e n
rewind = Rewind

-- | Observations

view :: Index a e n -> Maybe (IndexView a)
view (New _ depth initial) =
  if depth > 0
  then pure $ IndexView { ixDepth = depth
                        , ixView  = initial
                        , ixSize  = 1
                        }
  else Nothing
view (Insert e ix) = do
  let f = getFunction ix
  v <- view ix
  pure $ v { ixView = fst $ f (ixView v) e
           , ixSize = min (ixDepth v) (ixSize v + 1)
           }
view (Rewind n ix) = do
  h <- getHistory ix
  v <- view ix
  if length h > n
  then Just $ v { ixSize = ixSize v - n
                , ixView = h !! max 0 n
                }
  else Nothing

getNotifications :: Index a e n -> Maybe [n]
getNotifications New{} = Just []
getNotifications (Insert e ix) = do
  let f = getFunction ix
  v  <- view ix
  ns <- getNotifications ix
  pure $ maybeToList (snd (f (ixView v) e)) ++ ns
getNotifications (Rewind _ ix) = getNotifications ix

-- | Internal

getFunction :: Index a e n -> (a -> e -> (a, Maybe n))
getFunction (New f _ _)   = f
getFunction (Insert _ ix) = getFunction ix
getFunction (Rewind _ ix) = getFunction ix

getHistory :: Index a e n -> Maybe [a]
getHistory (New _ _ i) = Just [i]
getHistory (Insert e ix) = do
  let f = getFunction ix
  h <- getHistory ix
  v <- view ix
  pure $ fst (f (head h) e) : take (ixDepth v - 1) h
getHistory (Rewind n ix) = do
  h <- getHistory ix
  if length h > n
  then Just $ drop n h
  else Nothing

-- | Utility

insertL :: [e] -> Index a e n -> Index a e n
insertL es ix = foldl' (flip insert) ix es

-- | Generalised testing interface
data Conversion m a e n = Conversion
  { cView          :: Index a e n -> m (Maybe (IndexView a))
  , cHistory       :: Index a e n -> m (Maybe [a])
  , cNotifications :: Index a e n -> m [n]
  , cMonadic       :: m Property -> Property
  }

-- | Conversion for the model indexer (lifting of pure functions)
conversion :: Conversion Identity a e n
conversion = Conversion
  { cView          = pure . view
  , cHistory       = pure . getHistory
  , cNotifications = pure . fromJust . getNotifications
  , cMonadic       = runIdentity
  }

-- | Generic properties
prop_observeNew
  :: forall e a n m. (Eq a, Monad m)
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
  => Conversion m a e n
  -> ObservedBuilder a e n
  -> Property
prop_sizeLEDepth c (ObservedBuilder ix) =
  monadic (cMonadic c) $ do
    (Just v) <- run $ cView c ix
    assert $ ixSize v <= ixDepth v

-- | Relation between Rewind and Inverse
prop_insertRewindInverse
  :: forall e a n m. (Monad m, Show e, Arbitrary e, Eq a)
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
  :: forall e a n m. (Monad m, Eq a)
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
  :: forall e a n m. (Monad m, Eq n)
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
  :: forall e a n m. (Monad m, Show e, Arbitrary e, Eq n)
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

-- | QuickCheck

instance ( CoArbitrary a
         , CoArbitrary e
         , Arbitrary a
         , Arbitrary e
         , Arbitrary n ) => Arbitrary (ObservedBuilder a e n) where
  arbitrary = sized $ \n -> do
    depth <- frequency [ (05, pure 1)                       -- overfill
                       , (40, chooseInt (2, n + 2))         -- about filled
                       , (40, chooseInt (n + 2, n * 2 + 2)) -- not filled
                       ]
    overflow <- chooseInt (depth, depth * 2)
    acc <- arbitrary
    fn  <- arbitrary
    bs    <- frequency [ (05, pure [])                       -- empty
                       , (50, arbitrary)                     -- randomized
                       , (30, replicateM (depth `div` 2)     -- half filled
                                         arbitrary)
                       , (10, replicateM overflow arbitrary) -- overfilled
                       , (05, replicateM depth    arbitrary) -- exact
                       ]
    -- Construction can only fail due to NonPositive depth
    -- Tested with prop_hfNewReturns...
    let ix = new fn depth acc
    pure . ObservedBuilder $ insertL bs ix
  shrink = shrinkNothing

instance ( CoArbitrary a
         , CoArbitrary e
         , Arbitrary a
         , Arbitrary e
         , Arbitrary n ) => Arbitrary (Index a e n) where
  -- Use the ObservedIndex instance as a generator for Indexes
  arbitrary = do
    (ObservedBuilder ix) <- arbitrary
    pure ix
  shrink = shrinkNothing

instance ( CoArbitrary a
         , CoArbitrary e
         , Arbitrary a
         , Arbitrary e
         , Arbitrary n ) => Arbitrary (GrammarBuilder a e n) where
  arbitrary = sized $ \n -> do
    depth <- frequency [ (05, pure 1)                       -- overfill
                       , (40, chooseInt (2, n + 2))         -- about filled
                       , (40, chooseInt (n + 2, n * 2 + 2)) -- not filled
                       ]
    f     <- arbitrary
    acc   <- arbitrary
    let ix = new f depth acc
    complexity <- arbitrarySizedIntegral
    generateGrammarIndex complexity ix
  shrink = shrinkNothing

generateGrammarIndex :: Arbitrary e => Int -> Index a e n -> Gen (GrammarBuilder a e n)
generateGrammarIndex 0 ix = pure $ GrammarBuilder ix
generateGrammarIndex n ix = do
  b      <- arbitrary
  -- This should be correct by construction (the incorrect cases are not very
  -- interesting).
  d      <- chooseInt (1, ixDepth . fromJust $ view ix)
  nextIx <- frequency [ (80, pure            $ insert b ix)
                      , (20, pure            $ rewind d ix)
                      ]
  generateGrammarIndex (n - 1) nextIx

instance Arbitrary a => Arbitrary (IndexView a) where
  arbitrary = sized $ \n -> do
    depth <- chooseInt (2, n)
    size  <- chooseInt (0, depth)
    view'  <- arbitrary
    pure IndexView { ixDepth = depth
                   , ixSize  = size
                   , ixView  = view'
                   }
  shrink = shrinkNothing
