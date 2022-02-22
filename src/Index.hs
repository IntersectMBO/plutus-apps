module Index
  ( Index
  -- * Constructors
  , new
  , insert
  , rewind
  -- * Projections
  , IndexView(..)
  , view
  -- * Helpers
  , insertL
  ) where

import           Control.Monad   (replicateM)
import           Data.Foldable   (foldl')
import           Data.Maybe      (fromJust)
import           Test.QuickCheck (Arbitrary (..), CoArbitrary (..), Gen,
                                  arbitrarySizedIntegral, choose, chooseInt,
                                  frequency, listOf, sized)
import QuickSpec

data Index a b where
  New    :: (a -> b -> a) -> Int -> a -> Index a b
  Insert :: b -> Index a b -> Index a b
  Rewind :: Int -> Index a b -> Index a b

newtype GrammarIndex a b = GrammarIndex (Index a b)

newtype ObservedIndex a b = ObservedIndex (Index a b)

data IndexView a = IndexView
  { ixDepth :: Int
  , ixView  :: a
  , ixSize  :: Int
  } deriving (Show, Ord, Eq)

-- | Constructors

new :: (a -> b -> a) -> Int -> a -> Maybe (Index a b)
new f depth initial
  | depth > 0 = Just $ New f depth initial
  | otherwise = Nothing

insert :: b -> Index a b -> Index a b
insert = Insert

insertL bs ix = foldl' (flip insert) ix bs

rewind :: Int -> Index a b -> Maybe (Index a b)
rewind n ix
  | ixDepth (view ix) <= n = Nothing
  | ixSize  (view ix) <  n = Nothing
  | otherwise = Just $ Rewind n ix

-- | Observations

view :: Index a b -> IndexView a
view (New f depth initial) =
  IndexView { ixDepth = depth
            , ixView  = initial
            , ixSize  = 0
            }
view (Insert b ix) =
  let f = getFunction ix
      v = view ix
   in v { ixView = f (ixView v) b
        , ixSize = ixSize v + 1
        }
view (Rewind n ix) =
  let h = getHistory ix
      v = view ix
  in v { ixSize = ixSize v - n
       , ixView = head $ drop n h
       }

-- | Internal

getFunction :: Index a b -> (a -> b -> a)
getFunction (New f _ _)   = f
getFunction (Insert _ ix) = getFunction ix
getFunction (Rewind _ ix) = getFunction ix

getHistory :: Index a b -> [a]
getHistory (New _ _ i) = [i]
getHistory (Insert b ix) =
  let f = getFunction ix
      h = getHistory  ix
  in f (head h) b : h
getHistory (Rewind n ix) = drop n $ getHistory ix

-- | QuickCheck

instance ( CoArbitrary a
         , CoArbitrary b
         , Arbitrary a
         , Arbitrary b ) => Arbitrary (ObservedIndex a b) where
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
    let newHf = fromJust $ new fn depth acc
    pure . ObservedIndex $ insertL bs newHf

instance ( CoArbitrary a
         , CoArbitrary b
         , Arbitrary a
         , Arbitrary b ) => Arbitrary (GrammarIndex a b) where
  arbitrary = sized $ \n -> do
    depth <- frequency [ (05, pure 1)                       -- overfill
                       , (40, chooseInt (2, n + 2))         -- about filled
                       , (40, chooseInt (n + 2, n * 2 + 2)) -- not filled
                       ]
    f     <- arbitrary
    acc   <- arbitrary
    let ix = fromJust $ new f depth acc
    complexity <- arbitrarySizedIntegral
    generateGrammarIndex complexity ix

generateGrammarIndex :: Arbitrary b => Int -> Index a b -> Gen (GrammarIndex a b)
generateGrammarIndex 0 ix = pure $ GrammarIndex ix
generateGrammarIndex n ix = do
  b      <- arbitrary
  n      <- chooseInt (1, ixDepth $ view ix)
  nextIx <- frequency [ (80, pure            $ insert b ix)
                      , (20, pure . fromJust $ rewind n ix)
                      ]
  generateGrammarIndex (n - 1) nextIx

-- | QuickSpec

newtype IxEvents b = IxEvents [b]
  deriving (Eq, Ord, Typeable)

instance Arbitrary b => Arbitrary (IxEvents b) where
  arbitrary = IxEvents <$> listOf arbitrary

instance ( Ord a
         , Arbitrary a
         , Arbitrary b
         , CoArbitrary a
         , CoArbitrary b) => Observe (IxEvents b) (IndexView a) (Index a b) where
  observe (IxEvents es) ix = view $ insertL es ix
