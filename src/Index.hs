{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}

module Index
  ( Index
  -- * Constructors
  , new
  , insert
  , rewind
  -- * Projections
  , IndexView(..)
  , view
  , getFunction
  , getHistory
  -- * Helpers
  , insertL
  , matches
  -- * Testing
  , ObservedIndex (..)
  , GrammarIndex (..)
  , ixSignature
  ) where

import           Control.Monad   (replicateM)
import           Data.Foldable   (foldl')
import Data.List (isInfixOf)
import           Data.Maybe      (fromJust)
import           Test.QuickCheck (Arbitrary (..), CoArbitrary (..), Gen,
                                  arbitrarySizedIntegral, choose, chooseInt,
                                  frequency, listOf, sized)
import QuickSpec
import GHC.Generics

data Index a b = New (a -> b -> a) Int a
               | Insert b (Index a b)
               | Rewind Int (Index a b)

instance (Show a, Show b) => Show (Index a b) where
  show (New f depth acc) = "New <f> " <> show depth <> " " <> show acc
  show (Insert b ix) = "Insert " <> show b <> " (" <> show ix <> ")"
  show (Rewind n ix) = "Rewind " <> show n <> " (" <> show ix <> ")"

newtype GrammarIndex a b = GrammarIndex (Index a b)
  deriving (Show)

newtype ObservedIndex a b = ObservedIndex (Index a b)
  deriving (Show)

data IndexView a = IndexView
  { ixDepth :: Int
  , ixView  :: a
  , ixSize  :: Int
  } deriving (Show, Ord, Eq, Typeable, Generic)

-- | Constructors

new :: (a -> b -> a) -> Int -> a -> Maybe (Index a b)
new f depth initial
  | depth > 0 = Just $ New f depth initial
  | otherwise = Nothing

insert :: b -> Index a b -> Index a b
insert = Insert

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
        , ixSize = min (ixDepth v) (ixSize v + 1)
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
      v = view ix
  in f (head h) b : take (ixDepth v - 1) h
getHistory (Rewind n ix) = drop n $ getHistory ix

-- | Utility

matches :: Eq a => Index a b -> Index a b -> Bool
matches hl hr =
  let hlAccumulator = getHistory hl
      hrAccumulator = getHistory hr
  in     hlAccumulator `isInfixOf` hrAccumulator
      || hrAccumulator `isInfixOf` hlAccumulator
      || hrAccumulator     ==      hlAccumulator

insertL :: [b] -> Index a b -> Index a b
insertL bs ix = foldl' (flip insert) ix bs
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
         , Arbitrary b ) => Arbitrary (Index a b) where
  -- Use the ObservedIndex instance as a generator for Indexes
  arbitrary = do
    (ObservedIndex ix) <- arbitrary
    pure ix

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

instance Arbitrary a => Arbitrary (IndexView a) where
  arbitrary = sized $ \n -> do
    depth <- chooseInt (2, n)
    size  <- chooseInt (0, depth)
    view  <- arbitrary
    pure IndexView { ixDepth = depth
                   , ixSize  = size
                   , ixView  = view
                   }

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

ixSignature :: [Sig]
ixSignature =
  [ monoObserve @(Index Int String)
  , monoObserve @(Index Int Int)
  , monoObserve @(Index Int [Int])
  , monoObserve @(Maybe (Index Int String))
  , monoObserve @(Maybe (Index Int Int))
  , monoObserve @(Maybe (Index Int [Int]))
  , mono @(IndexView Int)
  , con "new" (new :: (Int -> String -> Int) -> Int -> Int -> Maybe (Index Int String))
  , con "insert" (insert :: String -> Index Int String -> Index Int String)
  , con "view" (view :: Index Int String -> IndexView Int)
  , con "rewind" (rewind :: Int -> Index Int String -> Maybe (Index Int String))
  , con "getHistory" (getHistory :: Index Int String -> [Int])
  , withMaxTermSize 6
  ]
