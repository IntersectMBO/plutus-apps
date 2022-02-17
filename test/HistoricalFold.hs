{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HistoricalFold
  ( hfSignature
  ) where

import           Control.Monad      (replicateM)
import           Data.Map           (Map)
import           Data.Maybe         (fromJust)
import           Data.List.NonEmpty (NonEmpty (..), toList, (<|))
import           GHC.Generics

import           QuickSpec
import           Test.QuickCheck    (Arbitrary (arbitrary), CoArbitrary, Gen,
                                     choose, chooseInt, frequency, listOf,
                                     sized)

import Index.HistoricalFold (HistoricalFold)
import Index.HistoricalFold qualified as HF

import qualified Debug.Trace        as Debug

-- QuickCheck infrastructure
instance ( CoArbitrary a
         , CoArbitrary b
         , Arbitrary a
         , Arbitrary b ) => Arbitrary (HistoricalFold a b) where
  arbitrary = sized $ \n -> do
    -- What happens when n is 0 or 1?
    depth <- frequency [ (05, pure 1)
                       , (40, chooseInt (2, n + 2))
                       , (40, chooseInt (n + 2, n * 2 + 2))
                       ]
    overflow <-  chooseInt (depth, depth * 2)
    acc   <- arbitrary
    fn    <- arbitrary
    bs    <- frequency [ (05, pure [])                       -- empty
                       , (50, arbitrary)                     -- randomized
                       , (30, replicateM (depth `div` 2)     -- half filled
                                         arbitrary)
                       , (10, replicateM overflow arbitrary) -- overfilled
                       , (05, replicateM depth    arbitrary) -- exact
                       ]
    -- Construction can only fail due to NonPositive depth
    -- Tested with prop_hfNewReturns...
    let newHf = fromJust $ HF.new fn depth acc
    pure $ HF.insertL bs newHf

-- QuickSpec infrastructure
data HFObs a = HFObs
  { hfoDepth       :: Int
  , hfoAccumulator :: NonEmpty a
  } deriving (Eq, Ord, Typeable)

newtype HFIns a b = HFIns [b]
    deriving (Eq, Ord, Typeable)

instance Arbitrary b => Arbitrary (HFIns a b) where
  arbitrary = HFIns <$> listOf arbitrary

instance ( Ord a
         , Arbitrary a
         , Arbitrary b
         , CoArbitrary a
         , CoArbitrary b ) => Observe (HFIns a b) (HFObs a) (HistoricalFold a b) where
  observe (HFIns bs) hf =
    let newHF = HF.insertL bs hf
     in HFObs { hfoDepth = HF.hfDepth newHF
              , hfoAccumulator = HF.hfAccumulator newHF
              }

hfSignature :: [Sig]
hfSignature =
  [ monoObserve @(HistoricalFold Int String)
  , con "new" (HF.new :: (Int -> String -> Int) -> Int -> Int -> Maybe (HistoricalFold Int String))
  , con "insert" (HF.insert :: String -> HistoricalFold Int String -> HistoricalFold Int String)
  , con "view" (HF.view :: HistoricalFold Int String -> Int)
  , con "historyLength" (HF.historyLength :: HistoricalFold Int String -> Int)
  , con "rewind" (HF.rewind :: Int -> HistoricalFold Int String -> Maybe (HistoricalFold Int String))
  , withMaxTermSize 4
  ]
