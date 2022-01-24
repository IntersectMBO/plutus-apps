module Model ( -- * Model data
               HistoricalFold
               -- Should we provide access to these?
             , hfDepth
               -- * Model functionality
             , new
             , insert
             , view
             , historyLength
             , rewind
             , sameHistory
               -- * Helpers
             , insertL
               -- * QuickCheck instrumentation
             ) where

import           Control.Monad      (replicateM)
import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (fromJust)

import           Test.QuickCheck    (Arbitrary (arbitrary), CoArbitrary, Gen,
                                     choose, chooseInt, frequency, listOf,
                                     sized)

-- | Model of a historical (we can go backwards) fold over a data set.

data HistoricalFold a b = HistoricalFold
  { hfFunction    :: a -> b -> a
  , hfDepth       :: Int
  , hfAccumulator :: NonEmpty a
  }

instance (Show a, Show b) => Show (HistoricalFold a b) where
  show (HistoricalFold _ depth acc) =
    show $ "HF " <> show depth <> " " <> show acc

-- | Operations over the historical folds.
new :: (a -> b -> a) -> Int -> a -> Maybe (HistoricalFold a b)
new fn depth acc
  | depth <= 0 = Nothing
  | otherwise  = Just $
    HistoricalFold { hfFunction = fn
                   , hfDepth = depth
                   , hfAccumulator = acc :| []
                   }

insert :: b -> HistoricalFold a b -> HistoricalFold a b
insert v hf@(HistoricalFold fn depth acc@(hacc :| _)) =
  -- forall hf v. historyLength (insert v hf) > 0
  --   Take will always return something non-null.
  hf { hfAccumulator = NE.fromList $ NE.take depth $
                       fn hacc v <| acc }

insertL :: [b] -> HistoricalFold a b -> HistoricalFold a b
insertL bs hf = foldl' (flip insert) hf bs

view :: HistoricalFold a b -> a
view (HistoricalFold _ _ (hacc :| _)) = hacc

historyLength :: HistoricalFold a b -> Int
historyLength (HistoricalFold _ _ acc) = NE.length acc

rewind :: Int -> HistoricalFold a b -> Maybe (HistoricalFold a b)
rewind depth hf
  | hfDepth hf < depth = Nothing
  | historyLength hf < depth = Nothing
  | otherwise = Just $ hf { hfAccumulator = NE.fromList
                                          $ NE.drop depth (hfAccumulator hf) }

sameHistory :: Eq a => HistoricalFold a b -> HistoricalFold a b -> Bool
sameHistory hl hr =
  hfAccumulator hl == hfAccumulator hr

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
    let newHf = fromJust $ new fn depth acc
    pure $ insertL bs newHf

