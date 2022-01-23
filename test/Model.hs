module Model ( -- * Model data
               HistoricalFold
               -- * Model functionality
             , new
             , insert
             , view
             , historyLength
             , rewind
               -- * Helpers
             , insertL
               -- * QuickCheck instrumentation
             ) where

import           Data.List          (foldl')
import           Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe         (fromJust)

import           Test.QuickCheck    (Arbitrary (arbitrary), CoArbitrary, Gen,
                                     choose, listOf, sized)

-- | Model of a historical (we can go backwards) fold over a data set.

data HistoricalFold a b = HistoricalFold
  { hfFunction    :: a -> b -> a
  , hfDepth       :: Int
  , hfAccumulator :: NonEmpty a
  }

-- | Operations over the historical folds.

new :: (a -> b -> a) -> Int -> a -> Maybe (HistoricalFold a b)
new fn depth acc
  | depth <= 0 = Nothing
  | otherwise = Just $
    HistoricalFold { hfFunction = fn
                   , hfDepth = depth
                   , hfAccumulator = acc :| []
                   }

insert :: b -> HistoricalFold a b -> HistoricalFold a b
insert v hf@(HistoricalFold fn depth acc@(hacc :| _)) =
  -- TODO: forall hf v. historyLength (insert v hf) > 0
  hf { hfAccumulator = NE.fromList $ NE.take depth $
                       fn hacc v <| acc }

insertL :: [b] -> HistoricalFold a b -> HistoricalFold a b
insertL bs hf = foldl' (flip insert) hf bs

view :: HistoricalFold a b -> a
view (HistoricalFold _ _ (hacc :| _)) = hacc

historyLength :: HistoricalFold a b -> Int
historyLength (HistoricalFold _ _ acc) = NE.length acc

rewind :: Int -> HistoricalFold a b -> Maybe (HistoricalFold a b)
-- TODO: Check all Nothing/Just cases.
rewind depth hf
  | hfDepth hf < depth = Nothing
  | historyLength hf < depth = Nothing
  | otherwise = Just $ hf { hfAccumulator = NE.fromList
                                          $ NE.drop depth (hfAccumulator hf) }

-- QuickCheck infrastructure
instance ( CoArbitrary a
         , CoArbitrary b
         , Arbitrary a
         , Arbitrary b ) => Arbitrary (HistoricalFold a b) where
  arbitrary = sized $ \n -> do
    depth <- choose (1, n * 2)
    acc   <- arbitrary
    fn    <- arbitrary
    -- depth > 1 => the result of new is Just..
    pure $ fromJust $ new fn depth acc

