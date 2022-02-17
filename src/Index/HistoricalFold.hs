module Index.HistoricalFold
  ( HistoricalFold
  -- * Accessors
  , hfDepth
  , hfAccumulator
  , hfFunction
  -- * Functions
  , new
  , insert
  , view
  , historyLength
  , rewind
  , matchesHistory
  -- * Helpers
  , insertL
  ) where

import           Data.List          (foldl', isInfixOf)
import           Data.List.NonEmpty (NonEmpty (..), toList, (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Typeable      (Typeable)
import           GHC.Generics

-- | Model of a historical (we can go backwards) fold over a data set.

--   Should we make `b` a monoid?
data HistoricalFold a b = HistoricalFold
  { hfFunction    :: a -> b -> a
  , hfDepth       :: Int
  , hfAccumulator :: NonEmpty a
  } deriving (Typeable)

instance (Show a, Show b) => Show (HistoricalFold a b) where
  show (HistoricalFold _ depth acc) =
    show $ "HistoricalFold { hfDepth = " <> show depth <> ", hfAccumulator = " <> show (toList acc) <> " }"

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
  | hfDepth hf <= depth = Nothing
  | historyLength hf < depth = Nothing
  | otherwise = Just $ hf { hfAccumulator = NE.fromList
                                          $ NE.drop depth (hfAccumulator hf) }

matchesHistory :: (Show a, Eq a) => HistoricalFold a b -> HistoricalFold a b -> Bool
matchesHistory hl hr =
  let hlAccumulator = toList $ hfAccumulator hl
      hrAccumulator = toList $ hfAccumulator hr
   in    hlAccumulator `isInfixOf` hrAccumulator
      || hrAccumulator `isInfixOf` hlAccumulator
      || hrAccumulator      ==     hlAccumulator


