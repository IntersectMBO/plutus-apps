{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}

module Model ( -- * Model data
               HistoricalFold
               -- Should we provide access to these?
             , hfDepth
             , hfAccumulator
             , hfFunction
               -- * Model functionality
             , new
             , insert
             , view
             , historyLength
             , rewind
             , matchesHistory
               -- * Helpers
             , insertL
               -- * QuickSpec
             , hfSignature
             ) where

import           Control.Monad      (replicateM)
import           Data.List          (foldl', isInfixOf)
import           Data.List.NonEmpty (NonEmpty (..), toList, (<|))
import qualified Data.List.NonEmpty as NE
import           Data.Map           (Map)
import           Data.Maybe         (fromJust)
import           GHC.Generics

import           QuickSpec
import           Test.QuickCheck    (Arbitrary (arbitrary), CoArbitrary, Gen,
                                     choose, chooseInt, frequency, listOf,
                                     sized)

import qualified Debug.Trace        as Debug

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
         , CoArbitrary b ) => Observe (HistoricalFold a b) (HFObs a) (HFIns a b) where
  observe hf (HFIns bs) =
    let newHF = insertL bs hf
     in HFObs { hfoDepth = hfDepth newHF
              , hfoAccumulator = hfAccumulator newHF
              }

hfSignature :: [Sig]
hfSignature =
  [ monoTypeObserve (Proxy :: Proxy (HFIns Int String))
  , con "new" (new :: (Int -> String -> Int) -> Int -> Int -> Maybe (HistoricalFold Int String))
  , con "insert" (insert :: String -> HistoricalFold Int String -> HistoricalFold Int String)
  , con "view" (view :: HistoricalFold Int String -> Int)
  , con "historyLength" (historyLength :: HistoricalFold Int String -> Int)
  , con "rewind" (rewind :: Int -> HistoricalFold Int String -> Maybe (HistoricalFold Int String))
  , withMaxTermSize 4
  ]
