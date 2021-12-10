{-# LANGUAGE FlexibleContexts, BangPatterns #-}

-- |
-- Module    : Statistics.Sample.Histogram
-- Copyright : (c) 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for computing histograms of sample data.

module Statistics.Sample.Histogram
    (
    -- * Building blocks
    histogram_
    ) where

import Numeric.MathFunctions.Constants (m_epsilon)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM

-- | /O(n)/ Compute a histogram over a data set.
--
-- Interval (bin) sizes are uniform, based on the supplied upper
-- and lower bounds.
histogram_ :: (Num b, RealFrac a, G.Vector v0 a, G.Vector v1 b) =>
              Int
           -- ^ Number of bins.  This value must be positive.  A zero
           -- or negative value will cause an error.
           -> a
           -- ^ Lower bound on interval range.  Sample data less than
           -- this will cause an error.
           -> a
           -- ^ Upper bound on interval range.  This value must not be
           -- less than the lower bound.  Sample data that falls above
           -- the upper bound will cause an error.
           -> v0 a
           -- ^ Sample data.
           -> v1 b
histogram_ numBins lo hi xs0 = G.create (GM.replicate numBins 0 >>= bin xs0)
  where
    bin xs bins = go 0
     where
       go i | i >= len = return bins
            | otherwise = do
         let x = xs `G.unsafeIndex` i
             b = truncate $ (x - lo) / d
         write' bins b . (+1) =<< GM.read bins b
         go (i+1)
       write' bs b !e = GM.write bs b e
       len = G.length xs
       d = ((hi - lo) * (1 + realToFrac m_epsilon)) / fromIntegral numBins
{-# INLINE histogram_ #-}

