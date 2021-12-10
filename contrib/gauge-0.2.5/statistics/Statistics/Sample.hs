{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Sample
-- Copyright : (c) 2008 Don Stewart, 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Commonly used sample statistics, also known as descriptive
-- statistics.

module Statistics.Sample
    (
    -- * Statistics of location
      mean

    -- ** Two-pass functions (numerically robust)
    -- $robust
    , variance
    , varianceUnbiased
    , stdDev

    -- * References
    -- $references
    ) where

import Statistics.Sample.Internal (robustSumVar, sum)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

-- Operator ^ will be overriden
import Prelude hiding ((^), sum)

-- | /O(n)/ Arithmetic mean.  This uses Kahan-Babuška-Neumaier
-- summation, so is more accurate than 'welfordMean' unless the input
-- values are very large.
mean :: (G.Vector v Double) => v Double -> Double
mean xs = sum xs / fromIntegral (G.length xs)
{-# SPECIALIZE mean :: U.Vector Double -> Double #-}
{-# SPECIALIZE mean :: V.Vector Double -> Double #-}

-- $variance
--
-- The variance&#8212;and hence the standard deviation&#8212;of a
-- sample of fewer than two elements are both defined to be zero.

-- $robust
--
-- These functions use the compensated summation algorithm of Chan et
-- al. for numerical robustness, but require two passes over the
-- sample data as a result.
--
-- Because of the need for two passes, these functions are /not/
-- subject to stream fusion.

-- | Maximum likelihood estimate of a sample's variance.  Also known
-- as the population variance, where the denominator is /n/.
variance :: (G.Vector v Double) => v Double -> Double
variance samp
    | n > 1     = robustSumVar (mean samp) samp / fromIntegral n
    | otherwise = 0
    where
      n = G.length samp
{-# SPECIALIZE variance :: U.Vector Double -> Double #-}
{-# SPECIALIZE variance :: V.Vector Double -> Double #-}


-- | Unbiased estimate of a sample's variance.  Also known as the
-- sample variance, where the denominator is /n/-1.
varianceUnbiased :: (G.Vector v Double) => v Double -> Double
varianceUnbiased samp
    | n > 1     = robustSumVar (mean samp) samp / fromIntegral (n-1)
    | otherwise = 0
    where
      n = G.length samp
{-# SPECIALIZE varianceUnbiased :: U.Vector Double -> Double #-}
{-# SPECIALIZE varianceUnbiased :: V.Vector Double -> Double #-}

-- | Standard deviation.  This is simply the square root of the
-- unbiased estimate of the variance.
stdDev :: (G.Vector v Double) => v Double -> Double
stdDev = sqrt . varianceUnbiased
{-# SPECIALIZE stdDev :: U.Vector Double -> Double #-}
{-# SPECIALIZE stdDev :: V.Vector Double -> Double #-}

-- $cancellation
--
-- The functions prefixed with the name @fast@ below perform a single
-- pass over the sample data using Knuth's algorithm. They usually
-- work well, but see below for caveats. These functions are subject
-- to array fusion.
--
-- /Note/: in cases where most sample data is close to the sample's
-- mean, Knuth's algorithm gives inaccurate results due to
-- catastrophic cancellation.

-- $references
--
-- * Chan, T. F.; Golub, G.H.; LeVeque, R.J. (1979) Updating formulae
--   and a pairwise algorithm for computing sample
--   variances. Technical Report STAN-CS-79-773, Department of
--   Computer Science, Stanford
--   University. <ftp://reports.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf>
--
-- * Knuth, D.E. (1998) The art of computer programming, volume 2:
--   seminumerical algorithms, 3rd ed., p. 232.
--
-- * Welford, B.P. (1962) Note on a method for calculating corrected
--   sums of squares and products. /Technometrics/
--   4(3):419&#8211;420. <http://www.jstor.org/stable/1266577>
--
-- * West, D.H.D. (1979) Updating mean and variance estimates: an
--   improved method. /Communications of the ACM/
--   22(9):532&#8211;535. <http://doi.acm.org/10.1145/359146.359153>
