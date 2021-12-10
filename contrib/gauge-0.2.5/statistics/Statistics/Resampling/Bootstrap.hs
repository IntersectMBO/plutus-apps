-- |
-- Module    : Statistics.Resampling.Bootstrap
-- Copyright : (c) 2009, 2011 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The bootstrap method for statistical inference.

module Statistics.Resampling.Bootstrap
    ( bootstrapBCA
    -- * References
    -- $references
    ) where

import           Data.Vector.Generic ((!))
import qualified Data.Vector.Unboxed as U

import Statistics.Distribution (cumulative, quantile)
import Statistics.Distribution.Normal
import Statistics.Resampling (Bootstrap(..), jackknife)
import Statistics.Sample (mean)
import Statistics.Types (Sample, CL, Estimate, ConfInt, estimateFromInterval,
                         estimateFromErr, CL, significanceLevel)
import qualified Statistics.Resampling as R


data T = {-# UNPACK #-} !Double :< {-# UNPACK #-} !Double
infixl 2 :<

-- | Bias-corrected accelerated (BCA) bootstrap. This adjusts for both
--   bias and skewness in the resampled distribution.
--
--   BCA algorithm is described in ch. 5 of Davison, Hinkley "Confidence
--   intervals" in section 5.3 "Percentile method"
bootstrapBCA
  :: CL Double       -- ^ Confidence level
  -> Sample          -- ^ Full data sample
  -> [(R.Estimator, Bootstrap U.Vector Double)]
  -- ^ Estimates obtained from resampled data and estimator used for
  --   this.
  -> [Estimate ConfInt Double]
bootstrapBCA confidenceLevel sample resampledData
  = map e resampledData
  where
    e (est, Bootstrap pt resample)
      | U.length sample == 1 || isInfinite bias =
          estimateFromErr      pt (0,0) confidenceLevel
      | otherwise =
          estimateFromInterval pt (resample ! lo, resample ! hi) confidenceLevel
      where
        -- Quantile estimates for given CL
        lo    = max (cumn a1) 0
          where a1 = bias + b1 / (1 - accel * b1)
                b1 = bias + z1
        hi    = min (cumn a2) (ni - 1)
          where a2 = bias + b2 / (1 - accel * b2)
                b2 = bias - z1
        -- Number of resamples
        ni    = U.length resample
        n     = fromIntegral ni
        -- Corrections
        z1    = quantile standard (significanceLevel confidenceLevel / 2)
        cumn  = round . (*n) . cumulative standard
        bias  = quantile standard (probN / n)
          where probN = fromIntegral . U.length . U.filter (<pt) $ resample
        accel = sumCubes / (6 * (sumSquares ** 1.5))
          where (sumSquares :< sumCubes) = U.foldl' f (0 :< 0) jack
                f (s :< c) j = s + d2 :< c + d2 * d
                    where d  = jackMean - j
                          d2 = d * d
                jackMean     = mean jack
        jack  = jackknife est sample

-- $references
--
-- * Davison, A.C; Hinkley, D.V. (1997) Bootstrap methods and their
--   application. <http://statwww.epfl.ch/davison/BMA/>
