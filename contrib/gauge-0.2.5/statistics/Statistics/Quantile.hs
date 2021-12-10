{-# LANGUAGE FlexibleContexts #-}
-- |
-- Module    : Statistics.Quantile
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Functions for approximating quantiles, i.e. points taken at regular
-- intervals from the cumulative distribution function of a random
-- variable.
--
-- The number of quantiles is described below by the variable /q/, so
-- with /q/=4, a 4-quantile (also known as a /quartile/) has 4
-- intervals, and contains 5 points.  The parameter /k/ describes the
-- desired point, where 0 ≤ /k/ ≤ /q/.

module Statistics.Quantile
    (
    
    -- * Quantile estimation functions
      weightedAvg
    , Sorted(..)
    -- * References
    -- $references
    ) where

import Data.Vector.Generic ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

newtype Sorted x = Sorted x

-- | O(/n/ log /n/). Estimate the /k/th /q/-quantile of a sample,
-- using the weighted average method.
--
-- The following properties should hold:
--   * the length of the input is greater than @0@
--   * the input does not contain @NaN@
--   * k ≥ 0 and k ≤ q
--
-- otherwise an error will be thrown.
weightedAvg :: G.Vector v Double =>
               Int        -- ^ /k/, the desired quantile.
            -> Int        -- ^ /q/, the number of quantiles.
            -> Sorted (v Double)   -- ^ /x/, the sample data.
            -> Double
weightedAvg k q (Sorted x)
  | G.any isNaN x   = modErr "weightedAvg" "Sample contains NaNs"
  | n == 0          = modErr "weightedAvg" "Sample is empty"
  | n == 1          = G.head x
  | q < 2           = modErr "weightedAvg" "At least 2 quantiles is needed"
  | k == q          = G.maximum x
  | k >= 0 || k < q = xj + g * (xj1 - xj)
  | otherwise       = modErr "weightedAvg" "Wrong quantile number"
  where
    j   = floor idx
    idx = fromIntegral (n - 1) * fromIntegral k / fromIntegral q
    g   = idx - fromIntegral j
    xj  = x ! j
    xj1 = x ! (j+1)
    n   = G.length x
{-# SPECIALIZE weightedAvg :: Int -> Int -> Sorted (U.Vector Double) -> Double #-}
{-# SPECIALIZE weightedAvg :: Int -> Int -> Sorted (V.Vector Double) -> Double #-}

modErr :: String -> String -> a
modErr f err = error $ "Statistics.Quantile." ++ f ++ ": " ++ err



-- $references
--
-- * Weisstein, E.W. Quantile. /MathWorld/.
--   <http://mathworld.wolfram.com/Quantile.html>
--
-- * Hyndman, R.J.; Fan, Y. (1996) Sample quantiles in statistical
--   packages. /American Statistician/
--   50(4):361&#8211;365. <http://www.jstor.org/stable/2684934>
