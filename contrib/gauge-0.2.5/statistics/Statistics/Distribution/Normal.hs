{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
-- |
-- Module    : Statistics.Distribution.Normal
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- The normal distribution.  This is a continuous probability
-- distribution that describes data that cluster around a mean.

module Statistics.Distribution.Normal
    (
      NormalDistribution
    -- * Constructors
    -- , normalDistr
    --, normalDistrE
    , standard
    ) where

import Data.Data             (Data, Typeable)
import GHC.Generics          (Generic)
import Numeric.MathFunctions.Constants (m_sqrt_2, m_sqrt_2_pi)
import Numeric.SpecFunctions (erfc, invErfc)

import qualified Statistics.Distribution as D
import Statistics.Internal


-- | The normal distribution.
data NormalDistribution = ND {
      mean       :: {-# UNPACK #-} !Double
    , stdDev     :: {-# UNPACK #-} !Double
    , ndPdfDenom :: {-# UNPACK #-} !Double
    , ndCdfDenom :: {-# UNPACK #-} !Double
    } deriving (Eq, Typeable, Data, Generic)

instance Show NormalDistribution where
  showsPrec i (ND m s _ _) = defaultShow2 "normalDistr" m s i
instance Read NormalDistribution where
  readPrec = defaultReadPrecM2 "normalDistr" normalDistrE

instance D.Distribution NormalDistribution where
    cumulative      = cumulative
    complCumulative = complCumulative

instance D.ContDistr NormalDistribution where
    logDensity    = logDensity
    quantile      = quantile
    complQuantile = complQuantile

-- | Standard normal distribution with mean equal to 0 and variance equal to 1
standard :: NormalDistribution
standard = ND { mean       = 0.0
              , stdDev     = 1.0
              , ndPdfDenom = log m_sqrt_2_pi
              , ndCdfDenom = m_sqrt_2
              }

-- | Create normal distribution from parameters.
--
-- IMPORTANT: prior to 0.10 release second parameter was variance not
-- standard deviation.
normalDistrE :: Double            -- ^ Mean of distribution
             -> Double            -- ^ Standard deviation of distribution
             -> Maybe NormalDistribution
normalDistrE m sd
  | sd > 0    = Just ND { mean       = m
                        , stdDev     = sd
                        , ndPdfDenom = log $ m_sqrt_2_pi * sd
                        , ndCdfDenom = m_sqrt_2 * sd
                        }
  | otherwise = Nothing

logDensity :: NormalDistribution -> Double -> Double
logDensity d x = (-xm * xm / (2 * sd * sd)) - ndPdfDenom d
    where xm = x - mean d
          sd = stdDev d

cumulative :: NormalDistribution -> Double -> Double
cumulative d x = erfc ((mean d - x) / ndCdfDenom d) / 2

complCumulative :: NormalDistribution -> Double -> Double
complCumulative d x = erfc ((x - mean d) / ndCdfDenom d) / 2

quantile :: NormalDistribution -> Double -> Double
quantile d p
  | p == 0         = -inf
  | p == 1         = inf
  | p == 0.5       = mean d
  | p > 0 && p < 1 = x * ndCdfDenom d + mean d
  | otherwise      =
    error $ "Statistics.Distribution.Normal.quantile: p must be in [0,1] range. Got: "++show p
  where x          = - invErfc (2 * p)
        inf        = 1/0

complQuantile :: NormalDistribution -> Double -> Double
complQuantile d p
  | p == 0         = inf
  | p == 1         = -inf
  | p == 0.5       = mean d
  | p > 0 && p < 1 = x * ndCdfDenom d + mean d
  | otherwise      =
    error $ "Statistics.Distribution.Normal.complQuantile: p must be in [0,1] range. Got: "++show p
  where x          = invErfc (2 * p)
        inf        = 1/0
