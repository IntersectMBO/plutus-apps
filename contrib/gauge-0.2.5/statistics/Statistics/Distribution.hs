{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
-- |
-- Module    : Statistics.Distribution
-- Copyright : (c) 2009 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Type classes for probability distributions

module Statistics.Distribution
    (
      -- * Type classes
      Distribution(..)
    , ContDistr(..)
    ) where

import Prelude hiding (sum)

-- | Type class common to all distributions. Only c.d.f. could be
-- defined for both discrete and continuous distributions.
class Distribution d where
    -- | Cumulative distribution function.  The probability that a
    -- random variable /X/ is less or equal than /x/,
    -- i.e. P(/X/&#8804;/x/). Cumulative should be defined for
    -- infinities as well:
    --
    -- > cumulative d +∞ = 1
    -- > cumulative d -∞ = 0
    cumulative :: d -> Double -> Double

    -- | One's complement of cumulative distibution:
    --
    -- > complCumulative d x = 1 - cumulative d x
    --
    -- It's useful when one is interested in P(/X/>/x/) and
    -- expression on the right side begin to lose precision. This
    -- function have default implementation but implementors are
    -- encouraged to provide more precise implementation.
    complCumulative :: d -> Double -> Double
    complCumulative d x = 1 - cumulative d x

-- | Continuous probability distributuion.
--
--   Minimal complete definition is 'quantile' and either 'density' or
--   'logDensity'.
class Distribution d => ContDistr d where
    -- | Probability density function. Probability that random
    -- variable /X/ lies in the infinitesimal interval
    -- [/x/,/x+/&#948;/x/) equal to /density(x)/&#8901;&#948;/x/
    density :: d -> Double -> Double
    density d = exp . logDensity d

    -- | Inverse of the cumulative distribution function. The value
    -- /x/ for which P(/X/&#8804;/x/) = /p/. If probability is outside
    -- of [0,1] range function should call 'error'
    quantile :: d -> Double -> Double

    -- | 1-complement of @quantile@:
    --
    -- > complQuantile x ≡ quantile (1 - x)
    complQuantile :: d -> Double -> Double
    complQuantile d x = quantile d (1 - x)

    -- | Natural logarithm of density.
    logDensity :: d -> Double -> Double
    logDensity d = log . density d
