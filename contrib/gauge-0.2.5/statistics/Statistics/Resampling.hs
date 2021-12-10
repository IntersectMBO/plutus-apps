{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module    : Statistics.Resampling
-- Copyright : (c) 2009, 2010 Bryan O'Sullivan
-- License   : BSD3
--
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : portable
--
-- Resampling statistics.

module Statistics.Resampling
    ( -- * Data types
      Bootstrap(..)
    , Estimator(..)
    , resample
      -- * Jackknife
    , jackknife
    ) where

import Control.Concurrent (forkIO, newChan, readChan, writeChan)
import Control.Monad
import Data.Data (Data, Typeable)
import Data.Vector.Generic (unsafeFreeze)
import qualified Data.Foldable as T
import qualified Data.Traversable as T
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU

import GHC.Conc (numCapabilities)
import GHC.Generics (Generic)
import Numeric.Sum (Summation(..), kbn)
import Statistics.Function (indices, inplaceSortIO)
import Statistics.Sample (mean, stdDev, variance, varianceUnbiased)
import Statistics.Types (Sample)
import System.Random.MWC (Gen, GenIO, uniformR, splitGen)


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

data Bootstrap v a = Bootstrap
  { fullSample :: !a
  , resamples  :: v a
  }
  deriving (Eq, Read, Show , Generic, Functor, T.Foldable, T.Traversable
#if __GLASGOW_HASKELL__ >= 708
           , Typeable, Data
#endif
           )

-- | An estimator of a property of a sample, such as its 'mean'.
--
-- The use of an algebraic data type here allows functions such as
-- 'jackknife' and 'bootstrapBCA' to use more efficient algorithms
-- when possible.
data Estimator = Mean
               | Variance
               | VarianceUnbiased
               | StdDev
               | Function (Sample -> Double)

-- | Run an 'Estimator' over a sample.
estimate :: Estimator -> Sample -> Double
estimate Mean             = mean
estimate Variance         = variance
estimate VarianceUnbiased = varianceUnbiased
estimate StdDev           = stdDev
estimate (Function est) = est


----------------------------------------------------------------
-- Resampling
----------------------------------------------------------------

-- | /O(e*r*s)/ Resample a data set repeatedly, with replacement,
-- computing each estimate over the resampled data.
--
-- This function is expensive; it has to do work proportional to
-- /e*r*s/, where /e/ is the number of estimation functions, /r/ is
-- the number of resamples to compute, and /s/ is the number of
-- original samples.
--
-- To improve performance, this function will make use of all
-- available CPUs.  At least with GHC 7.0, parallel performance seems
-- best if the parallel garbage collector is disabled (RTS option
-- @-qg@).
resample :: GenIO
         -> [Estimator]         -- ^ Estimation functions.
         -> Int                 -- ^ Number of resamples to compute.
         -> U.Vector Double     -- ^ Original sample.
         -> IO [(Estimator, Bootstrap U.Vector Double)]
resample gen ests numResamples samples = do
  let ixs = scanl (+) 0 $
            zipWith (+) (replicate numCapabilities q)
                        (replicate r 1 ++ repeat 0)
          where (q,r) = numResamples `quotRem` numCapabilities
  results <- mapM (const (MU.new numResamples)) ests
  done <- newChan
  gens <- splitGen numCapabilities gen
  forM_ (zip3 ixs (tail ixs) gens) $ \ (start,!end,gen') ->
    forkIO $ do
      let loop k ers | k >= end = writeChan done ()
                     | otherwise = do
            re <- resampleVector gen' samples
            forM_ ers $ \(est,arr) ->
                MU.write arr k . est $ re
            loop (k+1) ers
      loop start (zip ests' results)
  replicateM_ numCapabilities $ readChan done
  mapM_ inplaceSortIO results
  -- Build resamples
  res <- mapM unsafeFreeze results
  return $ zip ests
         $ zipWith Bootstrap [estimate e samples | e <- ests]
                             res
 where
  ests' = map estimate ests

-- | Create vector using resamples
resampleVector :: G.Vector v a => Gen -> v a -> IO (v a)
resampleVector gen v
  = G.replicateM n $ do i <- uniformR (0,n-1) gen
                        return $! G.unsafeIndex v i
  where
    n = G.length v

----------------------------------------------------------------
-- Jackknife
----------------------------------------------------------------

-- | /O(n) or O(n^2)/ Compute a statistical estimate repeatedly over a
-- sample, each time omitting a successive element.
jackknife :: Estimator -> Sample -> U.Vector Double
jackknife Mean sample             = jackknifeMean sample
jackknife Variance sample         = jackknifeVariance sample
jackknife VarianceUnbiased sample = jackknifeVarianceUnb sample
jackknife StdDev sample = jackknifeStdDev sample
jackknife (Function est) sample
  | G.length sample == 1 = singletonErr "jackknife"
  | otherwise            = U.map f . indices $ sample
  where f i = est (dropAt i sample)

-- | /O(n)/ Compute the jackknife mean of a sample.
jackknifeMean :: Sample -> U.Vector Double
jackknifeMean samp
  | len == 1  = singletonErr "jackknifeMean"
  | otherwise = G.map (/l) $ G.zipWith (+) (pfxSumL samp) (pfxSumR samp)
  where
    l   = fromIntegral (len - 1)
    len = G.length samp

-- | /O(n)/ Compute the jackknife variance of a sample with a
-- correction factor @c@, so we can get either the regular or
-- \"unbiased\" variance.
jackknifeVariance_ :: Double -> Sample -> U.Vector Double
jackknifeVariance_ c samp
  | len == 1  = singletonErr "jackknifeVariance"
  | otherwise = G.zipWith4 go als ars bls brs
  where
    als = pfxSumL . G.map goa $ samp
    ars = pfxSumR . G.map goa $ samp
    goa x = v * v where v = x - m
    bls = pfxSumL . G.map (subtract m) $ samp
    brs = pfxSumR . G.map (subtract m) $ samp
    m = mean samp
    n = fromIntegral len
    go al ar bl br = (al + ar - (b * b) / q) / (q - c)
      where b = bl + br
            q = n - 1
    len = G.length samp

-- | /O(n)/ Compute the unbiased jackknife variance of a sample.
jackknifeVarianceUnb :: Sample -> U.Vector Double
jackknifeVarianceUnb = jackknifeVariance_ 1

-- | /O(n)/ Compute the jackknife variance of a sample.
jackknifeVariance :: Sample -> U.Vector Double
jackknifeVariance = jackknifeVariance_ 0

-- | /O(n)/ Compute the jackknife standard deviation of a sample.
jackknifeStdDev :: Sample -> U.Vector Double
jackknifeStdDev = G.map sqrt . jackknifeVarianceUnb

pfxSumL :: U.Vector Double -> U.Vector Double
pfxSumL = G.map kbn . G.scanl add zero

pfxSumR :: U.Vector Double -> U.Vector Double
pfxSumR = G.tail . G.map kbn . G.scanr (flip add) zero

-- | Drop the /k/th element of a vector.
dropAt :: U.Unbox e => Int -> U.Vector e -> U.Vector e
dropAt n v = U.slice 0 n v U.++ U.slice (n+1) (U.length v - n - 1) v

singletonErr :: String -> a
singletonErr func = error $
                    "Statistics.Resampling." ++ func ++ ": singleton input"
