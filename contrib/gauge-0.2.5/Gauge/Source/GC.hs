-- |
-- Module      : Gauge.Source.GC
-- Copyright   : (c) 2017 Vincent Hanquez
--
-- Metrics gathering related to the GHC RTS / GC
--
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Gauge.Source.GC
    ( Metrics(..)
    , supported
    , withMetrics
    ) where

import           Control.Applicative
import           Data.Word
import           Data.IORef (readIORef, newIORef, IORef)
import           Gauge.Time
import           System.IO.Unsafe (unsafePerformIO)
import           Gauge.Optional (omitted, toOptional, Optional, OptionalTag)

#if MIN_VERSION_base(4,10,0)
import qualified GHC.Stats as GHC (RTSStats(..), getRTSStatsEnabled, getRTSStats)
#else
import qualified Control.Exception as Exn
import qualified GHC.Stats as GHC (GCStats(..), getGCStats)
import           Data.Int
#endif

import Prelude -- Silence redundant import warnings

#if MIN_VERSION_base(4,10,0)
newtype AbsMetrics = AbsMetrics GHC.RTSStats
#else
newtype AbsMetrics = AbsMetrics GHC.GCStats
#endif

-- | Check if RTS/GC metrics gathering is enabled or not
supported :: Bool
supported = unsafePerformIO (readIORef supportedVar)
{-# NOINLINE supported #-}

supportedVar :: IORef Bool
supportedVar = unsafePerformIO $ do
#if __GHCJS__
    let b = False
#elif MIN_VERSION_base(4,10,0)
    b <- GHC.getRTSStatsEnabled
#else
    b <- (const True <$> GHC.getGCStats) `Exn.catch` \(_ :: Exn.SomeException) -> pure False
#endif
    newIORef b
{-# NOINLINE supportedVar #-}

getMetrics :: IO AbsMetrics
getMetrics = AbsMetrics <$>
#if MIN_VERSION_base(4,10,0)
    GHC.getRTSStats
#else
    GHC.getGCStats
#endif

-- | Differential metrics related the RTS/GC
data Metrics = Metrics
    { allocated      :: {-# UNPACK #-} !(Optional Word64) -- ^ number of bytes allocated
    , numGCs         :: {-# UNPACK #-} !Word64 -- ^ number of GCs
    , copied         :: {-# UNPACK #-} !(Optional Word64) -- ^ number of bytes copied
    , mutWallSeconds :: {-# UNPACK #-} !NanoSeconds -- ^ mutator wall time measurement
    , mutCpuSeconds  :: {-# UNPACK #-} !NanoSeconds -- ^ mutator cpu time measurement
    , gcWallSeconds  :: {-# UNPACK #-} !NanoSeconds -- ^ gc wall time measurement
    , gcCpuSeconds   :: {-# UNPACK #-} !NanoSeconds -- ^ gc cpu time measurement
    } deriving (Show,Eq)

diffMetrics :: AbsMetrics -> AbsMetrics -> Metrics
diffMetrics (AbsMetrics end) (AbsMetrics start) =
#if MIN_VERSION_base(4,10,0)
    Metrics { allocated      = diff (-*?) GHC.allocated_bytes
            , numGCs         = diff (-*) (fromIntegral . GHC.gcs)
            , copied         = diff (-*?) GHC.copied_bytes
            , mutWallSeconds = NanoSeconds $ diff (-*) (fromIntegral . GHC.mutator_elapsed_ns)
            , mutCpuSeconds  = NanoSeconds $ diff (-*) (fromIntegral . GHC.mutator_cpu_ns)
            , gcWallSeconds  = NanoSeconds $ diff (-*) (fromIntegral . GHC.gc_elapsed_ns)
            , gcCpuSeconds   = NanoSeconds $ diff (-*) (fromIntegral . GHC.gc_cpu_ns)
            }
  where
    diff op f = f end `op` f start
    (-*) :: (Ord a, Num a) => a -> a -> a
    (-*) a b
        | a >= b    = a - b
        | otherwise = (-1)

    (-*?) :: (OptionalTag a, Ord a, Num a) => a -> a -> Optional a
    (-*?) a b
        | a >= b    = toOptional "gc metric" (a - b)
        | otherwise = omitted
#else
    Metrics { allocated      = diff (-*?) GHC.bytesAllocated
            , numGCs         = diff (-*) GHC.numGcs
            , copied         = diff (-*?) GHC.bytesCopied
            , mutWallSeconds = doubleToNanoSeconds $ diff (-) GHC.mutatorWallSeconds
            , mutCpuSeconds  = doubleToNanoSeconds $ diff (-) GHC.mutatorCpuSeconds
            , gcWallSeconds  = doubleToNanoSeconds $ diff (-) GHC.gcWallSeconds
            , gcCpuSeconds   = doubleToNanoSeconds $ diff (-) GHC.gcCpuSeconds
            }
  where
    diff op f = f end `op` f start

    (-*) :: Int64 -> Int64 -> Word64
    (-*) a b
        | a >= b    = fromIntegral (a - b)
        | otherwise = (-1)

    (-*?) :: Int64 -> Int64 -> Optional Word64
    (-*?) a b
        | a >= b    = toOptional "gc metrics" $ fromIntegral (a - b)
        | otherwise = omitted
#endif

-- | Return RTS/GC metrics differential between a call to `f`
withMetrics :: IO a -- ^ function to measure
            -> IO (a, Maybe Metrics)
withMetrics f
    | supported = do
        start <- getMetrics
        a     <- f
        end   <- getMetrics
        pure (a, Just $ diffMetrics end start)
    | otherwise = f >>= \a -> pure (a, Nothing)
