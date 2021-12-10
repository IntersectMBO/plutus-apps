{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE BangPatterns, DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Gauge.Analysis
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Analysis code for benchmarks.

-- XXX Do we really want to expose this module to users? This is all internal.
-- Most of the exports are not even used by Gauge itself outside of this
-- module.

module Gauge.Analysis
    ( Outliers(..)
    , OutlierEffect(..)
    , OutlierVariance(..)
    , Report
    , SampleAnalysis(..)
    , analyseSample
    , scale
    , analyseBenchmark
    , analyseMean
    , countOutliers
    , classifyOutliers
    , noteOutliers
    , outlierVariance
    , regress
    , benchmark'
    , benchmarkWith'
    ) where

import Control.Applicative
import Control.Arrow (second)
import Control.DeepSeq (NFData(rnf))
import Control.Monad (forM_, when)
import Gauge.Benchmark (Benchmark (..), BenchmarkAnalysis(..), Benchmarkable, runBenchmark)
import Gauge.IO.Printf (note, printError, prolix, rewindClearLine)
import Gauge.Main.Options (defaultConfig, Config(..), Verbosity (..),
                           DisplayMode (..))
import Gauge.Measurement (Measured(measTime), secs, rescale, measureKeys,
                          measureAccessors_, validateAccessors, renderNames)
import Gauge.Monad (Gauge, askConfig, gaugeIO, Crit(..), askCrit, withConfig)
import Gauge.Format
import qualified Gauge.CSV as CSV
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.IORef (IORef, readIORef, writeIORef)
import Gauge.ListMap (Map)
import Data.Maybe (fromJust, isJust)
import Data.Traversable
import GHC.Generics (Generic)
import Statistics.Function (sort)
import Statistics.Quantile (weightedAvg, Sorted(..))
import Statistics.Regression (bootstrapRegress, olsRegress)
import Statistics.Resampling (Estimator(..),resample)
import Statistics.Sample (mean)
import Statistics.Sample.KernelDensity (kde)
import Statistics.Types (Sample, Estimate(..),ConfInt(..),confidenceInterval
                        ,cl95,confidenceLevel)
import System.Random.MWC (GenIO, createSystemRandom)
import Text.Printf (printf)
import qualified Gauge.ListMap as Map
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Statistics.Resampling.Bootstrap as B
import qualified Statistics.Types                as B
import qualified Statistics.Types as St
import Prelude hiding (sequence, mapM)

-- | Outliers from sample data, calculated using the boxplot
-- technique.
data Outliers = Outliers {
      samplesSeen :: !Int64
    , lowSevere   :: !Int64
    -- ^ More than 3 times the interquartile range (IQR) below the
    -- first quartile.
    , lowMild     :: !Int64
    -- ^ Between 1.5 and 3 times the IQR below the first quartile.
    , highMild    :: !Int64
    -- ^ Between 1.5 and 3 times the IQR above the third quartile.
    , highSevere  :: !Int64
    -- ^ More than 3 times the IQR above the third quartile.
    } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData Outliers

-- | A description of the extent to which outliers in the sample data
-- affect the sample mean and standard deviation.
data OutlierEffect = Unaffected -- ^ Less than 1% effect.
                   | Slight     -- ^ Between 1% and 10%.
                   | Moderate   -- ^ Between 10% and 50%.
                   | Severe     -- ^ Above 50% (i.e. measurements
                                -- are useless).
                     deriving (Eq, Ord, Show, Typeable, Data, Generic)

instance NFData OutlierEffect

outliersEmpty :: Outliers
outliersEmpty = Outliers 0 0 0 0 0

addOutliers :: Outliers -> Outliers -> Outliers
addOutliers (Outliers s a b c d) (Outliers t w x y z) =
    Outliers (s+t) (a+w) (b+x) (c+y) (d+z)
{-# INLINE addOutliers #-}

-- | Analysis of the extent to which outliers in a sample affect its
-- standard deviation (and to some extent, its mean).
data OutlierVariance = OutlierVariance {
      ovEffect   :: OutlierEffect
    -- ^ Qualitative description of effect.
    , ovDesc     :: String
    -- ^ Brief textual description of effect.
    , ovFraction :: Double
    -- ^ Quantitative description of effect (a fraction between 0 and 1).
    } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData OutlierVariance where
    rnf OutlierVariance{..} = rnf ovEffect `seq` rnf ovDesc `seq` rnf ovFraction

-- | Results of a linear regression.
data Regression = Regression {
    regResponder  :: String
    -- ^ Name of the responding variable.
  , regCoeffs     :: Map String (St.Estimate St.ConfInt Double)
    -- ^ Map from name to value of predictor coefficients.
  , regRSquare    :: St.Estimate St.ConfInt Double
    -- ^ R&#0178; goodness-of-fit estimate.
  } deriving (Eq, Show, Typeable, Generic)

instance NFData Regression where
    rnf Regression{..} =
      rnf regResponder `seq` rnf regCoeffs `seq` rnf regRSquare

-- | Result of a bootstrap analysis of a non-parametric sample.
data SampleAnalysis = SampleAnalysis {
      anRegress    :: [Regression]
      -- ^ Estimates calculated via linear regression.
    , anMean       :: St.Estimate St.ConfInt Double
      -- ^ Estimated mean.
    , anStdDev     :: St.Estimate St.ConfInt Double
      -- ^ Estimated standard deviation.
    , anOutlierVar :: OutlierVariance
      -- ^ Description of the effects of outliers on the estimated
      -- variance.
    } deriving (Eq, Show, Typeable, Generic)

instance NFData SampleAnalysis where
    rnf SampleAnalysis{..} =
        rnf anRegress `seq` rnf anMean `seq`
        rnf anStdDev `seq` rnf anOutlierVar

-- | Data for a KDE chart of performance.
data KDE = KDE {
      kdeType   :: String
    , kdeValues :: U.Vector Double
    , kdePDF    :: U.Vector Double
    } deriving (Eq, Show, Typeable, Data, Generic)

instance NFData KDE where
    rnf KDE{..} = rnf kdeType `seq` rnf kdeValues `seq` rnf kdePDF

-- | Report of a sample analysis.
data Report = Report {
      reportName     :: String
      -- ^ The name of this report.
    , reportKeys     :: [String]
      -- ^ See 'measureKeys'.
    , reportMeasured :: V.Vector Measured
      -- ^ Raw measurements.
    , reportAnalysis :: SampleAnalysis
      -- ^ Report analysis.
    , reportOutliers :: Outliers
      -- ^ Analysis of outliers.
    , reportKDEs     :: [KDE]
      -- ^ Data for a KDE of times.
    } deriving (Eq, Show, Typeable, Generic)

instance NFData Report where
    rnf Report{..} =
      rnf reportName `seq` rnf reportKeys `seq`
      rnf reportMeasured `seq` rnf reportAnalysis `seq` rnf reportOutliers `seq`
      rnf reportKDEs

-- | Classify outliers in a data set, using the boxplot technique.
classifyOutliers :: Sample -> Outliers
classifyOutliers sa = U.foldl' ((. outlier) . addOutliers) outliersEmpty ssa
    where outlier e = Outliers {
                        samplesSeen = 1
                      , lowSevere = if e <= loS && e < hiM then 1 else 0
                      , lowMild = if e > loS && e <= loM then 1 else 0
                      , highMild = if e >= hiM && e < hiS then 1 else 0
                      , highSevere = if e >= hiS && e > loM then 1 else 0
                      }
          !loS = q1 - (iqr * 3)
          !loM = q1 - (iqr * 1.5)
          !hiM = q3 + (iqr * 1.5)
          !hiS = q3 + (iqr * 3)
          q1   = weightedAvg 1 4 (Sorted ssa)
          q3   = weightedAvg 3 4 (Sorted ssa)
          ssa  = sort sa
          iqr  = q3 - q1

-- | Compute the extent to which outliers in the sample data affect
-- the sample mean and standard deviation.
outlierVariance
  :: B.Estimate B.ConfInt Double -- ^ Bootstrap estimate of sample mean.
  -> B.Estimate B.ConfInt Double -- ^ Bootstrap estimate of sample
                                 --   standard deviation.
  -> Double                      -- ^ Number of original iterations.
  -> OutlierVariance
outlierVariance µ σ a = OutlierVariance effect desc varOutMin
  where
    ( effect, desc ) | varOutMin < 0.01 = (Unaffected, "no")
                     | varOutMin < 0.1  = (Slight,     "slight")
                     | varOutMin < 0.5  = (Moderate,   "moderate")
                     | otherwise        = (Severe,     "severe")
    varOutMin = (minBy varOut 1 (minBy cMax 0 µgMin)) / σb2
    varOut c  = (ac / a) * (σb2 - ac * σg2) where ac = a - c
    σb        = B.estPoint σ
    µa        = B.estPoint µ / a
    µgMin     = µa / 2
    σg        = min (µgMin / 4) (σb / sqrt a)
    σg2       = σg * σg
    σb2       = σb * σb
    minBy f q r = min (f q) (f r)
    cMax x    = fromIntegral (floor (-2 * k0 / (k1 + sqrt det)) :: Int)
      where
        k1    = σb2 - a * σg2 + ad
        k0    = -a * ad
        ad    = a * d
        d     = k * k where k = µa - x
        det   = k1 * k1 - 4 * σg2 * k0

-- | Count the total number of outliers in a sample.
countOutliers :: Outliers -> Int64
countOutliers (Outliers _ a b c d) = a + b + c + d
{-# INLINE countOutliers #-}

-- | Display the mean of a 'Sample', and characterise the outliers
-- present in the sample.
analyseMean :: Sample
            -> Int              -- ^ Number of iterations used to
                                -- compute the sample.
            -> Gauge Double
analyseMean a iters = do
  let µ = mean a
  _ <- note "mean is %s (%d iterations)\n" (secs µ) iters
  noteOutliers . classifyOutliers $ a
  return µ

-- | Multiply the 'Estimate's in an analysis by the given value, using
-- 'B.scale'.
scale :: Double                 -- ^ Value to multiply by.
      -> SampleAnalysis -> SampleAnalysis
scale f s@SampleAnalysis{..} = s {
                                 anMean = B.scale f anMean
                               , anStdDev = B.scale f anStdDev
                               }

-- | Return a random number generator, creating one if necessary.
--
-- This is not currently thread-safe, but in a harmless way (we might
-- call 'createSystemRandom' more than once if multiple threads race).
getGen :: Gauge GenIO
getGen = memoise gen createSystemRandom

getMeasurement :: (U.Unbox a) => (Measured -> a) -> V.Vector Measured -> U.Vector a
getMeasurement f v = U.convert . V.map f $ v

-- | Memoise the result of an 'IO' action.
--
-- This is not currently thread-safe, but hopefully in a harmless way.
-- We might call the given action more than once if multiple threads
-- race, so our caller's job is to write actions that can be run
-- multiple times safely.
memoise :: (Crit -> IORef (Maybe a)) -> IO a -> Gauge a
memoise ref generate = do
  r <- ref <$> askCrit
  gaugeIO $ do
    mv <- readIORef r
    case mv of
      Just rv -> return rv
      Nothing -> do
        rv <- generate
        writeIORef r (Just rv)
        return rv

toCL :: Maybe Double -> B.CL Double
toCL a =
    case a of
        Nothing -> B.cl95
        Just x -> B.mkCL x

-- | Perform an analysis of a measurement.
analyseSample :: String            -- ^ Experiment name.
              -> V.Vector Measured -- ^ Sample data.
              -> Gauge (Either String Report)
analyseSample name meas = do
  Config{..} <- askConfig
  let ests      = [Mean,StdDev]
      stime     = getMeasurement (measTime . rescale) $ meas
      n         = G.length meas
  _ <- prolix "bootstrapping with %d samples\n" n

  gen <- getGen
  ers <- (sequence <$>) . mapM (\(ps,r) -> regress gen ps r meas) $ ((["iters"],"time"):regressions)
  case ers of
    Left err -> pure $ Left err
    Right rs -> do
      resamps <- gaugeIO $ resample gen ests resamples stime
      let [estMean,estStdDev] = B.bootstrapBCA (toCL confInterval) stime
                                               resamps
          ov = outlierVariance estMean estStdDev (fromIntegral n)
          an = SampleAnalysis
                 { anRegress    = rs
                 , anMean       = estMean
                 , anStdDev     = estStdDev
                 , anOutlierVar = ov
                 }
      return $ Right $ Report
        { reportName     = name
        , reportKeys     = measureKeys
        , reportMeasured = meas
        , reportAnalysis = an
        , reportOutliers = classifyOutliers stime
        , reportKDEs     = [uncurry (KDE "time") (kde 128 stime)]
        }


-- | Regress the given predictors against the responder.
--
-- Errors may be returned under various circumstances, such as invalid
-- names or lack of needed data.
--
-- See 'olsRegress' for details of the regression performed.
regress :: GenIO
        -> [String]             -- ^ Predictor names.
        -> String               -- ^ Responder name.
        -> V.Vector Measured
        -> Gauge (Either String Regression)
regress gen predNames respName meas
    | G.null meas = pure $ Left "no measurements"
    | otherwise   = case validateAccessors predNames respName of
        Left err   -> pure $ Left err
        Right accs -> do
            let unmeasured = [n | (n, Nothing) <- map (second ($ G.head meas)) accs]
            if not (null unmeasured)
                then pure $ Left $ "no data available for " ++ renderNames unmeasured
                else do
                    let (r:ps) = map ((`getMeasurement` meas) . (fromJust .) . snd) accs
                    Config{..} <- askConfig
                    (coeffs,r2) <- gaugeIO $
                        bootstrapRegress gen resamples (toCL confInterval)
                                         olsRegress ps r
                    pure $ Right $ Regression
                        { regResponder = respName
                        , regCoeffs    = Map.fromList (zip (predNames ++ ["y"]) (G.toList coeffs))
                        , regRSquare   = r2
                        }

-- | Display a report of the 'Outliers' present in a 'Sample'.
noteOutliers :: Outliers -> Gauge ()
noteOutliers o = do
  let frac n = (100::Double) * fromIntegral n / fromIntegral (samplesSeen o)
      check :: Int64 -> Double -> String -> Gauge ()
      check k t d = when (frac k > t) $
                    note "  %d (%.1g%%) %s\n" k (frac k) d
      outCount = countOutliers o
  when (outCount > 0) $ do
    _ <- note "found %d outliers among %d samples (%.1g%%)\n"
         outCount (samplesSeen o) (frac outCount)
    check (lowSevere o) 0 "low severe"
    check (lowMild o) 1 "low mild"
    check (highMild o) 1 "high mild"
    check (highSevere o) 0 "high severe"

-- | Analyse a single benchmark.
analyseBenchmark :: String -> V.Vector Measured -> Gauge Report
analyseBenchmark desc meas = do
  Config{..} <- askConfig
  _ <- prolix "%sanalysing with %d resamples\n" rewindClearLine resamples
  -- XXX handle meas being empty
  erp <- analyseSample desc meas
  case erp of
    Left err -> printError "*** Error: %s\n" err
    Right rpt@Report{..} -> do
        let SampleAnalysis{..} = reportAnalysis
            OutlierVariance{..} = anOutlierVar
            wibble = printOverallEffect ovEffect
            (builtin, others) = splitAt 1 anRegress

        gaugeIO $ CSV.write csvFile $ CSV.Row
            [ CSV.string desc
            , CSV.float (estPoint anMean)
            , CSV.float (fst $ confidenceInterval anMean)
            , CSV.float (snd $ confidenceInterval anMean)
            , CSV.float (estPoint anStdDev)
            , CSV.float (fst $ confidenceInterval anStdDev)
            , CSV.float (snd $ confidenceInterval anStdDev)
            ]

        case displayMode of
            StatsTable -> do
              _ <- note "%sbenchmarked %s%s%s\n" rewindClearLine green desc reset
              let r2 n = printf "%.3f R\178" n
              forM_ builtin $ \Regression{..} ->
                case Map.lookup "iters" regCoeffs of
                  Nothing -> return ()
                  Just t  -> bs secs "time" t >> bs r2 "" regRSquare
              bs secs "mean" anMean
              bs secs "std dev" anStdDev
              forM_ others $ \Regression{..} -> do
                _ <- bs r2 (regResponder ++ ":") regRSquare
                forM_ (Map.toList regCoeffs) $ \(prd,val) ->
                  bs (printf "%.3g") ("  " ++ prd) val
              when (verbosity == Verbose || (ovEffect > Slight && verbosity > Quiet)) $ do
                when (verbosity == Verbose) $ noteOutliers reportOutliers
                _ <- note "variance introduced by outliers: %d%% (%s)\n"
                     (round (ovFraction * 100) :: Int) wibble
                return ()

              _ <- traverse
                    (\(k, (a, s, _)) -> reportStat Verbose a s k)
                    measureAccessors_
              _ <- note "\n"
              pure ()
            Condensed -> do
              _ <- note "%s%s%-40s%s " rewindClearLine green desc reset
              bsSmall secs "mean" anMean
              bsSmall secs "( +-" anStdDev
              _ <- note ")\n"
              pure ()

        return rpt
      where bs :: (Double -> String) -> String -> Estimate ConfInt Double -> Gauge ()
            bs f metric e@Estimate{..} =
              note "%s%-20s%s %s%-10s%s (%s .. %s%s)\n" yellow metric reset
                   red (f estPoint) reset
                   (f $ fst $ confidenceInterval e) (f $ snd $ confidenceInterval e)
                   (let cl = confIntCL estError
                        str | cl == cl95 = ""
                            | otherwise  = printf ", ci %.3f" (confidenceLevel cl)
                    in str
                   )
            bsSmall :: (Double -> String) -> String -> Estimate ConfInt Double -> Gauge ()
            bsSmall f metric Estimate{..} =
              note "%s %s%-10s%s" metric red (f estPoint) reset

            reportStat :: Verbosity
                       -> (Measured -> Maybe Double)
                       -> (Double -> String)
                       -> String -> Gauge ()
            reportStat lvl accessor sh msg = do
              let v0 = V.map (accessor . rescale) meas
              -- Print average only if all data points are present
              when (V.all isJust v0) $ do
                  let v = V.map fromJust v0
                      total = V.sum v
                      len = V.length v
                      avg = total / (fromIntegral len)
                  when (verbosity >= lvl && avg > 0.0) $ do
                    note "%-20s %-10s (%s .. %s)\n" msg (sh avg)
                      (sh (V.minimum v)) (sh (V.maximum v))


printOverallEffect :: OutlierEffect -> String
printOverallEffect Unaffected = "unaffected"
printOverallEffect Slight     = "slightly inflated"
printOverallEffect Moderate   = "moderately inflated"
printOverallEffect Severe     = "severely inflated"

-- XXX The original type of these types returned 'Report' type. But the
-- implementation was wrong as it was not running any environment settings on
-- the way to the benchmark. Now, we have used the correct function to do that
-- but unfortunately that function returns void. That can be fixed though if it
-- is important.

-- | Run a benchmark interactively and analyse its performance.
benchmarkWith' :: Config -> Benchmarkable -> IO ()
benchmarkWith' cfg bm = withConfig cfg $
    runBenchmark (const True) (Benchmark "function" bm) (BenchmarkNormal analyseBenchmark)

-- | Run a benchmark interactively and analyse its performanc.
benchmark' :: Benchmarkable -> IO ()
benchmark' = benchmarkWith' defaultConfig
