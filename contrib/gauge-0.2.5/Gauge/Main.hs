{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Trustworthy     #-}

-- |
-- Module      : Gauge.Main
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Wrappers for compiling and running benchmarks quickly and easily.
-- See 'defaultMain' below for an example.

module Gauge.Main
    (
    -- * Turning a suite of benchmarks into a program
      defaultMain
    , defaultMainWith
    , runMode
    -- * Running Benchmarks Interactively
    , benchmark
    , benchmarkWith
    , module Gauge.Benchmark
    ) where

import           Control.Applicative
import           Control.Monad (unless, when)
import qualified Gauge.CSV as CSV
#ifdef HAVE_ANALYSIS
import           Gauge.Analysis (analyseBenchmark)
#endif
import           Gauge.IO.Printf (note, printError, rewindClearLine)
import           Gauge.Benchmark
import           Gauge.Main.Options
import           Gauge.Measurement (Measured, measureAccessors_, rescale)
import           Gauge.Monad (Gauge, askConfig, withConfig, gaugeIO)
import           Data.List (sort)
import           Data.Traversable
import           System.Environment (getProgName, getArgs)
import           System.Exit (ExitCode(..), exitWith)
-- import System.FilePath.Glob
import           System.IO (BufferMode(..), hSetBuffering, stdout)
import           Basement.Terminal (initialize)
import qualified Data.Vector as V
import           Prelude -- Silence redundant import warnings

-- | An entry point that can be used as a @main@ function.
--
-- > import Gauge.Main
-- >
-- > fib :: Int -> Int
-- > fib 0 = 0
-- > fib 1 = 1
-- > fib n = fib (n-1) + fib (n-2)
-- >
-- > main = defaultMain [
-- >        bgroup "fib" [ bench "10" $ whnf fib 10
-- >                     , bench "35" $ whnf fib 35
-- >                     , bench "37" $ whnf fib 37
-- >                     ]
-- >                    ]
defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig

-- | Display an error message from a command line parsing failure, and
-- exit.
parseError :: String -> IO a
parseError msg = do
  _ <- printError "Error: %s\n" msg
  _ <- printError "Run \"%s --help\" for usage information\n" =<< getProgName
  exitWith (ExitFailure 64)

selectBenches :: MatchType -> [String] -> Benchmark -> IO (String -> Bool)
selectBenches matchType benches bsgroup = do
  let toRun = makeSelector matchType benches
  unless (null benches || any toRun (benchNames bsgroup)) $
    parseError "none of the specified names matches a benchmark"
  return toRun

-- | Analyse a single benchmark, printing just the time by default and all
-- stats in verbose mode.
quickAnalyse :: String -> V.Vector Measured -> Gauge ()
quickAnalyse desc meas = do
  Config{..} <- askConfig
  let timeAccessor = filter (("time" ==)  . fst) measureAccessors_
      accessors =
        if verbosity == Verbose
        then measureAccessors_
        else timeAccessor

  _ <- note "%s%-40s " rewindClearLine desc
  if verbosity == Verbose then gaugeIO (putStrLn "") else return ()
  _ <- traverse
        (\(k, (a, s, _)) -> reportStat a s k)
        accessors
  _ <- note "\n"

  _ <- traverse
        (\(_, (a, _, _)) -> writeToCSV csvFile a)
        timeAccessor
  pure ()

  where

  reportStat accessor sh msg =
    when (not $ V.null meas) $
      let val = (accessor . rescale) $ V.last meas
       in maybe (return ()) (\x -> note "%-20s %-10s\n" msg (sh x)) val

  writeToCSV file accessor =
    when (not $ V.null meas) $ do
      let val = (accessor . rescale) $ V.last meas
      case val of
        Nothing -> pure ()
        Just v ->
          gaugeIO $ CSV.write file $ CSV.Row
              [ CSV.string desc
              , CSV.float v
              ]

-- | Run a benchmark interactively with supplied config, and analyse its
-- performance.
benchmarkWith :: Config -> Benchmarkable -> IO ()
benchmarkWith cfg bm =
  withConfig cfg $
    runBenchmark (const True) (Benchmark "function" bm) (BenchmarkNormal quickAnalyse)

-- | Run a benchmark interactively with default config, and analyse its
-- performance.
benchmark :: Benchmarkable -> IO ()
benchmark = benchmarkWith defaultConfig

-- | An entry point that can be used as a @main@ function, with
-- configurable defaults.
--
-- Example:
--
-- > import Gauge.Main.Options
-- > import Gauge.Main
-- >
-- > myConfig = defaultConfig {
-- >              -- Do not GC between runs.
-- >              forceGC = False
-- >            }
-- >
-- > main = defaultMainWith myConfig [
-- >          bench "fib 30" $ whnf fib 30
-- >        ]
--
-- If you save the above example as @\"Fib.hs\"@, you should be able
-- to compile it as follows:
--
-- > ghc -O --make Fib
--
-- Run @\"Fib --help\"@ on the command line to get a list of command
-- line options.
defaultMainWith :: Config
                -> [Benchmark]
                -> IO ()
defaultMainWith defCfg bs = do
    initialize
    args <- getArgs
    let (cfg, extra) = parseWith defCfg args
#ifdef HAVE_ANALYSIS
    let cfg' = cfg
#else
    let cfg' = cfg {quickMode = True}
#endif
    runMode (mode cfg') cfg' extra bs

-- | Run a set of 'Benchmark's with the given 'Mode'.
--
-- This can be useful if you have a 'Mode' from some other source (e.g. from a
-- one in your benchmark driver's command-line parser).
runMode :: Mode -> Config -> [String] -> [Benchmark] -> IO ()
runMode wat cfg benches bs =
    -- TBD: This has become messy. We use mode as well as cfg options for the
    -- same purpose It is possible to specify multiple exclusive options.  We
    -- need to handle the exclusive options in a better way.
    case wat of
        List        -> mapM_ putStrLn . sort . concatMap benchNames $ bs
        Version     -> putStrLn versionInfo
        Help        -> putStrLn describe
        DefaultMode -> runDefault
  where
    runDefault = do
        -- write the raw csv file header
        CSV.write (csvRawFile cfg) $ CSV.Row $ map CSV.string $
            ["name"] ++ map fst measureAccessors_

        -- write the csv file header
        CSV.write (csvFile cfg) $ CSV.Row $ map CSV.string $ ["Name"] ++
            if quickMode cfg
            then ["Time"]
            -- This requires statistical analysis support.  This must
            -- remain compatible with criterion.
            else ["Mean","MeanLB","MeanUB","Stddev","StddevLB","StddevUB"]

        hSetBuffering stdout NoBuffering
        selector <- selectBenches (match cfg) benches bsgroup

        -- if compiled without analysis step, then default to quickmode
#ifdef HAVE_ANALYSIS
        let compiledAnalyseStep = analyseBenchmark
#else
        let compiledAnalyseStep = quickAnalyse
#endif

        let mode = case (measureOnly cfg, iters cfg, quickMode cfg) of
                (Just outfile, _           , _   )  -> BenchmarkNormal $ \_ r -> gaugeIO (writeFile outfile (show r))
                (Nothing     , Just nbIters, _   )  -> BenchmarkIters nbIters
                (Nothing     , Nothing     , True)  -> BenchmarkNormal quickAnalyse
                (Nothing     , Nothing     , False) -> BenchmarkNormal compiledAnalyseStep

        withConfig cfg $ runBenchmark selector bsgroup mode

    bsgroup = BenchGroup "" bs
