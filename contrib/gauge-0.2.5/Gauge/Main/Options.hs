{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}

-- |
-- Module      : Gauge.Main.Options
-- Copyright   : (c) 2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Benchmarking command-line configuration.

module Gauge.Main.Options
    ( defaultConfig
    , makeSelector
    , parseWith
    , describe
    , versionInfo
    , Config (..)
    , Verbosity (..)
    , DisplayMode (..)
    , MatchType (..)
    , Mode (..)
    ) where

import Gauge.Measurement
       (validateAccessors, defaultMinSamplesNormal,
        defaultMinSamplesQuick, defaultTimeLimitNormal,
        defaultTimeLimitQuick)
import Gauge.Time (MilliSeconds(..))
import Data.Char (isSpace, toLower)
import Data.List (foldl')
import Data.Version (showVersion)
import System.Console.GetOpt
import Paths_gauge (version)
import Data.Data (Data, Typeable)
import Data.Int (Int64)
import Data.List (isInfixOf, isPrefixOf)
import GHC.Generics (Generic)

-- | Control the amount of information displayed.
data Verbosity = Quiet
               | Normal
               | Verbose
                 deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data,
                           Generic)

-- | How to match a benchmark name.
data MatchType = Exact
                -- ^ Match the exact benchmark name
               | Prefix
                 -- ^ Match by prefix. For example, a prefix of
                 -- @\"foo\"@ will match @\"foobar\"@.
               | Pattern
                 -- ^ Match by searching given substring in benchmark
                 -- paths.
               | IPattern
                 -- ^ Same as 'Pattern', but case insensitive.
               deriving (Eq, Ord, Bounded, Enum, Read, Show, Typeable, Data,
                         Generic)

-- | Execution mode for a benchmark program.
data Mode = List
            -- ^ List all benchmarks.
          | Version
            -- ^ Print the version.
          | Help
            -- ^ Print help
          | DefaultMode
            -- ^ Default Benchmark mode
          deriving (Eq, Read, Show, Typeable, Data, Generic)

data DisplayMode =
      Condensed
    | StatsTable
    deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Top-level benchmarking configuration.
data Config = Config {
      confInterval :: Maybe Double
      -- ^ Confidence interval for bootstrap estimation (greater than
      -- 0, less than 1).
    , forceGC      :: Bool
      -- ^ /Obsolete, unused/.  This option used to force garbage
      -- collection between every benchmark run, but it no longer has
      -- an effect (we now unconditionally force garbage collection).
      -- This option remains solely for backwards API compatibility.
    , timeLimit    :: Maybe Double
      -- ^ Number of seconds to run a single benchmark.  In practice, execution
      -- time may exceed this limit to honor minimum number of samples or
      -- minimum duration of each sample. Increased time limit allows us to
      -- take more samples. Use 0 for a single sample benchmark.
    , minSamples   :: Maybe Int
      -- ^ Minimum number of samples to be taken.
    , minDuration  :: MilliSeconds
      -- ^ Minimum duration of each sample, increased duration allows us to
      -- perform more iterations in each sample. To enforce a single iteration
      -- in a sample use duration 0.
    , includeFirstIter :: Bool
      -- ^ Discard the very first iteration of a benchmark. The first iteration
      -- includes the potentially extra cost of one time evaluations
      -- introducing large variance.
    , quickMode    :: Bool
    -- ^ Quickly measure and report raw measurements.
    , measureOnly  :: Maybe FilePath
    -- ^ Just measure the given benchmark and place the raw output in this
    -- file, do not analyse and generate a report.
    , measureWith  :: Maybe FilePath
    -- ^ Specify the path of the benchmarking program to use (this program
    -- itself) for measuring the benchmarks in a separate process.
    , resamples    :: Int
      -- ^ Number of resamples to perform when bootstrapping.
    , regressions  :: [([String], String)]
      -- ^ Regressions to perform.
    , rawDataFile  :: Maybe FilePath
      -- ^ File to write binary measurement and analysis data to.  If
      -- not specified, this will be a temporary file.
    , reportFile   :: Maybe FilePath
      -- ^ File to write report output to, with template expanded.
    , csvFile      :: Maybe FilePath
      -- ^ File to write CSV summary to.
    , csvRawFile   :: Maybe FilePath
      -- ^ File to write CSV measurements to.
    , jsonFile     :: Maybe FilePath
      -- ^ File to write JSON-formatted results to.
    , junitFile    :: Maybe FilePath
      -- ^ File to write JUnit-compatible XML results to.
    , verbosity    :: Verbosity
      -- ^ Verbosity level to use when running and analysing
      -- benchmarks.
    , template     :: FilePath
      -- ^ Template file to use if writing a report.
    , iters        :: Maybe Int64
      -- ^ Number of iterations
    , match        :: MatchType
      -- ^ Type of matching to use, if any
    , mode         :: Mode
      -- ^ Mode of operation
    , displayMode  :: DisplayMode
    } deriving (Eq, Read, Show, Typeable, Data, Generic)

defaultMinDuration :: MilliSeconds
defaultMinDuration = MilliSeconds 30

-- | Default benchmarking configuration.
defaultConfig :: Config
defaultConfig = Config
    { confInterval     = Nothing
    , forceGC          = True
    , timeLimit        = Nothing
    , minSamples       = Nothing
    , minDuration      = defaultMinDuration
    , includeFirstIter = False
    , quickMode        = False
    , measureOnly      = Nothing
    , measureWith      = Nothing
    , resamples        = 1000
    , regressions      = []
    , rawDataFile      = Nothing
    , reportFile       = Nothing
    , csvFile          = Nothing
    , csvRawFile       = Nothing
    , jsonFile         = Nothing
    , junitFile        = Nothing
    , verbosity        = Normal
    , template         = "default"
    , iters            = Nothing
    , match            = Prefix
    , mode             = DefaultMode
    , displayMode      = StatsTable
    }

-- | Create a benchmark selector function that can tell if a name given on the
-- command line matches a defined benchmark.
makeSelector :: MatchType
            -> [String]
            -- ^ Command line arguments.
            -> (String -> Bool)
makeSelector matchKind args =
  case matchKind of
    Exact    -> \b -> null args || any (== b) args
    Prefix   -> \b -> null args || any (`isPrefixOf` b) args
    Pattern  -> \b -> null args || any (`isInfixOf` b) args
    IPattern -> \b -> null args || any (`isInfixOf` map toLower b) (map (map toLower) args)

parseWith :: Config
            -- ^ Default configuration to use
          -> [String]
            -- ^ Program Argument
          -> (Config, [String])
parseWith start argv =
    case getOpt Permute opts argv of
        (o,n,[]  ) -> (foldl' (flip id) start o, n)
        (_,_,errs) -> optionError (concat errs ++ usageInfo header opts)

opts :: [OptDescr (Config -> Config)]
opts =
    [ Option "I" ["ci"]         (ReqArg setCI "CI") "Confidence interval"
    , Option "G" ["no-gc"]      (NoArg setNoGC)     "Do not collect garbage between iterations"
    , Option "L" ["time-limit"] (ReqArg setTimeLimit "SECS") $
        "Min seconds for each benchmark run, default is "
        ++ show defaultTimeLimitNormal ++ " in normal mode, "
        ++ show defaultTimeLimitQuick ++ " in quick mode"
    , Option ""  ["min-samples"] (ReqArg setMinSamples "COUNT") $
        "Min no. of samples for each benchmark, default is "
        ++ show defaultMinSamplesNormal ++ " in normal mode, "
        ++ show defaultMinSamplesQuick ++ " in quick mode"
    , Option ""  ["min-duration"] (ReqArg setMinDuration "MILLISECS") $
        "Min duration for each sample, default is "
        ++ show defaultMinDuration ++ ", when 0 stops after first iteration"
    , Option ""  ["include-first-iter"] (NoArg setIncludeFirst) "Do not discard the measurement of the first iteration"
    , Option "q" ["quick"]      (NoArg setQuickMode) "Perform a quick measurement and report results without statistical analysis"
    , Option ""  ["measure-only"] (fileArg setMeasureOnly) "Just measure the benchmark and place the raw data in the given file"
    , Option ""  ["measure-with"] (fileArg setMeasureProg) "Perform measurements in a separate process using this program."
    , Option ""  ["resamples"]  (ReqArg setResamples "COUNT") "Number of boostrap resamples to perform"
    , Option ""  ["regress"]    (ReqArg setRegressions "RESP:PRED..") "Regressions to perform"
    , Option ""  ["raw"]        (fileArg setRaw) "File to write raw data to"
    , Option "o" ["output"]     (fileArg setOutput) "File to write report to"
    , Option ""  ["csvraw"]     (fileArg setCSVRaw) "File to write CSV measurements to"
    , Option ""  ["csv"]        (fileArg setCSV) "File to write CSV summary to"
    , Option ""  ["json"]       (fileArg setJSON) "File to write JSON summary to"
    , Option ""  ["junit"]      (fileArg setJUnit) "File to write JUnit summary to"
    , Option "v" ["verbosity"]  (ReqArg setVerbosity "LEVEL") "Verbosity level"
    , Option "t" ["template"]   (fileArg setTemplate) "Template to use for report"
    , Option "n" ["iters"]      (ReqArg setIters "ITERS") "Run benchmarks, don't analyse"
    , Option "m" ["match"]      (ReqArg setMatch "MATCH") $
        "Benchmark match style: prefix (default), exact, pattern (substring), "
        ++ "or ipattern (case insensitive)"
    , Option "l" ["list"]       (NoArg $ setMode List) "List benchmarks"
    , Option ""  ["version"]    (NoArg $ setMode Version) "Show version info"
    , Option "s" ["small"]      (NoArg $ setDisplayMode Condensed) "Set benchmark display to the minimum useful information"
    , Option "h" ["help"]       (NoArg $ setMode Help) "Show help"
    ]
  where
    fileArg f = ReqArg f "FILE"
    setCI s v = v { confInterval = Just $ range 0.001 0.999 s }
    setNoGC v = v { forceGC = False }
    setTimeLimit s v = v { timeLimit = Just $ range 0.0 86400 s }
    setMinSamples n v = v { minSamples = Just $ read n }
    setMinDuration ms v = v { minDuration = MilliSeconds $ read ms }
    setIncludeFirst v = v { includeFirstIter = True }
    setQuickMode v = v { quickMode = True }
    setMeasureOnly f v = v { measureOnly = Just f }
    setMeasureProg f v = v { measureWith = Just f }
    setResamples s v = v { resamples = range 1 1000000 s }
    setRegressions s v = v { regressions = regressParams s : regressions v }
    setRaw f v = v { rawDataFile = Just f }
    setOutput f v = v { reportFile = Just f }
    setCSV f v = v { csvFile = Just f }
    setCSVRaw f v = v { csvRawFile = Just f }
    setJSON f v = v { jsonFile = Just f }
    setJUnit f v = v { junitFile = Just f }
    setVerbosity s v = v { verbosity = toEnum (range 0 2 s) }
    setTemplate f v = v { template = f }
    setIters s v = v { iters = Just $ read s }
    setMatch s v =
        let m = case map toLower s of
                    "pfx"      -> Prefix
                    "prefix"   -> Prefix
                    "exact"    -> Exact
                    "pattern"  -> Pattern
                    "ipattern" -> IPattern
                    _          -> optionError ("unknown match type: " ++ s)
         in v { match = m }
    setMode m v = v { mode = m }
    setDisplayMode m v = v { displayMode = m }

-- FIXME
optionError :: String -> a
optionError s = error s

range :: (Show a, Read a, Ord a) => a -> a -> String -> a
range lo hi s = do
    case reads s of
        [(i, "")]
            | i >= lo && i <= hi -> i
            | otherwise          -> optionError $ show i ++ " is outside range " ++ show (lo,hi)
        _ -> optionError $ show s ++ " is not a number"

{-
Regression metrics (for use with --regress):
  time                     wall-clock time
  cpuTime                  CPU time
  cycles                   CPU cycles
  iters                    loop iterations
  allocated                (+RTS -T) bytes allocated
  numGcs                   (+RTS -T) number of garbage collections
  bytesCopied              (+RTS -T) number of bytes copied during GC
  mutatorWallSeconds       (+RTS -T) wall-clock time for mutator threads
  mutatorCpuSeconds        (+RTS -T) CPU time spent running mutator threads
  gcWallSeconds            (+RTS -T) wall-clock time spent doing GC
  gcCpuSeconds             (+RTS -T) CPU time spent doing GC
Benchmark self: FINISH

-- We sort not by name, but by likely frequency of use.
regressionHelp :: Chunk Doc
regressionHelp =
    fmap (text "Regression metrics (for use with --regress):" .$.) $
      tabulate [(text n,text d) | (n,(_,d)) <- map f measureKeys]
  where f k = (k, measureAccessors M.! k)
  -}

describe :: String
describe = usageInfo header opts

header :: String
header = "Microbenchmark suite - " ++ versionInfo

-- | A string describing the version of this benchmark (really, the
-- version of gauge that was used to build it).
versionInfo :: String
versionInfo = "built with gauge " ++ showVersion version

regressParams :: String -> ([String], String)
regressParams m
    | null r    = optionError "no responder specified"
    | null ps   = optionError "no predictors specified"
    | otherwise =
        let ret = (words . map repl . drop 1 $ ps, tidy r)
        in either optionError (const ret) $ uncurry validateAccessors ret
  where
      repl ','   = ' '
      repl c     = c
      tidy       = reverse . dropWhile isSpace . reverse . dropWhile isSpace
      (r,ps)     = break (==':') m

