{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
-- |
-- Module      : Gauge.Benchmark
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Constructing and running benchmarks.
-- To benchmark, an IO action or a pure function must be evaluated to normal
-- form (NF) or weak head normal form (WHNF). This library provides APIs to
-- reduce IO actions or pure functions to NF (`nf` or `nfAppIO`) or WHNF
-- (`whnf` or `whnfAppIO`).

module Gauge.Benchmark
    (
    -- * Benchmarkable
    -- $bench
      Benchmarkable(..)

    -- ** Constructing Benchmarkable
    , toBenchmarkable

    -- ** Benchmarking pure code
    -- $pure

    , nf
    , whnf

    -- ** Benchmarking IO actions

    , nfAppIO
    , whnfAppIO

    , nfIO
    , whnfIO

    -- ** Benchmarking with Environment
    , perBatchEnv
    , perBatchEnvWithCleanup
    , perRunEnv
    , perRunEnvWithCleanup

    -- * Benchmarks
    , Benchmark(..)

    -- ** Constructing Benchmarks
    , bench
    , bgroup

    -- ** Benchmarks with Environment
    , env
    , envWithCleanup

    -- * Listing benchmarks
    , benchNames

    -- * Running Benchmarks
    , runBenchmark
    , BenchmarkAnalysis(..)
    ) where

import Control.Applicative
import Control.DeepSeq (NFData(rnf))
import Control.Exception (bracket, catch, evaluate, finally)
import Control.Monad (foldM, void, when)
import Data.Int (Int64)
import Data.List (unfoldr)
import Gauge.IO.Printf (note, prolix)
import Gauge.Main.Options (Config(..), Verbosity(..))
import Gauge.Measurement
       (measure, getTime, secs, Measured(..), defaultMinSamplesNormal,
        defaultMinSamplesQuick, defaultTimeLimitNormal,
        defaultTimeLimitQuick)
import Gauge.Monad (Gauge, finallyGauge, askConfig, gaugeIO)
import Gauge.Time (MilliSeconds(..), milliSecondsToDouble, microSecondsToDouble)
import qualified Gauge.CSV as CSV
import qualified Gauge.Optional as Optional
import System.Directory (canonicalizePath, getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile)
import System.Mem (performGC)
import qualified Data.Vector as V
import System.Process (callProcess)
import Prelude

-------------------------------------------------------------------------------
-- Constructing benchmarkable
-------------------------------------------------------------------------------

-- $bench
--
-- A 'Benchmarkable' is the basic type which in turn is used to construct a
-- 'Benchmark'.  It is a container for code that can be benchmarked.  The value
-- contained inside a 'Benchmarkable' could be an IO action or a pure function.

-- | A pure function or impure action that can be benchmarked. The function to
-- be benchmarked is wrapped into a function ('runRepeatedly') that takes an
-- 'Int64' parameter which indicates the number of times to run the given
-- function or action.  The wrapper is constructed automatically by the APIs
-- provided in this library to construct 'Benchmarkable'.
--
-- When 'perRun' is not set then 'runRepeatedly' is invoked to perform all
-- iterations in one measurement interval.  When 'perRun' is set,
-- 'runRepeatedly' is always invoked with 1 iteration in one measurement
-- interval, before a measurement 'allocEnv' is invoked and after the
-- measurement 'cleanEnv' is invoked. The performance counters for each
-- iteration are then added together for all iterations.
data Benchmarkable = forall a . NFData a =>
    Benchmarkable
      { allocEnv :: Int64 -> IO a
      , cleanEnv :: Int64 -> a -> IO ()
      , runRepeatedly :: a -> Int64 -> IO ()
      , perRun :: Bool
      }

noop :: Monad m => a -> m ()
noop = const $ return ()
{-# INLINE noop #-}

-- | This is a low level function to construct a 'Benchmarkable' value from an
-- impure wrapper action, where the 'Int64' parameter dictates the number of
-- times the action wrapped inside would run. You would normally be using the
-- other higher level APIs rather than this function to construct a
-- benchmarkable.
toBenchmarkable :: (Int64 -> IO ()) -> Benchmarkable
toBenchmarkable f = Benchmarkable noop (const noop) (const f) False
{-# INLINE toBenchmarkable #-}

-- $pure
--
-- A pure computation is guaranteed to produce the same result every time.
-- Therefore, GHC may evaluate it just once and subsequently replace it with
-- the evaluated result. If we benchmark a pure value in a loop we may really
-- be measuring only one iteration and the rest of the iterations will be doing
-- nothing.
--
-- If we represent the computation being benchmarked as a function, we can
-- workaround this problem, we just need to keep one of the parameters in the
-- computation unknown and supply it as an argument to the function. When we
-- benchmark the computation we supply the function and the argument to the
-- benchmarking function, the argument is applied to the function at benchmark
-- run time. This way GHC would not evaluate the computation once and store the
-- result, it has to evaluate the function every time as the argument is not
-- statically known.
--
-- Suppose we want to benchmark the following pure function:
--
-- @
-- firstN :: Int -> [Int]
-- firstN k = take k [(0::Int)..]
-- @
--
-- We construct a benchmark evaluating it to NF as follows:
--
-- @
-- 'nf' firstN 1000
-- @
--
-- We can also evaluate a pure function to WHNF, however we must remember that
-- it only evaluates the result up to, well, WHNF. To naive eyes it might
-- /appear/ that the following code ought to benchmark the production of the
-- first 1000 list elements:
--
-- @
-- 'whnf' firstN 1000
-- @
--
-- Since this forces the expression to only WHNF, what this would /actually/
-- benchmark is merely how long it takes to produce the first list element!

pureFunc :: (b -> c) -> (a -> b) -> a -> Benchmarkable
pureFunc reduce f0 x0 = toBenchmarkable (go f0 x0)
  where go f x n
          | n <= 0    = return ()
          | otherwise = evaluate (reduce (f x)) >> go f x (n-1)
{-# INLINE pureFunc #-}

-- | Apply an argument to a function, and evaluate the result to weak
-- head normal form (WHNF).
whnf :: (a -> b) -> a -> Benchmarkable
whnf = pureFunc id
{-# INLINE whnf #-}

-- | Apply an argument to a function, and evaluate the result to
-- normal form (NF).
nf :: NFData b => (a -> b) -> a -> Benchmarkable
nf = pureFunc rnf
{-# INLINE nf #-}

impure :: (a -> b) -> IO a -> Int64 -> IO ()
impure strategy a = go
  where go n
          | n <= 0    = return ()
          | otherwise = a >>= (evaluate . strategy) >> go (n-1)
{-# INLINE impure #-}

-- | Perform an IO action, then evaluate its result to normal form (NF).  When
-- run in a loop during benchmarking, this may evaluate any pure component used
-- in the construction of the IO action only once. If this is not what you want
-- use `nfAppIO` instead.
nfIO :: NFData a => IO a -> Benchmarkable
nfIO = toBenchmarkable . impure rnf
{-# INLINE nfIO #-}

-- | Perform an action, then evaluate its result to weak head normal form
-- (WHNF).  This is useful for forcing an 'IO' action whose result is an
-- expression to be evaluated down to a more useful value.
--
-- When run in a loop during benchmarking, this may evaluate any pure component
-- used in the construction of the IO action only once. If this is not what you
-- want use `whnfAppIO` instead.
whnfIO :: IO a -> Benchmarkable
whnfIO = toBenchmarkable . impure id
{-# INLINE whnfIO #-}

-- | Construct and perform an IO computation, then evaluate its result to
-- normal form (NF).  This is an IO version of `nf`. It makes sure that any
-- pure component of the computation used to construct the IO computation is
-- evaluated every time (see the discussion before `nf`). It is safer to use
-- this function instead of `nfIO` unless you deliberately do not want to
-- evaluate the pure part of the computation every time. This is the function
-- that you would want to use almost always.  It can even be used to benchmark
-- pure computations by wrapping them in IO.
nfAppIO :: NFData b => (a -> IO b) -> a -> Benchmarkable
nfAppIO = impureFunc rnf
{-# INLINE nfAppIO #-}

-- | Construct and perform an IO computation, then evaluate its result to weak
-- head normal form (WHNF).  This is an IO version of `whnf`. It makes sure
-- that any pure component of the computation used to construct the IO
-- computation is evaluated every time (see the discussion before `nf`). It is
-- safer to use this function instead of `whnfIO` unless you deliberately do
-- not want to evaluate the pure part of the computation every time.
whnfAppIO :: (a -> IO b) -> a -> Benchmarkable
whnfAppIO = impureFunc id
{-# INLINE whnfAppIO #-}

impureFunc :: (b -> c) -> (a -> IO b) -> a -> Benchmarkable
impureFunc strategy f0 x0 = toBenchmarkable (go f0 x0)
  where go f x n
          | n <= 0    = return ()
          | otherwise = f x >>= evaluate . strategy >> go f x (n-1)
{-# INLINE impureFunc #-}

-------------------------------------------------------------------------------
-- Constructing Benchmarkable with Environment
-------------------------------------------------------------------------------

-- | Same as `perBatchEnv`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
perBatchEnvWithCleanup
    :: (NFData env, NFData b)
    => (Int64 -> IO env)
    -- ^ Create an environment for a batch of N runs. The environment will be
    -- evaluated to normal form before running.
    -> (Int64 -> env -> IO ())
    -- ^ Clean up the created environment.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perBatchEnvWithCleanup alloc clean work
    = Benchmarkable alloc clean (impure rnf . work) False

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- batch of runs of the benchmarkable.
--
-- The environment is evaluated to normal form before the benchmark is run.
--
-- When using 'whnf', 'whnfIO', etc. Gauge creates a 'Benchmarkable'
-- whichs runs a batch of @N@ repeat runs of that expressions. Gauge may
-- run any number of these batches to get accurate measurements. Environments
-- created by 'env' and 'envWithCleanup', are shared across all these batches
-- of runs.
--
-- This is fine for simple benchmarks on static input, but when benchmarking
-- IO operations where these operations can modify (and especially grow) the
-- environment this means that later batches might have their accuracy effected
-- due to longer, for example, longer garbage collection pauses.
--
-- An example: Suppose we want to benchmark writing to a Chan, if we allocate
-- the Chan using environment and our benchmark consists of @writeChan env ()@,
-- the contents and thus size of the Chan will grow with every repeat. If
-- Gauge runs a 1,000 batches of 1,000 repeats, the result is that the
-- channel will have 999,000 items in it by the time the last batch is run.
-- Since GHC GC has to copy the live set for every major GC this means our last
-- set of writes will suffer a lot of noise of the previous repeats.
--
-- By allocating a fresh environment for every batch of runs this function
-- should eliminate this effect.
perBatchEnv
    :: (NFData env, NFData b)
    => (Int64 -> IO env)
    -- ^ Create an environment for a batch of N runs. The environment will be
    -- evaluated to normal form before running.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly generated environment.
    -> Benchmarkable
perBatchEnv alloc = perBatchEnvWithCleanup alloc (const noop)

-- | Same as `perRunEnv`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
perRunEnvWithCleanup
    :: (NFData env, NFData b)
    => IO env
    -- ^ Action that creates the environment for a single run.
    -> (env -> IO ())
    -- ^ Clean up the created environment.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly genereted environment.
    -> Benchmarkable
perRunEnvWithCleanup alloc clean work = bm { perRun = True }
  where
    bm = perBatchEnvWithCleanup (const alloc) (const clean) work

-- | Create a Benchmarkable where a fresh environment is allocated for every
-- run of the operation to benchmark. This is useful for benchmarking mutable
-- operations that need a fresh environment, such as sorting a mutable Vector.
--
-- As with 'env' and 'perBatchEnv' the environment is evaluated to normal form
-- before the benchmark is run.
--
-- This introduces extra noise and result in reduce accuracy compared to other
-- Gauge benchmarks. But allows easier benchmarking for mutable operations
-- than was previously possible.
perRunEnv
    :: (NFData env, NFData b)
    => IO env
    -- ^ Action that creates the environment for a single run.
    -> (env -> IO b)
    -- ^ Function returning the IO action that should be benchmarked with the
    -- newly genereted environment.
    -> Benchmarkable
perRunEnv alloc = perRunEnvWithCleanup alloc noop

-------------------------------------------------------------------------------
-- Constructing benchmarks
-------------------------------------------------------------------------------

-- | Specification of a collection of benchmarks and environments. A
-- benchmark may consist of:
--
-- * An environment that creates input data for benchmarks, created
--   with 'env'.
--
-- * A single 'Benchmarkable' item with a name, created with 'bench'.
--
-- * A (possibly nested) group of 'Benchmark's, created with 'bgroup'.
data Benchmark where
    Environment  :: NFData env
                 => IO env -> (env -> IO a) -> (env -> Benchmark) -> Benchmark
    Benchmark    :: String -> Benchmarkable -> Benchmark
    BenchGroup   :: String -> [Benchmark] -> Benchmark

instance Show Benchmark where
    show (Environment _ _ b) = "Environment _ _" ++ show (b undefined)
    show (Benchmark d _)   = "Benchmark " ++ show d
    show (BenchGroup d _)  = "BenchGroup " ++ show d

-- | Create a single benchmark.
bench :: String                 -- ^ A name to identify the benchmark.
      -> Benchmarkable          -- ^ An activity to be benchmarked.
      -> Benchmark
bench = Benchmark

-- | Group several benchmarks together under a common name.
bgroup :: String                -- ^ A name to identify the group of benchmarks.
       -> [Benchmark]           -- ^ Benchmarks to group under this name.
       -> Benchmark
bgroup = BenchGroup

-------------------------------------------------------------------------------
-- Constructing Benchmarks with Environment
-------------------------------------------------------------------------------

-- | Run a benchmark (or collection of benchmarks) in the given
-- environment.  The purpose of an environment is to lazily create
-- input data to pass to the functions that will be benchmarked.
--
-- A common example of environment data is input that is read from a
-- file.  Another is a large data structure constructed in-place.
--
-- By deferring the creation of an environment when its associated
-- benchmarks need the its, we avoid two problems that this strategy
-- caused:
--
-- * Memory pressure distorted the results of unrelated benchmarks.
--   If one benchmark needed e.g. a gigabyte-sized input, it would
--   force the garbage collector to do extra work when running some
--   other benchmark that had no use for that input.  Since the data
--   created by an environment is only available when it is in scope,
--   it should be garbage collected before other benchmarks are run.
--
-- * The time cost of generating all needed inputs could be
--   significant in cases where no inputs (or just a few) were really
--   needed.  This occurred often, for instance when just one out of a
--   large suite of benchmarks was run, or when a user would list the
--   collection of benchmarks without running any.
--
-- __Creation.__ An environment is created right before its related
-- benchmarks are run.  The 'IO' action that creates the environment
-- is run, then the newly created environment is evaluated to normal
-- form (hence the 'NFData' constraint) before being passed to the
-- function that receives the environment.
--
-- __Complex environments.__ If you need to create an environment that
-- contains multiple values, simply pack the values into a tuple.
--
-- __Lazy pattern matching.__ In situations where a \"real\"
-- environment is not needed, e.g. if a list of benchmark names is
-- being generated, @undefined@ will be passed to the function that
-- receives the environment.  This avoids the overhead of generating
-- an environment that will not actually be used.
--
-- The function that receives the environment must use lazy pattern
-- matching to deconstruct the tuple, as use of strict pattern
-- matching will cause a crash if @undefined@ is passed in.
--
-- __Example.__ This program runs benchmarks in an environment that
-- contains two values.  The first value is the contents of a text
-- file; the second is a string.  Pay attention to the use of a lazy
-- pattern to deconstruct the tuple in the function that returns the
-- benchmarks to be run.
--
-- > setupEnv = do
-- >   let small = replicate 1000 (1 :: Int)
-- >   big <- map length . words <$> readFile "/usr/dict/words"
-- >   return (small, big)
-- >
-- > main = defaultMain [
-- >    -- notice the lazy pattern match here!
-- >    env setupEnv $ \ ~(small,big) -> bgroup "main" [
-- >    bgroup "small" [
-- >      bench "length" $ whnf length small
-- >    , bench "length . filter" $ whnf (length . filter (==1)) small
-- >    ]
-- >  ,  bgroup "big" [
-- >      bench "length" $ whnf length big
-- >    , bench "length . filter" $ whnf (length . filter (==1)) big
-- >    ]
-- >  ] ]
--
-- __Discussion.__ The environment created in the example above is
-- intentionally /not/ ideal.  As Haskell's scoping rules suggest, the
-- variable @big@ is in scope for the benchmarks that use only
-- @small@.  It would be better to create a separate environment for
-- @big@, so that it will not be kept alive while the unrelated
-- benchmarks are being run.
env :: NFData env =>
       IO env
    -- ^ Create the environment.  The environment will be evaluated to
    -- normal form before being passed to the benchmark.
    -> (env -> Benchmark)
    -- ^ Take the newly created environment and make it available to
    -- the given benchmarks.
    -> Benchmark
env alloc = Environment alloc noop

-- | Same as `env`, but but allows for an additional callback
-- to clean up the environment. Resource clean up is exception safe, that is,
-- it runs even if the 'Benchmark' throws an exception.
envWithCleanup
    :: NFData env
    => IO env
    -- ^ Create the environment.  The environment will be evaluated to
    -- normal form before being passed to the benchmark.
    -> (env -> IO a)
    -- ^ Clean up the created environment.
    -> (env -> Benchmark)
    -- ^ Take the newly created environment and make it available to
    -- the given benchmarks.
    -> Benchmark
envWithCleanup = Environment

-------------------------------------------------------------------------------
-- Listing Benchmarks
-------------------------------------------------------------------------------

-- | Add the given prefix to a name.  If the prefix is empty, the name
-- is returned unmodified.  Otherwise, the prefix and name are
-- separated by a @\'\/\'@ character.
addPrefix :: String             -- ^ Prefix.
          -> String             -- ^ Name.
          -> String
addPrefix ""  desc = desc
addPrefix pfx desc = pfx ++ '/' : desc

-- | Retrieve the names of all benchmarks.  Grouped benchmarks are
-- prefixed with the name of the group they're in.
benchNames :: Benchmark -> [String]
benchNames (Environment _ _ b) = benchNames (b undefined)
benchNames (Benchmark d _)   = [d]
benchNames (BenchGroup d bs) = map (addPrefix d) . concatMap benchNames $ bs

-------------------------------------------------------------------------------
-- Running benchmarkable
-------------------------------------------------------------------------------

-- | Take a 'Benchmarkable', number of iterations, a function to combine the
-- results of multiple iterations and a measurement function to measure the
-- stats over a number of iterations.
iterateBenchmarkable :: Benchmarkable
                 -> Int64
                 -> (a -> a -> a)
                 -> (Int64 -> IO () -> IO a)
                 -> IO a
iterateBenchmarkable Benchmarkable{..} i comb f
    | perRun = work >>= go (i - 1)
    | otherwise = work
  where
    go 0 result = return result
    go !n !result = work >>= go (n - 1) . comb result

    count | perRun = 1
          | otherwise = i

    work = do
        env0 <- allocEnv count
        let clean = cleanEnv count env0
            run = runRepeatedly env0 count

        clean `seq` run `seq` evaluate $ rnf env0

        performGC
        f count run `finally` clean <* performGC
    {-# INLINE work #-}
{-# INLINE iterateBenchmarkable #-}

iterateBenchmarkable_ :: Benchmarkable -> Int64 -> IO ()
iterateBenchmarkable_ bm i = iterateBenchmarkable bm i (\() () -> ()) (const id)
{-# INLINE iterateBenchmarkable_ #-}

series :: Double -> Maybe (Int64, Double)
series k = Just (truncate l, l)
  where l = k * 1.05

-- Our series starts its growth very slowly when we begin at 1, so we
-- eliminate repeated values.
squish :: (Eq a) => [a] -> [a]
squish ys = foldr go [] ys
  where go x xs = x : dropWhile (==x) xs

-- | Run a single benchmark, and return measurements collected while executing
-- it, along with the amount of time the measurement process took. The
-- benchmark will not terminate until we reach all the minimum bounds
-- specified. If the minimum bounds are satisfied, the benchmark will terminate
-- as soon as we reach any of the maximums.
runBenchmarkable' :: Benchmarkable
             -> MilliSeconds
             -- ^ Minimum sample duration in ms.
             -> Int
             -- ^ Minimum number of samples.
             -> Double
             -- ^ Upper bound on how long the benchmarking process
             -- should take.
             -> IO (V.Vector Measured, Double, Int64)
runBenchmarkable' bm minDuration minSamples timeLimit = do
  start <- performGC >> getTime
  let loop [] !_ _ = error "unpossible!"
      loop (iters:niters) iTotal acc = do
        endTime <- getTime
        if length acc >= minSamples && endTime - start >= timeLimit
          then do
            let !v = V.reverse (V.fromList acc)
            return (v, endTime - start, iTotal)
          else do
               m <- measure (iterateBenchmarkable bm iters)
               if measTime m >= milliSecondsToDouble minDuration
               then loop niters (iTotal + iters) (m:acc)
               else loop niters (iTotal + iters) (acc)
  loop (squish (unfoldr series 1)) 0 []

withSystemTempFile
  :: String   -- ^ File name template
  -> (FilePath -> IO a) -- ^ Callback that can use the file
  -> IO a
withSystemTempFile template action = do
  tmpDir <- getTemporaryDirectory >>= canonicalizePath
  withTempFile tmpDir
  where
  withTempFile tmpDir = bracket
    (openTempFile tmpDir template >>= \(f, h) -> hClose h >> return f)
    (\name -> ignoringIOErrors (removeFile name))
    (action)
  ignoringIOErrors act = act `catch` (\e -> const (return ()) (e :: IOError))

bmTimeLimit :: Config -> Double
bmTimeLimit  Config {..} =
    let def = if quickMode
              then defaultTimeLimitQuick
              else defaultTimeLimitNormal
    in maybe def id timeLimit

bmMinSamples :: Config -> Int
bmMinSamples Config {..} =
    let def = if quickMode
              then defaultMinSamplesQuick
              else defaultMinSamplesNormal
    in maybe def id minSamples

-- | Run a single benchmark measurement in a separate process.
runBenchmarkIsolated :: Config -> String -> String -> IO (V.Vector Measured)
runBenchmarkIsolated cfg prog desc =
    withSystemTempFile "gauge-quarantine" $ \file -> do
      -- XXX This is dependent on option names, if the option names change this
      -- will break.
      let MilliSeconds ms = minDuration cfg
      callProcess prog ([ "--time-limit", show (bmTimeLimit cfg)
                        , "--min-duration", show ms
                        , "--min-samples", show (bmMinSamples cfg)
                        , "--match", "exact"
                        , "--measure-only", file, desc
                        ] ++ if (quickMode cfg) then ["--quick"] else [])
      meas <- readFile file >>= return . read
      writeMeas (csvRawFile cfg) desc meas
      return meas

-- | Run a single benchmarkable and return the result.
runBenchmarkable :: String -> Benchmarkable -> Gauge (V.Vector Measured)
runBenchmarkable desc bm = do
    cfg@Config{..} <- askConfig
    case measureWith of
        Just prog -> gaugeIO $ runBenchmarkIsolated cfg prog desc
        Nothing   -> gaugeIO $ runNormal cfg
  where
    runNormal cfg@Config{..} = do
        _ <- note "benchmarking %s ... " desc
        i0 <- if includeFirstIter
                then return 0
                else iterateBenchmarkable_ bm 1 >> return 1
        let limit = bmTimeLimit cfg
        (meas, timeTaken, i) <- runBenchmarkable' bm minDuration (bmMinSamples cfg) limit
        when ((verbosity == Verbose || not quickMode) && timeTaken > limit * 1.25) .
            void $ prolix "took %s, total %d iterations\n" (secs timeTaken) (i0 + i)
        writeMeas csvRawFile desc meas
        return meas

writeMeas :: Maybe FilePath -> String -> V.Vector Measured -> IO ()
writeMeas fp desc meas = V.forM_ meas $ \m ->
    CSV.write fp $ CSV.Row
        [ CSV.string   $ desc
        , CSV.integral $ measIters m
        , CSV.float    $ measTime m
        , CSV.integral $ measCycles m
        , CSV.float    $ measCpuTime m
        , opt (CSV.float . microSecondsToDouble) $ measUtime m
        , opt (CSV.float . microSecondsToDouble) $ measStime m
        , opt CSV.integral $ measMaxrss m
        , opt CSV.integral $ measMinflt m
        , opt CSV.integral $ measMajflt m
        , opt CSV.integral $ measNvcsw m
        , opt CSV.integral $ measNivcsw m
        , opt CSV.integral $ measAllocated m
        , opt CSV.integral $ measNumGcs m
        , opt CSV.integral $ measBytesCopied m
        , opt CSV.float    $ measMutatorWallSeconds m
        , opt CSV.float    $ measMutatorCpuSeconds m
        , opt CSV.float    $ measGcWallSeconds m
        , opt CSV.float    $ measGcCpuSeconds m
        ]
  where
    -- an optional CSV field is either the empty string or the normal field value
    opt :: Optional.OptionalTag a => (a -> CSV.Field) -> Optional.Optional a -> CSV.Field
    opt f = maybe (CSV.string "") f . Optional.toMaybe

-------------------------------------------------------------------------------
-- Running benchmarks
-------------------------------------------------------------------------------

-- | The function to run after measurement
data BenchmarkAnalysis =
      forall a . BenchmarkNormal (String -> V.Vector Measured -> Gauge a)
    | BenchmarkIters Int64

-- | Run benchmarkables, selected by a given selector function, under a given
-- benchmark and analyse the output using the given analysis function.
runBenchmark :: (String -> Bool) -- ^ Select benchmarks by name.
             -> Benchmark
             -> BenchmarkAnalysis -- ^ Analysis function
             -> Gauge ()
runBenchmark selector bs analyse =
    for selector bs $ \_idx desc bm ->
        case analyse of
            BenchmarkNormal step ->
                runBenchmarkable desc bm >>= step desc >>= \_ -> return ()
            BenchmarkIters iters -> do
                _ <- note "benchmarking %s\r" desc
                gaugeIO $ iterateBenchmarkable_ bm iters

-- | Iterate over benchmarks.
for :: (String -> Bool)
    -> Benchmark
    -> (Int -> String -> Benchmarkable -> Gauge ())
    -> Gauge ()
for select bs0 handle = go (0::Int) ("", bs0) >> return ()
  where
    go !idx (pfx, Environment mkenv cleanenv mkbench)
      | shouldRun pfx mkbench = do
        e <- gaugeIO $ do
          ee <- mkenv
          evaluate (rnf ee)
          return ee
        go idx (pfx, mkbench e) `finallyGauge` gaugeIO (cleanenv e)
      | otherwise = return idx
    go idx (pfx, Benchmark desc b)
      | select desc' = do handle idx desc' b; return $! idx + 1
      | otherwise    = return idx
      where desc' = addPrefix pfx desc
    go idx (pfx, BenchGroup desc bs) =
      foldM go idx [(addPrefix pfx desc, b) | b <- bs]

    shouldRun :: forall a. String -> (a -> Benchmark) -> Bool
    shouldRun pfx mkbench =
      any (select . addPrefix pfx) . benchNames . mkbench $
      error "Gauge.env could not determine the list of your benchmarks since they force the environment (see the documentation for details)"

{-
-- | Write summary JUnit file (if applicable)
junit :: [Report] -> Gauge ()
junit rs
  = do junitOpt <- asks junitFile
       case junitOpt of
         Just fn -> liftIO $ writeFile fn msg
         Nothing -> return ()
  where
    msg = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" ++
          printf "<testsuite name=\"Gauge benchmarks\" tests=\"%d\">\n"
          (length rs) ++
          concatMap single rs ++
          "</testsuite>\n"
    single Report{..} = printf "  <testcase name=\"%s\" time=\"%f\" />\n"
               (attrEsc reportName) (estPoint $ anMean $ reportAnalysis)
    attrEsc = concatMap esc
      where
        esc '\'' = "&apos;"
        esc '"'  = "&quot;"
        esc '<'  = "&lt;"
        esc '>'  = "&gt;"
        esc '&'  = "&amp;"
        esc c    = [c]
-}
