{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

import           System.Timeout (timeout)
import           Data.Word
import           GHC.Exts (IsList(..))

import qualified Gauge as C
import           Gauge.Main.Options
import           Gauge.Benchmark (bench, bgroup, env, whnf)

import           Basement.Compat.Base ((<>))
import           Foundation.Check
import           Foundation.Check.Main

fib :: Int -> Int
fib = sum . go
  where go 0 = [0]
        go 1 = [1]
        go n = go (n-1) ++ go (n-2)

withSpecialMain :: Bool -> Bool -> [C.Benchmark] -> IO ()
withSpecialMain useVerbose useQuick = C.defaultMainWith cfg
  where
    cfg = defaultConfig
            { rawDataFile = Just "sanity.dat"
            , jsonFile    = Just "sanity.json"
            , csvFile     = Just "sanity.csv"
            , reportFile  = Just "sanity.html"
            , junitFile   = Just "sanity.junit"
            , verbosity   = if useVerbose then Verbose else verbosity defaultConfig
            , quickMode   = useQuick
            }

sanity :: Bool -> Bool -> Check ()
sanity useVerbose useQuick = do
    let tooLong = 30
    wat <- pick "run-program" $ timeout (tooLong * 1000000) $ withSpecialMain useVerbose useQuick
            [ bgroup "fib"
                [ bench "fib 10" $ whnf fib 10
                , bench "fib 22" $ whnf fib 22
                ]
            , env (return (replicate 1024 0 :: [Word8])) $ \xs ->
                bgroup "length . filter"
                    [ bench "string" $ whnf (length . filter (==0)) xs
                    -- , env (return (B.pack xs)) $ \bs -> bench "uarray" $ whnf (B.length . B.filter (==0)) bs
                    ]
            ]

    validate ("not killed for running longer than " <> fromList (show tooLong) <> " seconds") $
        wat === Just ()

main :: IO ()
main = defaultMain $ Group "gauge-sanity"
    [ CheckPlan "normal" $ sanity False False
    , CheckPlan "verbose" $ sanity True False
    , CheckPlan "quick" $ sanity False True
    , CheckPlan "verbose-quick" $ sanity True True
    ]
