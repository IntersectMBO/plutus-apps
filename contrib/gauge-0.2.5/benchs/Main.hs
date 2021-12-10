{-# LANGUAGE BangPatterns #-}
module Main where

import Gauge
import System.IO.Unsafe
import Control.Applicative
import Control.Concurrent
import Control.Exception

delayed :: (a -> b) -> a -> b
delayed f a = unsafePerformIO $ do
    !b <- evaluate (f a)
    threadDelay 10000
    pure b
{-# NOINLINE delayed #-}

main = defaultMain
    [ bench "identity" $ nf (map (+ 1)) [1,2,3 :: Int]
    , bench "slow" $ nf (map (\i -> delayed (+ 1))) [1..10::Int]
    ]
