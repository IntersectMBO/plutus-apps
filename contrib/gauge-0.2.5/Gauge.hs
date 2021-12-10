{-# LANGUAGE CPP             #-}
-- |
-- Module      : Gauge
-- Copyright   : (c) 2009-2014 Bryan O'Sullivan
--
-- License     : BSD-style
-- Maintainer  : bos@serpentine.com
-- Stability   : experimental
-- Portability : GHC
--
-- Fast and reliable micro benchmarking.

module Gauge
    ( module Gauge.Benchmark
    , module Gauge.Main
    , module Gauge.Main.Options
    ) where

import Gauge.Benchmark
import Gauge.Main
import Gauge.Main.Options
