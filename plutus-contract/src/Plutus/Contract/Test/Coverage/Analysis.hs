{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Plutus.Contract.Test.Coverage.Analysis
  ( computeRefinedCoverageIndex
  ) where

import Control.Lens

import Data.Map qualified as Map
import Data.Set qualified as Set

import PlutusCore.Default
import PlutusTx.Code
import PlutusTx.Coverage

import Plutus.Contract.Test.Coverage.Analysis.Interpreter

computeRefinedCoverageIndex :: CompiledCodeIn DefaultUni DefaultFun a -> CoverageIndex
computeRefinedCoverageIndex cc =
    foldr (flip addCoverageMetadata IgnoredAnnotation) covIdx (Set.toList ignoredLocs)
  where
    covIdx        = getCovIdx cc
    importantLocs = allNonFailLocations cc
    ignoredLocs   = covIdx ^. coverageMetadata . to Map.keysSet . to (`Set.difference` importantLocs)
