{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Plutus.Contract.Test.Coverage.Analysis
  ( computeRefinedCoverageIndex
  , refinedCoverageIndex
  ) where

import Control.Lens

import Data.Map qualified as Map
import Data.Set qualified as Set

import PlutusCore.Default
import PlutusTx.Code
import PlutusTx.Coverage

import Plutus.Contract.Test.Coverage.Analysis.Interpreter

import Language.Haskell.TH

unsafeIgnoreLocationInCoverageIndex :: String -> Int -> CoverageIndex -> CoverageIndex
unsafeIgnoreLocationInCoverageIndex file line =
  over coverageMetadata $ Map.filterWithKey (\k _ -> not $ ignore k)
  where
    ignore (CoverLocation loc') = ignoreLoc loc'
    ignore (CoverBool loc' _)   = ignoreLoc loc'
    ignoreLoc loc = view covLocFile loc == file
                 && view covLocStartLine loc `elem` map (+ line) [-1, 0, 1]

refinedCoverageIndex :: Q Exp
refinedCoverageIndex = do
  loc <- location
  let fn = loc_filename loc
      st = fst $ loc_start loc
  [| unsafeIgnoreLocationInCoverageIndex fn st . computeRefinedCoverageIndex |]

computeRefinedCoverageIndex :: CompiledCodeIn DefaultUni DefaultFun a -> CoverageIndex
computeRefinedCoverageIndex cc =
    foldr (flip addCoverageMetadata IgnoredAnnotation) covIdx (Set.toList ignoredLocs)
  where
    covIdx        = getCovIdx cc
    importantLocs = allNonFailLocations cc
    ignoredLocs   = covIdx ^. coverageMetadata . to Map.keysSet . to (`Set.difference` importantLocs)
