{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:coverage-all #-}
module Plutus.Contracts.GameStateMachine.Coverage(
    covIndex
  ) where

import Ledger
import Plutus.Contract.Test.Coverage.Analysis
import Plutus.Contracts.GameStateMachine
import PlutusTx qualified
import PlutusTx.Code
import PlutusTx.Coverage
import PlutusTx.Prelude (check)

cc :: CompiledCode (GameParam -> GameState -> GameInput -> ScriptContext -> ())
cc = $$(PlutusTx.compile [|| \a b c d -> check (mkValidator a b c d) ||])

covIndex :: CoverageIndex
covIndex = computeRefinedCoverageIndex cc

