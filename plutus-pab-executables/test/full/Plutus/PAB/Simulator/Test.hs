{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-

A 'Simulator' for the test contracts

-}
module Plutus.PAB.Simulator.Test(runSimulation) where

import Control.Monad.Freer (interpret)
import Data.Default (Default (def))
import Ledger.Params (Params (pSlotConfig), increaseTransactionLimits)
import Ledger.TimeSlot (SlotConfig (..))
import Plutus.PAB.Core (EffectHandlers)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), handleBuiltin)
import Plutus.PAB.Effects.Contract.ContractTest (TestContracts (..))
import Plutus.PAB.Simulator (Simulation, SimulatorContractHandler, SimulatorState, mkSimulatorHandlers,
                             runSimulationWith)
import Plutus.PAB.Types (PABError)

-- | Run the PAB simulator with the test contracts
runSimulation :: Simulation (Builtin TestContracts) a -> IO (Either PABError a)
runSimulation = runSimulationWith simulatorHandlers

-- | 'EffectHandlers' for running the PAB as a simulator (no connectivity to
--   out-of-process services such as wallet backend, node, etc.)
simulatorHandlers :: EffectHandlers (Builtin TestContracts) (SimulatorState (Builtin TestContracts))
simulatorHandlers = mkSimulatorHandlers params handler
  where
    params :: Params
    params = increaseTransactionLimits $ def { pSlotConfig = def { scSlotLength = 1 } }

    handler :: SimulatorContractHandler (Builtin TestContracts)
    handler = interpret (contractHandler handleBuiltin)
