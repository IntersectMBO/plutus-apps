{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-

A 'Simulator' for the test contracts

-}
module Plutus.PAB.Simulator.Test(runSimulation, runSimulationWithParams) where

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
runSimulation = runSimulationWithParams params
 where
    params :: Params
    params = increaseTransactionLimits . increaseTransactionLimits
           $ def { pSlotConfig = def { scSlotLength = 1 } }

-- | Run the PAB simulator with the test contracts with provided params
runSimulationWithParams :: Params -> Simulation (Builtin TestContracts) a -> IO (Either PABError a)
runSimulationWithParams params = runSimulationWith (simulatorHandlers params)

-- | 'EffectHandlers' for running the PAB as a simulator (no connectivity to
--   out-of-process services such as wallet backend, node, etc.)
simulatorHandlers :: Params -> EffectHandlers (Builtin TestContracts) (SimulatorState (Builtin TestContracts))
simulatorHandlers params = mkSimulatorHandlers params handler
  where
    handler :: SimulatorContractHandler (Builtin TestContracts)
    handler = interpret (contractHandler handleBuiltin)
