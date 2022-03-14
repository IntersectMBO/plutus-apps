{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module ErrorHandlingSimulations where

import Data.Text (Text)
import ErrorHandling (registeredKnownCurrencies)
import Playground.Types (Simulation (Simulation), SimulatorAction, simulationActions, simulationId, simulationName,
                         simulationWallets)
import SimulationUtils (callEndpoint, simulatorWallet)
import Wallet.Emulator.Types (WalletNumber (..))

simulations :: [Simulation]
simulations = [throwCatch]
  where
    wallet1 = WalletNumber 1
    wallet2 = WalletNumber 2
    simulationWallets =
        simulatorWallet registeredKnownCurrencies 100_000_000 <$> [wallet1, wallet2]
    throwCatch =
        Simulation
            { simulationName = "Throw/Catch"
            , simulationId = 1
            , simulationWallets
            , simulationActions = [throwError wallet1, catchError wallet2]
            }

throwError :: WalletNumber -> SimulatorAction
throwError caller = callEndpoint @Text caller "throwError" "Hello"

catchError :: WalletNumber -> SimulatorAction
catchError caller = callEndpoint @Text caller "catchError" "World"
