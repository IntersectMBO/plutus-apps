{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}

module HelloWorldSimulations where

import HelloWorld (registeredKnownCurrencies)
import Playground.Types (ContractCall (AddBlocks), Simulation (Simulation), simulationActions, simulationId,
                         simulationName, simulationWallets)
import SimulationUtils (simulatorWallet)
import Wallet.Emulator.Types (WalletNumber (..))

simulations :: [Simulation]
simulations = [helloWorld]
  where
    wallet1 = WalletNumber 1
    wallet2 = WalletNumber 2
    simulationWallets =
        simulatorWallet registeredKnownCurrencies 100_000_000 <$>
        [wallet1, wallet2]
    helloWorld =
        Simulation
            { simulationName = "Hello, world"
            , simulationId = 1
            , simulationWallets
            , simulationActions = [ AddBlocks 1 ]
            }
