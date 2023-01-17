{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-

"inline" contracts from plutus-use-cases for testing

-}
module Plutus.PAB.Effects.Contract.ContractTest(
    TestContracts(..)
    ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Row
import GHC.Generics (Generic)
import Prettyprinter

import ContractExample.AtomicSwap qualified as Contracts.AtomicSwap
import ContractExample.PayToWallet qualified as Contracts.PayToWallet
import Data.Text.Extras (tshow)
import Plutus.Contract (awaitPromise)
import Plutus.Contracts.Currency qualified as Contracts.Currency
import Plutus.Contracts.GameStateMachine qualified as Contracts.GameStateMachine
import Plutus.Contracts.PingPong qualified as Contracts.PingPong
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Types (PABError (..))

data TestContracts = GameStateMachine | Currency | AtomicSwap | PayToWallet | PingPong
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty TestContracts where
    pretty = viaShow

instance HasDefinitions TestContracts where
    getDefinitions = [ GameStateMachine, Currency, AtomicSwap, PayToWallet, PingPong ]
    getContract = getTestContracts

getTestContracts :: TestContracts -> SomeBuiltin
getTestContracts = \case
    GameStateMachine -> SomeBuiltin game
    Currency         -> SomeBuiltin $ awaitPromise currency
    AtomicSwap       -> SomeBuiltin $ awaitPromise swp
    PayToWallet      -> SomeBuiltin $ awaitPromise payToWallet
    PingPong         -> SomeBuiltin pingPong
    where
        game = Contracts.GameStateMachine.contract
        currency = Contracts.Currency.mintCurrency
        swp = first tshow Contracts.AtomicSwap.atomicSwap
        payToWallet = Contracts.PayToWallet.payToWallet
        pingPong = Contracts.PingPong.combined
