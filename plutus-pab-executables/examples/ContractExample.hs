{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module ContractExample(
    ContractExample(..)
    , handlers
    ) where

import Control.Monad.Freer
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import Data.Text (Text)
import GHC.Generics (Generic)
import Prettyprinter

import ContractExample.AtomicSwap qualified as Contracts.AtomicSwap
import ContractExample.IntegrationTest qualified as Contracts.IntegrationTest
import ContractExample.PayToWallet qualified as Contracts.PayToWallet
import ContractExample.WaitForTx qualified as Contracts.WaitForTx
import Data.OpenApi.Schema qualified as OpenApi
import Ledger (TxId)
import Plutus.Contracts.Currency qualified as Contracts.Currency
import Plutus.Contracts.Game qualified as Contracts.Game
import Plutus.Contracts.GameStateMachine qualified as Contracts.GameStateMachine
import Plutus.Contracts.PingPong qualified as Contracts.PingPong
import Plutus.Contracts.Prism.Mirror qualified as Contracts.Prism
import Plutus.Contracts.Prism.Unlock qualified as Contracts.Prism
import Plutus.Contracts.Uniswap qualified as Contracts.Uniswap
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (..), HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Simulator (SimulatorEffectHandlers)
import Plutus.PAB.Simulator qualified as Simulator

data ContractExample = UniswapInit
                     | UniswapOwner
                     | UniswapUser Contracts.Uniswap.Uniswap
                     | Game
                     | GameStateMachine
                     | PayToWallet
                     | AtomicSwap
                     | Currency
                     | PrismMirror
                     | PrismUnlockExchange
                     | PrismUnlockSto
                     | PingPong
                     | PingPongAuto -- ^ Variant of 'PingPong' that starts the initialise phase automatically
                     | WaitForTx TxId
                     | IntegrationTest -- ^ Contract that runs a number of transactions (no user input)
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

instance Pretty ContractExample where
    pretty = viaShow

instance HasDefinitions ContractExample where
    getDefinitions = [ UniswapInit
                     , UniswapOwner
                     , Game
                     , GameStateMachine
                     , PayToWallet
                     , AtomicSwap
                     , Currency
                     , PrismMirror
                     , PrismUnlockExchange
                     , PrismUnlockSto
                     , PingPong
                     , PingPongAuto
                     , IntegrationTest
                     ]
    getContract = getContractExample

getContractExample :: ContractExample -> SomeBuiltin
getContractExample = \case
    UniswapInit         -> SomeBuiltin Contracts.Uniswap.setupTokens
    UniswapUser us      -> SomeBuiltin $ Contracts.Uniswap.userEndpoints us
    UniswapOwner        -> SomeBuiltin Contracts.Uniswap.ownerEndpoint
    Game                -> SomeBuiltin (Contracts.Game.contract @Text)
    GameStateMachine    -> SomeBuiltin Contracts.GameStateMachine.contract
    PayToWallet         -> SomeBuiltin Contracts.PayToWallet.payToWallet
    AtomicSwap          -> SomeBuiltin Contracts.AtomicSwap.atomicSwap
    Currency            -> SomeBuiltin Contracts.Currency.mintCurrency
    PrismMirror         -> SomeBuiltin (Contracts.Prism.mirror @Contracts.Prism.MirrorSchema @())
    PrismUnlockExchange -> SomeBuiltin (Contracts.Prism.unlockExchange @() @Contracts.Prism.UnlockExchangeSchema)
    PrismUnlockSto      -> SomeBuiltin (Contracts.Prism.subscribeSTO @() @Contracts.Prism.STOSubscriberSchema)
    PingPong            -> SomeBuiltin Contracts.PingPong.simplePingPong
    PingPongAuto        -> SomeBuiltin Contracts.PingPong.simplePingPongAuto
    WaitForTx txi       -> SomeBuiltin (Contracts.WaitForTx.waitForTx txi)
    IntegrationTest     -> SomeBuiltin Contracts.IntegrationTest.run

handlers :: SimulatorEffectHandlers (Builtin ContractExample)
handlers =
    Simulator.mkSimulatorHandlers def
    $ interpret (contractHandler Builtin.handleBuiltin)
