{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Wallet.Emulator.Types(
    -- * Wallets
    Wallet.Emulator.Wallet.Wallet(..),
    Wallet.Emulator.Wallet.WalletId(..),
    Crypto.XPrv,
    Crypto.XPub,
    Wallet.Emulator.Wallet.mockWalletAddress,
    Wallet.Emulator.Wallet.mockWalletPaymentPubKey,
    Wallet.Emulator.Wallet.mockWalletPaymentPubKeyHash,
    Wallet.Emulator.Wallet.knownWallets,
    Wallet.Emulator.Wallet.knownWallet,
    Ledger.CardanoWallet.WalletNumber(..),
    Ledger.CardanoWallet.toWalletNumber,
    Wallet.Emulator.Wallet.fromWalletNumber,
    Ledger.CardanoWallet.MockWallet(..),
    Cardano.Node.Emulator.Chain.TxPool,
    -- * Emulator
    EmulatorEffs,
    Wallet.Emulator.MultiAgent.Assertion(OwnFundsEqual, IsValidated),
    Wallet.Emulator.MultiAgent.assert,
    Wallet.Emulator.MultiAgent.assertIsValidated,
    Plutus.Contract.Error.AssertionError(..),
    Plutus.Contract.Error.AsAssertionError(..),
    Wallet.Emulator.NodeClient.ChainClientNotification(..),
    Wallet.Emulator.MultiAgent.EmulatorEvent,
    Wallet.Emulator.MultiAgent.EmulatorEvent',
    Wallet.Emulator.MultiAgent.EmulatorTimeEvent(..),
    -- ** Wallet state
    Wallet.Emulator.Wallet.WalletState(..),
    Wallet.Emulator.Wallet.emptyWalletState,
    Wallet.Emulator.Wallet.ownPaymentPrivateKey,
    Wallet.Emulator.Wallet.ownAddress,
    -- ** Traces
    Wallet.Emulator.MultiAgent.walletAction,
    Wallet.Emulator.MultiAgent.assertion,
    Wallet.Emulator.MultiAgent.assertOwnFundsEq,
    Wallet.Emulator.MultiAgent.ownFundsEqual,
    -- * Emulator internals
    Wallet.Emulator.MultiAgent.EmulatorState(..),
    Wallet.Emulator.MultiAgent.emptyEmulatorState,
    Wallet.Emulator.MultiAgent.emulatorState,
    Wallet.Emulator.MultiAgent.emulatorStatePool,
    Wallet.Emulator.MultiAgent.emulatorStateInitialDist,
    Cardano.Node.Emulator.Chain.txPool,
    Wallet.Emulator.MultiAgent.walletStates,
    Cardano.Node.Emulator.Chain.index,
    Wallet.Emulator.MultiAgent.chainState,
    Cardano.Node.Emulator.Chain.chainCurrentSlot,
    processEmulated,
    Wallet.Emulator.MultiAgent.fundsDistribution,
    Wallet.Emulator.MultiAgent.emLog
    ) where

import Cardano.Crypto.Wallet qualified as Crypto
import Control.Lens hiding (index)
import Control.Monad.Freer (Eff, Member, interpret, reinterpret2, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras qualified as Eff
import Control.Monad.Freer.Extras.Log (LogMsg, mapLog)
import Control.Monad.Freer.State (State)

import Cardano.Node.Emulator.Params (Params)
import Plutus.ChainIndex (ChainIndexError)
import Wallet.API (WalletAPIError)

import Cardano.Node.Emulator.Chain (ChainControlEffect, ChainEffect, ChainEvent, ChainState, handleChain,
                                    handleControlChain)
import Cardano.Node.Emulator.Chain qualified
import Ledger.CardanoWallet qualified
import Plutus.Contract.Error (AssertionError)
import Plutus.Contract.Error qualified
import Wallet.Emulator.MultiAgent (EmulatorEvent', EmulatorState, MultiAgentControlEffect, MultiAgentEffect, chainEvent,
                                   chainState, handleMultiAgent, handleMultiAgentControl)
import Wallet.Emulator.MultiAgent qualified
import Wallet.Emulator.NodeClient qualified
import Wallet.Emulator.Wallet qualified

type EmulatorEffs = '[MultiAgentEffect, ChainEffect, ChainControlEffect]

processEmulated :: forall effs.
    ( Member (Error WalletAPIError) effs
    , Member (Error ChainIndexError) effs
    , Member (Error AssertionError) effs
    , Member (State EmulatorState) effs
    , Member (LogMsg EmulatorEvent') effs
    )
    => Params
    -> Eff (MultiAgentEffect ': MultiAgentControlEffect ': ChainEffect ': ChainControlEffect ': effs)
    ~> Eff effs
processEmulated params act =
    act
        & handleMultiAgent
        & handleMultiAgentControl
        & reinterpret2 @ChainEffect @(State ChainState) @(LogMsg ChainEvent) (handleChain params)
        & interpret (Eff.handleZoomedState chainState)
        & interpret (mapLog (review chainEvent))
        & reinterpret2 @ChainControlEffect @(State ChainState) @(LogMsg ChainEvent) (handleControlChain params)
        & interpret (Eff.handleZoomedState chainState)
        & interpret (mapLog (review chainEvent))
