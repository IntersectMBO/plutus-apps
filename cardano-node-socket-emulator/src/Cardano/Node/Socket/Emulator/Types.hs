{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-| This module exports data types for logging, events and configuration
-}
module Cardano.Node.Socket.Emulator.Types where

import Cardano.Node.Emulator.Internal.Node (ChainControlEffect, ChainEffect, ChainEvent, pNetworkId, testnet)
import Cardano.Node.Socket.Emulator.Chain (MockNodeServerChainState, fromEmulatorChainState)
import Control.Lens (makeLenses, view)
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg)
import Control.Monad.Freer.State qualified as Eff
import Control.Monad.IO.Class (MonadIO)
import Data.Default (def)
import Data.Either (fromRight)
import Data.Map qualified as Map
import Plutus.Contract.Trace qualified as Trace
import Wallet.Emulator (Wallet)
import Wallet.Emulator qualified as EM
import Wallet.Emulator.MultiAgent qualified as MultiAgent

type NodeServerEffects m
     = '[ ChainControlEffect
        , ChainEffect
        , Eff.State MockNodeServerChainState
        , Eff.State AppState
        , LogMsg ChainEvent
        , m]

-- | Application State
data AppState =
    AppState
        { _chainState   :: MockNodeServerChainState -- ^ blockchain state
        , _eventHistory :: [LogMessage ChainEvent] -- ^ history of all log messages
        }
    deriving (Show)

makeLenses 'AppState

-- | 'AppState' with an initial transaction that pays some Ada to
--   the wallets.
initialAppState :: MonadIO m => [Wallet] -> m AppState
initialAppState wallets = do
    initialState <- initialChainState (Trace.defaultDistFor wallets)
    pure $ AppState
        { _chainState = initialState
        , _eventHistory = mempty
        }

-- | 'ChainState' with initial values
initialChainState :: MonadIO m => Trace.InitialDistribution -> m MockNodeServerChainState
initialChainState =
    fromEmulatorChainState . view EM.chainState . fromRight (error "Can't initialise chain state") .
    MultiAgent.emulatorStateInitialDist (def {pNetworkId = testnet}) . Map.mapKeys EM.mockWalletPaymentPubKeyHash
