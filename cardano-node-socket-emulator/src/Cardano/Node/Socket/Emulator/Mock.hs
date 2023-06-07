{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Node.Socket.Emulator.Mock where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, putMVar, takeMVar)
import Control.Lens (over, set, unto, view)
import Control.Monad (forever, void)
import Control.Monad.Freer (Eff, interpret, reinterpret, runM, send)
import Control.Monad.Freer.Extras.Log
import Control.Monad.Freer.Extras.Modify (handleZoomedState)
import Control.Monad.Freer.State qualified as Eff
import Control.Monad.Freer.Writer qualified as Eff
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Time.Units (Millisecond, toMicroseconds)
import Data.Time.Units.Extra ()
import Servant (NoContent (NoContent))

import Cardano.BM.Data.Trace (Trace)
import Cardano.Node.Emulator.Internal.Node (ChainEvent, Params (..), SlotConfig (SlotConfig, scSlotLength), currentSlot)
import Cardano.Node.Emulator.Internal.Node.Chain qualified as Chain
import Cardano.Node.Socket.Emulator.Chain (handleChain, handleControlChain)
import Cardano.Node.Socket.Emulator.Server qualified as Server
import Cardano.Node.Socket.Emulator.Types
import Plutus.Monitoring.Util qualified as LM

healthcheck :: Monad m => m NoContent
healthcheck = pure NoContent

consumeEventHistory :: MonadIO m => MVar AppState -> m [LogMessage ChainEvent]
consumeEventHistory stateVar =
    liftIO $ do
        oldState <- takeMVar stateVar
        let events = view eventHistory oldState
        let newState = set eventHistory mempty oldState
        putMVar stateVar newState
        pure events

-- | Run all chain effects in the IO Monad
runChainEffects
    :: Params
    -> MVar AppState
    -> Eff (NodeServerEffects IO) a
    -> IO ([LogMessage ChainEvent], a)
runChainEffects params stateVar eff = do
    oldAppState <- liftIO $ takeMVar stateVar
    ((a, newState), events) <- liftIO $
            processBlock eff
            & interpret (handleControlChain params)
            & interpret (handleChain params)
            & mergeState
            & Eff.runState oldAppState
            & toWriter
            & runM
    liftIO $ putMVar stateVar newState
    pure (events, a)
        where
            processBlock e = e >>= \r -> Chain.processBlock >> pure r

            mergeState = interpret (handleZoomedState chainState)

            toWriter = Eff.runWriter . reinterpret (handleLogWriter @ChainEvent @[LogMessage ChainEvent] (unto return))

processChainEffects ::
    Trace IO ChainEvent
    -> Params
    -> MVar AppState
    -> Eff (NodeServerEffects IO) a
    -> IO a
processChainEffects trace params stateVar eff = do
    (events, result) <- liftIO $ runChainEffects params stateVar eff
    LM.runLogEffects trace $ traverse_ (send . LMessage) events
    liftIO $
        modifyMVar_
            stateVar
            (\state -> pure $ over eventHistory (mappend events) state)
    pure result

-- | Calls 'addBlock' at the start of every slot, causing pending transactions
--   to be validated and added to the chain.
slotCoordinator ::
    SlotConfig
    -> Server.ServerHandler
    -> IO a
slotCoordinator sc@SlotConfig{scSlotLength} serverHandler = do
    forever $ do
        void $ Server.processBlock serverHandler
        newSlot <- currentSlot sc
        void $ Server.modifySlot (const newSlot) serverHandler
        liftIO $ threadDelay
               $ fromIntegral
               $ toMicroseconds (fromInteger scSlotLength :: Millisecond)
