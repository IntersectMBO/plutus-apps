{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Cardano.Node.Mock where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, putMVar, takeMVar)
import Control.Lens (over, set, unto, view)
import Control.Monad (forever, void)
import Control.Monad.Freer (Eff, LastMember, Member, interpret, reinterpret, runM)
import Control.Monad.Freer.Extras.Log
import Control.Monad.Freer.Extras.Modify (handleZoomedState)
import Control.Monad.Freer.Reader (Reader)
import Control.Monad.Freer.Reader qualified as Eff
import Control.Monad.Freer.State qualified as Eff
import Control.Monad.Freer.Writer qualified as Eff
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Time.Units (Millisecond, toMicroseconds)
import Data.Time.Units.Extra ()
import Servant (NoContent (NoContent))

import Cardano.Api qualified as C
import Cardano.BM.Data.Trace (Trace)
import Cardano.Chain (handleChain, handleControlChain)
import Cardano.Node.Emulator.Chain qualified as Chain
import Cardano.Node.Emulator.Params (Params (..))
import Cardano.Node.Emulator.TimeSlot (SlotConfig (SlotConfig, scSlotLength), currentSlot)
import Cardano.Node.Types
import Cardano.Protocol.Socket.Mock.Client qualified as Client
import Cardano.Protocol.Socket.Mock.Server qualified as Server
import Plutus.PAB.Arbitrary ()
import Plutus.PAB.Monitoring.Monitoring qualified as LM

healthcheck :: Monad m => m NoContent
healthcheck = pure NoContent

consumeEventHistory :: MonadIO m => MVar AppState -> m [LogMessage PABServerLogMsg]
consumeEventHistory stateVar =
    liftIO $ do
        oldState <- takeMVar stateVar
        let events = view eventHistory oldState
        let newState = set eventHistory mempty oldState
        putMVar stateVar newState
        pure events

addTx ::
    ( Member (LogMsg PABServerLogMsg) effs
    , Member (Reader (Maybe Client.TxSendHandle)) effs
    , MonadIO m
    , LastMember m effs
    )
 => C.Tx C.BabbageEra -> Eff effs NoContent
addTx tx = do
    logInfo $ BlockOperation $ NewTransaction tx
    clientHandler <- Eff.ask
    case clientHandler of
      Nothing      -> logError TxSendCalledWithoutMock
      Just handler ->
          liftIO $ Client.queueTx handler tx
    pure NoContent

-- | Run all chain effects in the IO Monad
runChainEffects ::
 Trace IO PABServerLogMsg
 -> Params
 -> Maybe Client.TxSendHandle
 -> MVar AppState
 -> Eff (NodeServerEffects IO) a
 -> IO ([LogMessage PABServerLogMsg], a)
runChainEffects trace params clientHandler stateVar eff = do
    oldAppState <- liftIO $ takeMVar stateVar
    ((a, events), newState) <- liftIO
            $ processBlock eff
            & runChain
            & mergeState
            & toWriter
            & runReaders oldAppState
            & interpret (LM.handleLogMsgTrace trace)
            & runM
    liftIO $ putMVar stateVar newState
    pure (events, a)
        where
            processBlock e = e >>= \r -> Chain.processBlock >> pure r

            runChain = interpret (mapLog ProcessingChainEvent)
                     . reinterpret (handleChain params)
                     . interpret (mapLog ProcessingChainEvent)
                     . reinterpret (handleControlChain params)

            mergeState = interpret (handleZoomedState chainState)

            toWriter = Eff.runWriter . reinterpret (handleLogWriter @PABServerLogMsg @[LogMessage PABServerLogMsg] (unto return))

            runReaders s = Eff.runState s . Eff.runReader clientHandler

processChainEffects ::
    Trace IO PABServerLogMsg
    -> Params
    -> Maybe Client.TxSendHandle
    -> MVar AppState
    -> Eff (NodeServerEffects IO) a
    -> IO a
processChainEffects trace params clientHandler stateVar eff = do
    (events, result) <- liftIO $ runChainEffects trace params clientHandler stateVar eff
    LM.runLogEffects trace $ traverse_ (\(LogMessage _ chainEvent) -> logDebug chainEvent) events
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
