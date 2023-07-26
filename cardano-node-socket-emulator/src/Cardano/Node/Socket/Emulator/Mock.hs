{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Socket.Emulator.Mock where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, putMVar, takeMVar)
import Control.Lens (set, view)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock.POSIX qualified as Time
import Data.Time.Units (Millisecond, toMicroseconds)
import Data.Time.Units.Extra ()
import Servant (NoContent (NoContent))

import Cardano.Node.Emulator.Internal.API (EmulatorLogs)
import Cardano.Node.Emulator.Internal.Node.TimeSlot (SlotConfig, currentSlot, nominalDiffTimeToPOSIXTime,
                                                     slotToBeginPOSIXTime)
import Cardano.Node.Socket.Emulator.Server qualified as Server
import Cardano.Node.Socket.Emulator.Types (AppState (..), emulatorLogs)

healthcheck :: Monad m => m NoContent
healthcheck = pure NoContent

consumeEventHistory :: MonadIO m => MVar AppState -> m EmulatorLogs
consumeEventHistory stateVar =
    liftIO $ do
        oldState <- takeMVar stateVar
        let events = view emulatorLogs oldState
        let newState = set emulatorLogs mempty oldState
        putMVar stateVar newState
        pure events

-- | Calls 'addBlock' at the start of every slot, causing pending transactions
--   to be validated and added to the chain.
slotCoordinator ::
    SlotConfig
    -> Server.ServerHandler
    -> IO a
slotCoordinator sc serverHandler = do
    forever $ do
        slot <- currentSlot sc
        void $ Server.modifySlot (const slot) serverHandler
        now <- Time.getPOSIXTime
        let delay = slotToBeginPOSIXTime sc (slot + 1) - nominalDiffTimeToPOSIXTime now
        liftIO $ threadDelay
               $ fromIntegral
               $ toMicroseconds (fromIntegral delay :: Millisecond)
        void $ Server.processBlock serverHandler
