{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE TypeFamilies      #-}

{-|
   This mock client has been used to test the PAB while we had no real node available.
   Since now we do, this will be phased out and eventually removed in favor of the
   `Cardano.Protocol.Socket.Client` module which connects to a real cardano node.
-}
module Cardano.Protocol.Socket.Mock.Client where

import Data.ByteString.Lazy qualified as LBS
import Data.Time.Units (Second, TimeUnit, toMicroseconds)
import Data.Void (Void)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Catch (catchAll)
import Control.Tracer

import Cardano.Api qualified as C

import Ouroboros.Network.Block (Point (..))
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync

import Cardano.Node.Emulator.Internal.Node (SlotConfig, currentSlot)
import Ouroboros.Network.IOManager
import Ouroboros.Network.Mux
import Ouroboros.Network.NodeToClient (NodeToClientProtocols (..), connectTo, versionedNodeToClientProtocols)
import Ouroboros.Network.Snocket
import Ouroboros.Network.Socket

import Cardano.Protocol.Socket.Client (ChainSyncHandle (..))
import Cardano.Protocol.Socket.Type
import Ledger (Block, Slot (..))

newtype TxSendHandle = TxSendHandle
    { tshQueue :: TQueue (C.Tx C.BabbageEra) }

-- | Queue a transaction to be sent to the server.
queueTx ::
    TxSendHandle
 -> C.Tx C.BabbageEra
 -> IO ()
queueTx TxSendHandle { tshQueue } tx =
    atomically (writeTQueue tshQueue tx)

getCurrentSlot :: ChainSyncHandle Block -> IO Slot
getCurrentSlot = cshCurrentSlot

-- | Run the chain sync protocol to get access to the current slot number.
runChainSync' :: FilePath
              -> SlotConfig
              -> IO (ChainSyncHandle Block)
runChainSync' socketPath slotConfig =
  runChainSync socketPath slotConfig (\_ _ -> pure ())

runChainSync :: FilePath
             -> SlotConfig
             -> (Block -> Slot -> IO ())
             -> IO (ChainSyncHandle Block)
runChainSync socketPath slotConfig onNewBlock = do
    let handle = ChainSyncHandle { cshCurrentSlot = currentSlot slotConfig
                                 , cshHandler = onNewBlock
                                 }

    _ <- forkIO $ withIOManager $ loop (1 :: Second) handle
    pure handle
    where
      loop :: TimeUnit a => a -> ChainSyncHandle Block -> IOManager -> IO ()
      loop timeout ch@ChainSyncHandle{ cshHandler } iocp = do
        catchAll
          (connectTo
            (localSnocket iocp)
            nullNetworkConnectTracers
            (versionedNodeToClientProtocols
              nodeToClientVersion
              nodeToClientVersionData
              (\_ _ -> nodeToClientProtocols cshHandler))
            socketPath)
          {- If we receive any error or disconnect, try to reconnect.
             This happens a lot on startup, until the server starts. -}
          (\_ -> do
               threadDelay (fromIntegral $ toMicroseconds timeout)
               loop timeout ch iocp)

      nodeToClientProtocols
        :: (Block -> Slot -> IO ())
        -> NodeToClientProtocols 'InitiatorMode LBS.ByteString IO () Void
      nodeToClientProtocols blockHandler =
        NodeToClientProtocols
          { localChainSyncProtocol = chainSync blockHandler
          , localTxSubmissionProtocol = doNothingInitiatorProtocol
          , localStateQueryProtocol = doNothingInitiatorProtocol
          , localTxMonitorProtocol = doNothingInitiatorProtocol
          }

      chainSync :: (Block -> Slot -> IO ())
                -> RunMiniProtocol 'InitiatorMode LBS.ByteString IO () Void
      chainSync onNewBlock' =
          InitiatorProtocolOnly $
          MuxPeer
            nullTracer
            chainSyncCodec
            (ChainSync.chainSyncClientPeer
               (chainSyncClient slotConfig (onNewBlock' . fromCardanoBlock)))

-- | The client updates the application state when the protocol state changes.
chainSyncClient :: forall block. SlotConfig
                -> (block -> Slot -> IO ())
                -> ChainSync.ChainSyncClient block (Point block) Tip IO ()
chainSyncClient slotConfig onNewBlock =
    ChainSync.ChainSyncClient $ pure requestNext
    where
      requestNext :: ChainSync.ClientStIdle block (Point block) Tip IO ()
      requestNext =
        ChainSync.SendMsgRequestNext
          handleNext
          (return handleNext)

      handleNext :: ChainSync.ClientStNext block (Point block) Tip IO ()
      handleNext =
        ChainSync.ClientStNext
        {
          ChainSync.recvMsgRollForward  = \block _ ->
            ChainSync.ChainSyncClient $ do
              slot <- currentSlot slotConfig
              onNewBlock block slot
              return requestNext
        , ChainSync.recvMsgRollBackward = error "Not supported."
        }

runTxSender :: FilePath
            -> C.NetworkId
            -> IO TxSendHandle
runTxSender socketPath networkId = do
    inputQueue  <- newTQueueIO
    let handle = TxSendHandle { tshQueue = inputQueue }

    _ <- forkIO $ withIOManager $ loop (1 :: Second) handle
    pure handle
    where
      loop :: TimeUnit a => a -> TxSendHandle -> IOManager -> IO ()
      loop timeout ch@TxSendHandle{ tshQueue } iocp = do

        tx <- atomically $ readTQueue tshQueue
        _ <- C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx C.BabbageEraInCardanoMode

        threadDelay (fromIntegral $ toMicroseconds timeout)
        loop timeout ch iocp

      localNodeConnectInfo = C.LocalNodeConnectInfo {
        C.localConsensusModeParams = C.CardanoModeParams epochSlots,
        C.localNodeNetworkId = networkId,
        C.localNodeSocketPath = socketPath }
