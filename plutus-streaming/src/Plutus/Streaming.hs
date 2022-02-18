module Plutus.Streaming where

import Cardano.Api
import Cardano.Api.ChainSync.Client
import Control.Concurrent
import Control.Concurrent.Async
import Streaming
import Streaming.Prelude qualified as S

-- import Plutus.Contract.CardanoAPI (fromCardanoBlock, fromCardanoTx)

--
-- FIXME this needs IsString (Hash BlockHeader) which seems to be missing
--
-- recentPoint :: ChainPoint
-- recentPoint = ChainPoint (SlotNo 53427524) "5e2bde4e504a9888a4f218dafc79a7619083f97d48684fcdba9dc78190df8f99"

-- Simple ChainSync client (non pipelined)

data ChainSyncEvent
  = RollForward (BlockInMode CardanoMode) ChainTip
  | RollBackward ChainPoint ChainTip
  deriving (Show)

withChainSyncEventStream ::
  FilePath ->
  NetworkId ->
  ChainPoint ->
  (Stream (Of ChainSyncEvent) IO () -> IO b) ->
  IO b
withChainSyncEventStream filePath networkId point consumer = do
  -- We use a MVar as a synchronisation point to learn if the client as
  -- successfully found an intersection. He rely on the fact that
  -- clientSyncChain will write into m, telling us whether it has found an
  -- intersection. If this doesn't happen we will be stuck waiting forever.
  -- FIXME I haven't even thought about exception safety here.
  m <- newEmptyMVar
  withAsync (chainSyncClient filePath networkId point m) $ \_ -> do
    mc <- takeMVar m

    -- let waitForClient = do
    --       mmc <- tryTakeMVar m
    --       case mmc of
    --         Nothing -> do
    --           putStrLn "waiting ..."
    --           threadDelay 1000000
    --           waitForClient
    --         Just mc -> return mc
    -- mc <- waitForClient

    case mc of
      Nothing ->
        consumer $ return ()
      Just c ->
        -- Client gets brutally killed when the consumer finishes, we
        -- should allow for a better clean up here
        consumer $ S.repeatM $ readChan c

chainSyncClient ::
  FilePath ->
  NetworkId ->
  ChainPoint ->
  MVar (Maybe (Chan ChainSyncEvent)) ->
  IO ()
chainSyncClient socketPath networkId point mChan = do
  connectToLocalNode
    connectInfo
    localNodeClientProtocols
  where
    --
    -- Client state-machine definition
    --

    onIntersect =
      ClientStIntersect
        { recvMsgIntersectFound = \point' _ ->
            ChainSyncClient $ do
              putStrLn $ "Intersection found at " ++ show point'
              c <- newChan
              putMVar mChan (Just c)
              requestNext c,
          recvMsgIntersectNotFound = \_ ->
            ChainSyncClient $ do
              putStrLn "Intersection not found"
              putMVar mChan Nothing
              pure $ SendMsgDone ()
        }

    requestNext c =
      -- FIXME I don't understand this bit
      pure $ SendMsgRequestNext (onNext c) (pure (onNext c))

    onNext c =
      ClientStNext
        { recvMsgRollForward = \bim ct ->
            ChainSyncClient $ do
              writeChan c (RollForward bim ct)
              requestNext c,
          recvMsgRollBackward = \cp ct ->
            ChainSyncClient $ do
              writeChan c (RollBackward cp ct)
              requestNext c
        }

    --
    -- Client state-machine entry point
    --

    entryPoint :: ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    entryPoint =
      ChainSyncClient $ do
        putStrLn "Connecting ..."
        pure $ SendMsgFindIntersect [point] onIntersect

    --
    -- Connection protocols
    --

    localNodeClientProtocols :: LocalNodeClientProtocolsInMode CardanoMode
    localNodeClientProtocols =
      LocalNodeClientProtocols
        { localChainSyncClient = LocalChainSyncClient entryPoint,
          localTxSubmissionClient = Nothing,
          localStateQueryClient = Nothing
        }

    --
    -- Connection Information
    --

    connectInfo :: LocalNodeConnectInfo CardanoMode
    connectInfo =
      LocalNodeConnectInfo
        { localConsensusModeParams = CardanoModeParams epochSlots,
          localNodeNetworkId = networkId,
          localNodeSocketPath = socketPath
        }

    -- FIXME this comes from the config file see
    -- https://input-output-hk.github.io/cardano-node//cardano-api/lib/src/Cardano.Api.LedgerState.html#local-6989586621679476739
    epochSlots = EpochSlots 40

--
-- https://input-output-hk.github.io/cardano-node//cardano-api/lib/src/Cardano.Api.LedgerState.html#foldBlocks
--
