{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}

module Cardano.Streaming.Callbacks where

import Control.Exception (throw)
import Data.Word (Word32)

import Cardano.Api qualified as C
import Cardano.Slotting.Slot (WithOrigin (At, Origin))
import Network.TypedProtocol.Pipelined (N (Z), Nat (Succ, Zero))
import Ouroboros.Network.Protocol.ChainSync.Client qualified as CS
import Ouroboros.Network.Protocol.ChainSync.ClientPipelined qualified as CSP
import Ouroboros.Network.Protocol.ChainSync.PipelineDecision (PipelineDecision (Collect), pipelineDecisionMax)

import Cardano.Streaming.Helpers qualified as H
import Ouroboros.Consensus.Util.ResourceRegistry qualified as Util
import Ouroboros.Consensus.Util.Args qualified as Util
import Ouroboros.Consensus.Storage.ImmutableDB qualified as ImmutableDB
import Ouroboros.Consensus.Storage.ChainDB qualified as ChainDB
import Ouroboros.Consensus.Storage.FS.API qualified as FS
import Ouroboros.Consensus.Storage.FS.API.Types qualified as FS
import Ouroboros.Consensus.Storage.FS.IO qualified as FS
-- import Ouroboros.Consensus.Storage.FS.API.Types

-- * Raw chain-sync clients using callback

blocksCallbackPipelined
  :: Word32 -> C.LocalNodeConnectInfo C.CardanoMode -> C.ChainPoint
  -> (H.ChainSyncEvent (C.BlockInMode C.CardanoMode) -> IO ())
  -> IO ()
blocksCallbackPipelined n con point callback =
  C.connectToLocalNode con $ C.LocalNodeClientProtocols
    { C.localChainSyncClient    = C.LocalChainSyncClientPipelined $ CSP.ChainSyncClientPipelined $ pure $ CSP.SendMsgFindIntersect [point] onIntersect
    , C.localTxSubmissionClient = Nothing
    , C.localStateQueryClient   = Nothing
    , C.localTxMonitoringClient = Nothing
    }
  where
    onIntersect =
      CSP.ClientPipelinedStIntersect
        { CSP.recvMsgIntersectFound = \_ _ -> pure $ work n
        , CSP.recvMsgIntersectNotFound = throw H.NoIntersectionFound
        }

    work :: Word32 -> CSP.ClientPipelinedStIdle 'Z (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
    work pipelineSize = requestMore Origin Origin Zero
      where
          requestMore -- was clientIdle_RequestMoreN
            :: WithOrigin C.BlockNo -> WithOrigin C.BlockNo -> Nat n
            -> CSP.ClientPipelinedStIdle n (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
          requestMore clientTip serverTip rqsInFlight = let
            in case pipelineDecisionMax pipelineSize rqsInFlight clientTip serverTip of
                -- handle a response
                Collect -> case rqsInFlight of
                  Succ predN -> CSP.CollectResponse Nothing (clientNextN predN)
                -- fire more requests
                _ -> CSP.SendMsgRequestNextPipelined (requestMore clientTip serverTip (Succ rqsInFlight))

          clientNextN
            :: Nat n
            -> CSP.ClientStNext n (C.BlockInMode C.CardanoMode) C.ChainPoint C.ChainTip IO ()
          clientNextN rqsInFlight = CSP.ClientStNext
            { CSP.recvMsgRollForward = \bim ct -> do
                callback $ H.RollForward bim ct
                return $ requestMore (At $ H.bimBlockNo bim) (H.fromChainTip ct) rqsInFlight
            , CSP.recvMsgRollBackward = \cp ct -> do
                callback $ H.RollBackward cp ct
                return $ requestMore Origin (H.fromChainTip ct) rqsInFlight
            }

blocksCallback
  :: C.LocalNodeConnectInfo C.CardanoMode -> C.ChainPoint
  -> (H.ChainSyncEvent (C.BlockInMode C.CardanoMode) -> IO ())
  -> IO ()
blocksCallback con point callback =
  C.connectToLocalNode con $ C.LocalNodeClientProtocols
    { C.localChainSyncClient    = C.LocalChainSyncClient $ CS.ChainSyncClient $ pure $ CS.SendMsgFindIntersect [point] onIntersect
    , C.localTxSubmissionClient = Nothing
    , C.localStateQueryClient   = Nothing
    , C.localTxMonitoringClient = Nothing
    }
  where
    onIntersect =
      CS.ClientStIntersect
        { CS.recvMsgIntersectFound = \_ _ -> CS.ChainSyncClient sendRequestNext
        , CS.recvMsgIntersectNotFound = throw H.NoIntersectionFound
        }
    sendRequestNext = pure $ CS.SendMsgRequestNext onNext (pure onNext)
      where
        onNext = CS.ClientStNext
            { CS.recvMsgRollForward = \bim ct -> CS.ChainSyncClient $ do
                callback $ H.RollForward bim ct
                sendRequestNext
            , CS.recvMsgRollBackward = \cp ct -> CS.ChainSyncClient $ do
                callback $ H.RollBackward cp ct
                sendRequestNext
            }

type Block = C.BlockInMode C.CardanoMode

blocksFromChainDbCallback :: C.ChainPoint -> (Block -> IO ()) -> IO ()
blocksFromChainDbCallback _point _callback = let
  preprod = "/home/markus/preprod" :: FilePath
  preprodHasFS = FS.ioHasFS $ FS.MountPoint preprod :: FS.HasFS IO FS.HandleIO
  args = ImmutableDB.defaultArgs $ FS.SomeHasFS preprodHasFS :: ImmutableDB.ImmutableDbArgs Util.Defaults IO Block
  iargs = undefined :: ImmutableDB.ImmutableDbArgs Util.Identity IO Block

  f :: Util.WithTempRegistry st IO (ImmutableDB.ImmutableDB IO Block, st) -> IO (ImmutableDB.ImmutableDB IO Block)
  f = u

  open = ImmutableDB.openDB iargs f

  uopen = u
  in ImmutableDB.withDB uopen printBlockHashes

-- | Print the hash of each block in the ImmutableDB
printBlockHashes :: ImmutableDB.ImmutableDB IO (Block) -> IO ()
printBlockHashes db = do
  -- create an iterator over the ImmutableDB
  eitherIterator <- let
    resourceRegistry = u :: Util.ResourceRegistry m
    blockComponent = u :: ChainDB.BlockComponent Block b
    streamFrom = u :: ChainDB.StreamFrom Block
    streamTo = u :: ChainDB.StreamTo Block
    -- -> m (Either (MissingBlock blk) (Iterator m blk b))
    in ImmutableDB.stream db resourceRegistry blockComponent streamFrom streamTo

  case eitherIterator of
    Left (_e :: ImmutableDB.MissingBlock Block) -> do
      putStrLn "MissingBlock, exiting"
      return () -- todo
    Right (iterator :: ImmutableDB.Iterator IO Block b) ->
      let
        loop = do
          r :: ImmutableDB.IteratorResult Block <- ImmutableDB.iteratorNext iterator
          case r of
            ImmutableDB.IteratorResult (blk :: Block) -> print $ H.bimSlotNo blk
            ImmutableDB.IteratorExhausted -> pure ()
        in loop

u :: a
u = undefined
