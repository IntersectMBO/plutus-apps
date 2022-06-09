{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Cardano.Node.Client where

import Control.Monad.Freer
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class
import Data.Proxy (Proxy (Proxy))
import Ledger (Params, onCardanoTx)
import Servant (NoContent, (:<|>) (..))
import Servant.Client (ClientM, client)

import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (..))
import Cardano.Node.API (API)
import Cardano.Node.Types (ChainSyncHandle, NodeMode (..), PABServerConfig (..), PABServerLogMsg)
import Cardano.Protocol.Socket.Client qualified as Client
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Control.Monad.Freer.Extras.Log (LogMessage)
import Plutus.PAB.Types (PABError (..))
import Wallet.Effects (NodeClientEffect (..))

healthcheck :: ClientM NoContent
consumeEventHistory :: ClientM [LogMessage PABServerLogMsg]
(healthcheck, consumeEventHistory) =
    ( healthcheck_
    , consumeEventHistory_
    )
  where
    healthcheck_ :<|> consumeEventHistory_ =
        client (Proxy @API)

handleNodeClientClient ::
    forall m effs.
    ( LastMember m effs
    , MonadIO m
    , Member (Error PABError) effs
    , Member (Reader (Maybe MockClient.TxSendHandle)) effs
    , Member (Reader ChainSyncHandle) effs
    )
    => Params
    -> NodeClientEffect
    ~> Eff effs
handleNodeClientClient params e = do
    txSendHandle <- ask @(Maybe MockClient.TxSendHandle)
    chainSyncHandle <- ask @ChainSyncHandle
    case e of
        PublishTx tx  ->
            case txSendHandle of
              Nothing ->
                  -- If the PAB is started with the real node working transactions
                  -- need to be sent via the wallet, not the mocked server node
                  -- (which is not actually running).
                  throwError TxSenderNotAvailable
              Just handle ->
                  liftIO $
                      onCardanoTx (MockClient.queueTx handle)
                                  (const $ error "Cardano.Node.Client: Expecting a mock tx, not an Alonzo tx when publishing it.")
                                  tx
        GetClientSlot ->
            either (liftIO . MockClient.getCurrentSlot)
                   (liftIO . Client.getCurrentSlot)
                   chainSyncHandle
        GetClientParams -> pure params

-- | This does not seem to support resuming so it means that the slot tick will
-- be behind everything else. This is due to having 2 connections to the node
-- one for chainSync/block transfer and one for chainSync/currentSlot information.
-- TODO: Think about merging the two functionalities, or keep them in sync.
runChainSyncWithCfg ::
     PABServerConfig
  -> IO ChainSyncHandle
runChainSyncWithCfg PABServerConfig { pscSocketPath
                                    , pscNodeMode
                                    , pscNetworkId
                                    , pscSlotConfig } =
    case pscNodeMode of
      AlonzoNode ->
          Right <$> Client.runChainSync' pscSocketPath
                                         pscSlotConfig
                                         (unNetworkIdWrapper pscNetworkId)
                                         []
      MockNode   ->
          Left <$> MockClient.runChainSync' pscSocketPath pscSlotConfig
