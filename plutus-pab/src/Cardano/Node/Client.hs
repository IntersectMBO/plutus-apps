{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

module Cardano.Node.Client where

import Cardano.Node.Emulator.Internal.Node.Params (Params)
import Control.Monad.Freer
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class
import Data.Proxy (Proxy (Proxy))
import Ledger (CardanoTx (CardanoEmulatorEraTx))
import Servant (NoContent, (:<|>) (..))
import Servant.Client (ClientM, client)

import Cardano.Node.Socket.Emulator.API (API)
import Cardano.Node.Socket.Emulator.Types (CNSEServerLogMsg, NodeServerConfig (..))
import Cardano.Node.Types (ChainSyncHandle, NodeMode (..), PABServerConfig (..))
import Cardano.Protocol.Socket.Client qualified as Client
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Control.Monad.Freer.Extras.Log (LogMessage)
import Plutus.PAB.Types (PABError (..))
import Wallet.Effects (NodeClientEffect (..))

healthcheck :: ClientM NoContent
consumeEventHistory :: ClientM [LogMessage CNSEServerLogMsg]
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
              Just handle -> liftIO $ (MockClient.queueTx handle . (\(CardanoEmulatorEraTx c) -> c)) tx
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
runChainSyncWithCfg PABServerConfig
                        { pscNodeMode
                        , pscNodeServerConfig = NodeServerConfig
                            { nscSocketPath
                            , nscNetworkId
                            , nscSlotConfig
                            }
                        } =
    case pscNodeMode of
      MockNode   ->
          Left <$> MockClient.runChainSync' nscSocketPath nscSlotConfig
      _ ->
          Right <$> Client.runChainSync' nscSocketPath
                                         nscSlotConfig
                                         nscNetworkId
                                         []

