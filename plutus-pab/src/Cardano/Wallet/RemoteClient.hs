{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Wallet.RemoteClient
    ( handleWalletClient
    ) where

import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (NetworkIdWrapper))
import Cardano.Api.Shelley qualified as Cardano.Api
import Cardano.Node.Types (PABServerConfig (pscNetworkId))
import Control.Concurrent.STM qualified as STM
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Text qualified as Text
import Plutus.Contract.Wallet (export)
import Plutus.PAB.Core.ContractInstance.STM (InstancesState)
import Plutus.PAB.Core.ContractInstance.STM qualified as Instances
import Wallet.API qualified as WAPI
import Wallet.Effects (WalletEffect (BalanceTx, OwnPaymentPubKeyHash, SubmitTxn, TotalFunds, WalletAddSignature, YieldUnbalancedTx))
import Wallet.Error (WalletAPIError (RemoteClientFunctionNotYetSupported), throwOtherError)
import Wallet.Types (ContractInstanceId)

-- | Wallet effect handler to remote client scenario.
--
-- Useful for browser-based wallets (Nami, Yoroi, etc.) where the PAB doesn't
-- have direct access.
--
-- TODO: All wallet effects, except 'YieldUnbalancedTx' need to be implemented. See SCP-3094.
handleWalletClient
    :: forall m effs.
    ( LastMember m effs
    , MonadIO m
    , Member WAPI.NodeClientEffect effs
    , Member (Error WalletAPIError) effs
    , Member (Reader Cardano.Api.ProtocolParameters) effs
    , Member (Reader InstancesState) effs
    )
    => PABServerConfig
    -> Maybe ContractInstanceId
    -> WalletEffect
    ~> Eff effs
handleWalletClient config cidM event = do
    let NetworkIdWrapper networkId = pscNetworkId config
    protocolParams <- ask @Cardano.Api.ProtocolParameters
    case event of
        OwnPaymentPubKeyHash -> do
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.OwnPaymentPubKeyHash"

        WalletAddSignature _ -> do
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.WalletAddSignature"

        TotalFunds -> do
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.TotalFunds"

        SubmitTxn _ -> do
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.SubmitTxn"

        BalanceTx _ ->
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.BalanceTx"

        YieldUnbalancedTx utx -> do
            slotConfig <- WAPI.getClientSlotConfig
            case export protocolParams networkId slotConfig utx of
                Left err -> throwOtherError $ Text.pack $ show err
                Right ex -> do
                  case cidM of
                    Nothing -> throwOtherError "RemoteWalletClient: No contract instance id"
                    Just cid -> do
                        iss <- ask @InstancesState
                        liftIO $ STM.atomically $ do
                            is <- Instances.instanceState cid iss
                            STM.modifyTVar (Instances.issYieldedExportTxs is) (\txs -> txs ++ [ex])
