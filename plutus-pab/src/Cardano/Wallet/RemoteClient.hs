{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Remove once TotalFunds gets removed

module Cardano.Wallet.RemoteClient
    ( handleWalletClient
    ) where

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
import Wallet.Effects (WalletEffect (BalanceTx, OwnAddresses, SubmitTxn, TotalFunds, WalletAddSignature, YieldUnbalancedTx))
import Wallet.Error (WalletAPIError (OtherError, RemoteClientFunctionNotYetSupported), throwOtherError)
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
    , Member (Reader InstancesState) effs
    )
    => Maybe ContractInstanceId
    -> WalletEffect
    ~> Eff effs
handleWalletClient cidM event =
    case event of
        OwnAddresses -> do
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.OwnAddresses"

        WalletAddSignature _ -> do
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.WalletAddSignature"

        TotalFunds -> do
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.TotalFunds"

        SubmitTxn _ -> do
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.SubmitTxn"

        BalanceTx _ ->
            throwError $ RemoteClientFunctionNotYetSupported "Cardano.Wallet.RemoteClient.BalanceTx"

        YieldUnbalancedTx utx -> do
            params <- WAPI.getClientParams
            case export params utx of
                Left err -> throwOtherError $ Text.pack $ show err
                Right ex -> do
                  case cidM of
                    Nothing -> throwOtherError "RemoteWalletClient: No contract instance id"
                    Just cid -> do
                        s <- ask @InstancesState >>= liftIO . Instances.instanceState cid
                        case s of
                            Nothing -> throwError $ OtherError $ "RemoteWalletClient: Contract instance not found: " <> Text.pack (show cid)
                            Just instanceState -> liftIO $ STM.atomically $ do
                                STM.modifyTVar (Instances.issYieldedExportTxs instanceState) (\txs -> txs ++ [ex])
