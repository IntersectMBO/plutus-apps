{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Cardano.Wallet.Mock.Server
    ( main
    ) where

import Cardano.BM.Data.Trace (Trace)
import Cardano.ChainIndex.Types (ChainIndexUrl (ChainIndexUrl))
import Cardano.Node.Types (ChainSyncHandle)
import Cardano.Protocol.Socket.Mock.Client qualified as MockClient
import Cardano.Wallet.Mock.API (API)
import Cardano.Wallet.Mock.Handlers (processWalletEffects)
import Cardano.Wallet.Mock.Types (Port (Port), WalletInfo (wiAddresses, wiPaymentPubKeyHash),
                                  WalletMsg (StartingWallet), Wallets, createWallet, getWalletInfo, multiWallet)
import Cardano.Wallet.Types (LocalWalletSettings (LocalWalletSettings, baseUrl), WalletUrl (WalletUrl))
import Control.Concurrent.Availability (Availability, available)
import Control.Concurrent.MVar (MVar, newMVar)
import Control.Monad ((>=>))
import Control.Monad.Freer.Error (throwError)
import Control.Monad.Freer.Extras.Log (logInfo)
import Control.Monad.IO.Class (liftIO)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy (Proxy))
import Ledger.Ada qualified as Ada
import Ledger.CardanoWallet qualified as CW
import Ledger.Params (Params (..))
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Wai.Handler.Warp qualified as Warp
import Plutus.PAB.Arbitrary ()
import Plutus.PAB.Monitoring.Monitoring qualified as LM
import Servant (Application, NoContent (NoContent), err404, hoistServer, serve, (:<|>) ((:<|>)))
import Servant.Client (BaseUrl (baseUrlPort), ClientEnv, mkClientEnv)
import Wallet.Effects (balanceTx, submitTxn, totalFunds, walletAddSignature)
import Wallet.Emulator.Wallet (Wallet (Wallet), WalletId)
import Wallet.Emulator.Wallet qualified as Wallet

app :: Trace IO WalletMsg
    -> MockClient.TxSendHandle
    -> ChainSyncHandle
    -> ClientEnv
    -> MVar Wallets
    -> Params
    -> Application
app trace txSendHandle chainSyncHandle chainIndexEnv mVarState params =
    serve (Proxy @(API WalletId)) $
    hoistServer
        (Proxy @(API WalletId))
        (processWalletEffects trace txSendHandle chainSyncHandle chainIndexEnv mVarState params) $
            (\funds -> createWallet (Ada.lovelaceOf <$> funds)) :<|>
            (\w tx -> multiWallet (Wallet Nothing w) (submitTxn tx) >>= const (pure NoContent)) :<|>
            (getWalletInfo >=> maybe (throwError err404) (pure . wiPaymentPubKeyHash) ) :<|>
            (getWalletInfo >=> maybe (throwError err404) (pure . wiAddresses) ) :<|>
            (\w -> multiWallet (Wallet Nothing w) . balanceTx) :<|>
            (\w -> multiWallet (Wallet Nothing w) totalFunds) :<|>
            (\w tx -> multiWallet (Wallet Nothing w) (walletAddSignature tx))

main :: Trace IO WalletMsg -> LocalWalletSettings -> FilePath -> Params -> ChainIndexUrl -> Availability -> IO ()
main trace LocalWalletSettings { baseUrl } serverSocket params (ChainIndexUrl chainUrl) availability = LM.runLogEffects trace $ do
    chainIndexEnv <- buildEnv chainUrl defaultManagerSettings
    let knownWallets = Map.fromList $ zip (Wallet.getWalletId <$> Wallet.knownWallets) (Wallet.fromMockWallet <$> CW.knownMockWallets)
    mVarState <- liftIO $ newMVar knownWallets
    txSendHandle    <- liftIO $ MockClient.runTxSender serverSocket
    chainSyncHandle <- Left <$> (liftIO $ MockClient.runChainSync' serverSocket $ pSlotConfig params)
    logInfo $ StartingWallet (Port servicePort)
    liftIO $ Warp.runSettings warpSettings
           $ app trace
                 txSendHandle
                 chainSyncHandle
                 chainIndexEnv
                 mVarState
                 params
    where
        servicePort = baseUrlPort (coerce baseUrl)
        warpSettings = Warp.defaultSettings & Warp.setPort servicePort & Warp.setBeforeMainLoop (available availability)

        buildEnv url settings = liftIO
            $ newManager settings >>= \mgr -> pure $ mkClientEnv mgr url
