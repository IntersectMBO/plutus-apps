{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}

{-# OPTIONS_GHC -Wno-deprecations #-} -- TODO Remove once TotalFunds gets removed

module Cardano.Wallet.Mock.Client where

import Cardano.Wallet.Mock.API (API)
import Cardano.Wallet.Mock.Types (WalletInfo)
import Control.Monad (void)
import Control.Monad.Freer (Eff, LastMember, Member, sendM, type (~>))
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (Proxy))
import Ledger (PaymentPubKeyHash)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Tx (CardanoTx)
import Plutus.V1.Ledger.Api (Address, Value)
import Servant ((:<|>) ((:<|>)))
import Servant.Client (ClientEnv, ClientError, ClientM, client, runClientM)
import Wallet.Effects (WalletEffect (BalanceTx, OwnAddresses, SubmitTxn, TotalFunds, WalletAddSignature, YieldUnbalancedTx))
import Wallet.Emulator.Error (WalletAPIError)
import Wallet.Emulator.Wallet (Wallet (Wallet, getWalletId), WalletId)

{-# DEPRECATED ownPaymentPubKeyHash "Use ownAddresses instead" #-}

createWallet :: Maybe Integer -> ClientM WalletInfo
submitTxn :: Wallet -> CardanoTx -> ClientM ()
ownPaymentPubKeyHash :: Wallet -> ClientM PaymentPubKeyHash
ownAddresses :: Wallet -> ClientM (NonEmpty Address)
balanceTx :: Wallet -> UnbalancedTx -> ClientM (Either WalletAPIError CardanoTx)
totalFunds :: Wallet -> ClientM Value
sign :: Wallet -> CardanoTx -> ClientM CardanoTx
(createWallet, submitTxn, ownPaymentPubKeyHash, ownAddresses, balanceTx, totalFunds, sign) =
  ( createWallet_
  , \(Wallet _ wid) tx -> void (submitTxn_ wid tx)
  , ownPaymentPubKeyHash_ . getWalletId
  , ownAddresses_ . getWalletId
  , balanceTx_ . getWalletId
  , totalFunds_ . getWalletId
  , sign_ . getWalletId)
  where
    ( createWallet_
      :<|> (submitTxn_
      :<|> ownPaymentPubKeyHash_
      :<|> ownAddresses_
      :<|> balanceTx_
      :<|> totalFunds_
      :<|> sign_)) = client (Proxy @(API WalletId))

handleWalletClient ::
  forall m effs.
  ( LastMember m effs
  , MonadIO m
  , Member (Error ClientError) effs
  , Member (Error WalletAPIError) effs
  , Member (Reader ClientEnv) effs
  )
  => Wallet
  -> WalletEffect
  ~> Eff effs
handleWalletClient wallet event = do
    clientEnv <- ask @ClientEnv
    let
        runClient :: forall a. ClientM a -> Eff effs a
        runClient a = (sendM $ liftIO $ runClientM a clientEnv) >>= either throwError pure

        submitTxnH :: CardanoTx -> Eff effs ()
        submitTxnH tx = runClient (submitTxn wallet tx)

        ownAddressesH :: Eff effs (NonEmpty Address)
        ownAddressesH = runClient (ownAddresses wallet)

        balanceTxH :: UnbalancedTx -> Eff effs (Either WalletAPIError CardanoTx)
        balanceTxH utx = runClient (balanceTx wallet utx)

        walletAddSignatureH :: CardanoTx -> Eff effs CardanoTx
        walletAddSignatureH tx = runClient $ sign wallet tx

        totalFundsH :: Eff effs Value
        totalFundsH = runClient (totalFunds wallet)

        yieldUnbalancedTx :: UnbalancedTx -> Eff effs ()
        yieldUnbalancedTx utx = do
            balancedTxM <- balanceTxH utx
            case balancedTxM of
              Left err         -> throwError err
              Right balancedTx -> walletAddSignatureH balancedTx >>= submitTxnH

    case event of
        SubmitTxn tx          -> submitTxnH tx
        OwnAddresses          -> ownAddressesH
        BalanceTx utx         -> balanceTxH utx
        WalletAddSignature tx -> walletAddSignatureH tx
        TotalFunds            -> totalFundsH
        YieldUnbalancedTx utx -> yieldUnbalancedTx utx
