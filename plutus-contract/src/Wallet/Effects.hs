{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Wallet.Effects(
    -- * Wallet effect
    WalletEffect(..)
    , submitTxn
    , ownPaymentPubKeyHash
    , balanceTx
    , totalFunds
    , walletAddSignature
    , yieldUnbalancedTx
    -- * Node client
    , NodeClientEffect(..)
    , publishTx
    , getClientSlot
    , getClientParams
    ) where

import Control.Monad.Freer.TH (makeEffect)
import Ledger (CardanoTx, Params, PaymentPubKeyHash, Slot, Value)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Wallet.Error (WalletAPIError)

data WalletEffect r where
    SubmitTxn :: CardanoTx -> WalletEffect ()
    OwnPaymentPubKeyHash :: WalletEffect PaymentPubKeyHash
    BalanceTx :: UnbalancedTx -> WalletEffect (Either WalletAPIError CardanoTx)
    TotalFunds :: WalletEffect Value -- ^ Total of all funds that are in the wallet (incl. tokens)
    WalletAddSignature :: CardanoTx -> WalletEffect CardanoTx
    -- | Sends an unbalanced tx to be balanced, signed and submitted.
    YieldUnbalancedTx :: UnbalancedTx -> WalletEffect ()
makeEffect ''WalletEffect

data NodeClientEffect r where
    PublishTx :: CardanoTx -> NodeClientEffect ()
    GetClientSlot :: NodeClientEffect Slot
    GetClientParams :: NodeClientEffect Params
makeEffect ''NodeClientEffect
