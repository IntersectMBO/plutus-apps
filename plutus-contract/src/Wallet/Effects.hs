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
    , ownAddresses
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

import Cardano.Node.Emulator.Internal.Node.Params (Params)
import Control.Monad.Freer.TH (makeEffect)
import Data.List.NonEmpty (NonEmpty)
import Ledger (CardanoAddress, CardanoTx, Slot)
import Ledger.Tx.Constraints.OffChain (UnbalancedTx)
import Plutus.V1.Ledger.Value (Value)
import Wallet.Error (WalletAPIError)

{-# DEPRECATED TotalFunds "We won't use the wallet for querying blockchain information. See https://plutus-apps.readthedocs.io/en/latest/adr/0005-pab-indexing-solution-integration.html" #-}

data WalletEffect r where
    SubmitTxn :: CardanoTx -> WalletEffect ()
    OwnAddresses :: WalletEffect (NonEmpty CardanoAddress)
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
