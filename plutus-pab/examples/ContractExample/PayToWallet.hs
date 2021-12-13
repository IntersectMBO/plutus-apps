{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TypeApplications   #-}

module ContractExample.PayToWallet(
    payToWallet
    , PayToWalletParams(..)
    , PayToWalletSchema
    ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Void (Void)
import GHC.Generics (Generic)
import Schema (ToSchema)

import Ledger (PaymentPubKeyHash, Value)
import Ledger.Constraints (adjustUnbalancedTx, mustPayToPubKey)
import Plutus.Contract (ContractError, Endpoint, Promise, endpoint, mkTxConstraints, yieldUnbalancedTx)

data PayToWalletParams =
    PayToWalletParams
        { amount :: Value
        , pkh    :: PaymentPubKeyHash
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON, ToSchema)

type PayToWalletSchema = Endpoint "PayToWallet" PayToWalletParams

payToWallet :: Promise () PayToWalletSchema ContractError ()
payToWallet = endpoint @"PayToWallet" $ \PayToWalletParams{amount, pkh} -> do
  utx <- mkTxConstraints @Void mempty (mustPayToPubKey pkh amount)
  yieldUnbalancedTx $ adjustUnbalancedTx utx
