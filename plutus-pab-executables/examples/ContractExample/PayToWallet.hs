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

import Ledger (PaymentPubKeyHash)
import Ledger.Tx.Constraints (mustPayToPubKey)
import Plutus.Contract (ContractError, Endpoint, Promise, adjustUnbalancedTx, endpoint, logInfo, mkTxConstraints,
                        yieldUnbalancedTx)
import Plutus.Script.Utils.Value (Value)

data PayToWalletParams =
    PayToWalletParams
        { amount :: Value
        , pkh    :: PaymentPubKeyHash
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

type PayToWalletSchema = Endpoint "PayToWallet" PayToWalletParams

payToWallet :: Promise () PayToWalletSchema ContractError ()
payToWallet = endpoint @"PayToWallet" $ \PayToWalletParams{amount, pkh} -> do
    logInfo @String "Calling PayToWallet endpoint"
    utx <- mkTxConstraints @Void mempty (mustPayToPubKey pkh amount)
    logInfo @String $ "Yielding the unbalanced transaction " <> show utx
    adjustUnbalancedTx utx >>= yieldUnbalancedTx

