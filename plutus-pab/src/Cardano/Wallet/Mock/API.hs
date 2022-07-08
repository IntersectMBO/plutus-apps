{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Wallet.Mock.API
    ( API
    ) where

import Cardano.Wallet.Mock.Types (WalletInfo)
import Data.List.NonEmpty (NonEmpty)
import Ledger (PaymentPubKeyHash)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Tx (CardanoTx)
import Plutus.V1.Ledger.Api (Address, Value)
import Servant.API (Capture, Get, JSON, NoContent, Post, QueryParam, ReqBody, (:<|>), (:>))
import Wallet.Emulator.Error (WalletAPIError)

{- Note [WalletID type in wallet API]

We use the following type to identify wallets:

@newtype Wallet = Wallet Integer@

This creates two problems for the purescript bridge:

1. We need to use a bigint library in Purescript. This is done in
'PSGenerator.Common.integerBridge'. Technically, JSON numbers have no size
limit, but we need the special library for parsing (I think!)
2. Sometimes we want to use 'Wallet' as part of a URL. Normally we would do
this in servant with 'Capture "walletId" Wallet'. But this is going to break
the purescript bridge that generates the API client, because it can't handle
'Wallet' or even 'Integer' types.
To address this, we parameterise the API over the type of wallet ID. In the
servant server implementation we specialise this to 'Integer'. In the
PSGenerator we specialise it to 'Text'.

-}

type API walletId -- see note [WalletID type in wallet API]
    = "create" :> QueryParam "funds" Integer :> Post '[JSON] WalletInfo
      :<|> Capture "walletId" walletId :> "submit-txn" :> ReqBody '[JSON] CardanoTx :> Post '[JSON] NoContent
      -- TODO: Should we removed in favor of 'own-addresses'. However, how do we deprecate an HTTP request?
      :<|> Capture "walletId" walletId :> "own-payment-public-key-hash" :> Get '[JSON] PaymentPubKeyHash
      :<|> Capture "walletId" walletId :> "own-addresses" :> Get '[JSON] (NonEmpty Address)
      :<|> Capture "walletId" walletId :> "balance-tx" :> ReqBody '[JSON] UnbalancedTx :> Post '[JSON] (Either WalletAPIError CardanoTx)
      :<|> Capture "walletId" walletId :> "total-funds" :> Get '[JSON] Value
      :<|> Capture "walletId" walletId :> "sign" :> ReqBody '[JSON] CardanoTx :> Post '[JSON] CardanoTx
