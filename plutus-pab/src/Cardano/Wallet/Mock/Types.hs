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
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module Cardano.Wallet.Mock.Types (
     -- * effect type for the mock wallet
      WalletEffects
    , Wallets
    , MultiWalletEffect (..)
    , createWallet
    , multiWallet
    , getWalletInfo

     -- * wallet log messages
    , WalletMsg (..)

     -- * newtypes for convenience
    , Port (..)
    , NodeClient (..)
    , ChainClient (..)
    , ChainIndexUrl
    -- * Wallet info
    , WalletInfo(..)
    , fromWalletState
    ) where

import Cardano.BM.Data.Tracer (ToObject (toObject))
import Cardano.BM.Data.Tracer.Extras (Tagged (Tagged), mkObjectStr)
import Cardano.ChainIndex.Types (ChainIndexUrl)
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Freer.State (State)
import Control.Monad.Freer.TH (makeEffect)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map.Strict (Map)
import Data.Text (Text)
import GHC.Generics (Generic)
import Ledger (PaymentPubKeyHash)
import Ledger.Ada (Ada)
import Plutus.ChainIndex (ChainIndexQueryEffect)
import Plutus.PAB.Arbitrary ()
import Plutus.PAB.Types (PABError)
import Prettyprinter (Pretty (pretty), (<+>))
import Servant (ServerError)
import Servant.Client (ClientError)
import Servant.Client.Internal.HttpClient (ClientEnv)
import Wallet.Effects (NodeClientEffect, WalletEffect)
import Wallet.Emulator.Error (WalletAPIError)
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg, TxBalanceMsg)
import Wallet.Emulator.Wallet (Wallet, WalletId, WalletState (WalletState, _mockWallet), mockWalletPaymentPubKeyHash,
                               toMockWallet)

-- | Information about an emulated wallet.
data WalletInfo =
    WalletInfo
        { wiWallet            :: Wallet
        , wiPaymentPubKeyHash :: PaymentPubKeyHash -- ^ Hash of the wallet's public key, serving as wallet ID
        }
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

type Wallets = Map WalletId WalletState

fromWalletState :: WalletState -> WalletInfo
fromWalletState WalletState{_mockWallet} = WalletInfo{wiWallet, wiPaymentPubKeyHash} where
    wiWallet = toMockWallet _mockWallet
    wiPaymentPubKeyHash = mockWalletPaymentPubKeyHash wiWallet

data MultiWalletEffect r where
    CreateWallet :: Maybe Ada -> MultiWalletEffect WalletInfo
    MultiWallet :: Wallet -> Eff '[WalletEffect] a -> MultiWalletEffect a
    GetWalletInfo :: WalletId -> MultiWalletEffect (Maybe WalletInfo)
makeEffect ''MultiWalletEffect

type WalletEffects m = '[ MultiWalletEffect
                        , NodeClientEffect
                        , ChainIndexQueryEffect
                        , State Wallets
                        , Error PABError
                        , LogMsg Text
                        , Error WalletAPIError
                        , Error ClientError
                        , Error ServerError
                        , m]

newtype NodeClient = NodeClient ClientEnv

newtype ChainClient = ChainClient ClientEnv

newtype Port = Port Int
    deriving (Show)
    deriving (Eq, Num, ToJSON, FromJSON, Pretty) via Int

data WalletMsg = StartingWallet Port
               | ChainClientMsg Text
               | Balancing TxBalanceMsg
               | RequestHandling RequestHandlerLogMsg
    deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty WalletMsg where
    pretty = \case
        StartingWallet port -> "Starting wallet server on port" <+> pretty port
        ChainClientMsg m    -> "Chain Client: " <+> pretty m
        Balancing m         -> pretty m
        RequestHandling m   -> pretty m

instance ToObject WalletMsg where
    toObject _ = \case
        StartingWallet port -> mkObjectStr "Starting wallet server" (Tagged @"port" port)
        ChainClientMsg m    -> mkObjectStr "Chain Client: " (Tagged @"msg" m)
        Balancing m         -> mkObjectStr "Balancing" (Tagged @"msg" m)
        RequestHandling m   -> mkObjectStr "RequestHandling" (Tagged @"msg" m)
