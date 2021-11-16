{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}

module Cardano.Wallet.Types
    ( -- * Wallet configuration
      WalletConfig (..)
    , LocalWalletSettings (..)
    , WalletUrl (..)
    , defaultWalletConfig
      -- * Lens and Prisms
    , walletSettingsL
    , baseUrlL
    , _LocalWalletConfig
    , _RemoteWalletConfig
    ) where

import Control.Lens (Lens', Traversal', lens, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (Default (def))
import GHC.Generics (Generic)
import Servant.Client (BaseUrl (BaseUrl), Scheme (Http))

data WalletConfig =
      LocalWalletConfig { walletSettings :: LocalWalletSettings }
    | RemoteWalletConfig
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Default WalletConfig where
  def = defaultWalletConfig

defaultWalletConfig :: WalletConfig
defaultWalletConfig =
  LocalWalletConfig $ LocalWalletSettings
    -- See Note [pab-ports] in "test/full/Plutus/PAB/CliSpec.hs".
    { baseUrl = WalletUrl $ BaseUrl Http "localhost" 9081 ""
    }

walletSettingsL :: Traversal' WalletConfig LocalWalletSettings
walletSettingsL f (LocalWalletConfig settings) =
    (\settings' -> LocalWalletConfig settings') <$> f settings
walletSettingsL _ c@RemoteWalletConfig {} = pure c

newtype LocalWalletSettings = LocalWalletSettings { baseUrl :: WalletUrl }
    deriving (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

newtype WalletUrl = WalletUrl BaseUrl
    deriving (Eq, Show, ToJSON, FromJSON) via BaseUrl

baseUrlL :: Lens' LocalWalletSettings WalletUrl
baseUrlL = lens g s where
    g = baseUrl
    s settings url = settings { baseUrl = url }

makePrisms ''WalletConfig
