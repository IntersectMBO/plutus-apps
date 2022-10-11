module Wallet.Lenses
  ( _simulatorWalletWallet
  , _simulatorWalletBalance
  , _walletId
  , _pubKey
  ) where

import Data.BigInt.Argonaut (BigInt)
import Data.Lens (Iso', Lens', iso)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Playground.Types (SimulatorWallet, _SimulatorWallet)
import Ledger.Crypto (PubKey, _PubKey)
import Plutus.V1.Ledger.Value (Value)
import Prelude ((<<<))
import Ledger.CardanoWallet (WalletNumber, _WalletNumber)

_simulatorWalletWallet :: Lens' SimulatorWallet WalletNumber
_simulatorWalletWallet = _SimulatorWallet <<< prop (Proxy :: _ "simulatorWalletWallet")

_simulatorWalletBalance :: Lens' SimulatorWallet Value
_simulatorWalletBalance = _SimulatorWallet <<< prop (Proxy :: _ "simulatorWalletBalance")

_walletId :: Iso' WalletNumber BigInt
_walletId = _WalletNumber <<< iso _.getWallet { getWallet: _ }

_pubKey :: Lens' PubKey String
_pubKey = _PubKey <<< prop (Proxy :: _ "getPubKey")
