module Data.Cardano.BaseAddress
  ( BaseAddress
  , fromAddress
  , paymentCred
  , stakeCred
  ) where

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Cardano (CardanoWasm)
import Data.Cardano.Address (Address)
import Data.Cardano.StakeCredential (StakeCredential)

foreign import data BaseAddress :: Type

foreign import fromAddressImpl :: CardanoWasm -> Address -> BaseAddress

fromAddress :: forall m. MonadAsk CardanoWasm m => Address -> m BaseAddress
fromAddress address = asks \wasm -> fromAddressImpl wasm address

foreign import paymentCred :: BaseAddress -> StakeCredential

foreign import stakeCred :: BaseAddress -> StakeCredential
