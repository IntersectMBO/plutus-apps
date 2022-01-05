module Data.Cardano.TransactionUnspentOutput
  ( TransactionUnspentOutput
  , fromBytes
  ) where

import Prologue
import Control.Monad.Reader (class MonadAsk, asks)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Cardano (CardanoWasm)
import Effect.Aff (Error)

foreign import data TransactionUnspentOutput :: Type

foreign import fromBytesImpl :: forall r. CardanoWasm -> Uint8Array -> (Error -> r) -> (TransactionUnspentOutput -> r) -> r

fromBytes :: forall m. MonadAsk CardanoWasm m => Uint8Array -> m (Either Error TransactionUnspentOutput)
fromBytes bytes = asks \wasm -> fromBytesImpl wasm bytes Left Right
