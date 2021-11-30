module Data.Cardano.Transaction
  ( Transaction
  , fromBytes
  , toBytes
  , new
  , body
  ) where

import Prologue
import Control.Monad.Reader (class MonadAsk, asks)
import Data.ArrayBuffer.Types (Uint8Array)
import Data.Cardano (CardanoWasm)
import Data.Cardano.TransactionBody (TransactionBody)
import Data.Cardano.TransactionWitnessSet (TransactionWitnessSet)
import Effect.Aff (Error)

foreign import data Transaction :: Type

foreign import fromBytesImpl :: forall r. CardanoWasm -> Uint8Array -> (Error -> r) -> (Transaction -> r) -> r

fromBytes :: forall m. MonadAsk CardanoWasm m => Uint8Array -> m (Either Error Transaction)
fromBytes bytes = asks \wasm -> fromBytesImpl wasm bytes Left Right

foreign import toBytes :: Transaction -> Uint8Array

foreign import newImpl :: forall r. CardanoWasm -> TransactionBody -> TransactionWitnessSet -> (Error -> r) -> (Transaction -> r) -> r

new :: forall m. MonadAsk CardanoWasm m => TransactionBody -> TransactionWitnessSet -> m (Either Error Transaction)
new txBody witnessSet = asks \wasm -> newImpl wasm txBody witnessSet Left Right

foreign import body :: Transaction -> TransactionBody
