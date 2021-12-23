module Data.Cardano
  ( CardanoWasm
  , loadCardanoWasm
  ) where

import Prologue
import Control.Promise (Promise, toAffE)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)

foreign import data CardanoWasm :: Type

foreign import loadCardanoWasmImpl :: Effect (Promise CardanoWasm)

loadCardanoWasm :: forall m. MonadAff m => m CardanoWasm
loadCardanoWasm = liftAff $ toAffE loadCardanoWasmImpl
