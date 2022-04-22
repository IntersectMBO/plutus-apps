-- | The set of parameters, like protocol parameters and slot configuration.
module Ledger.Params(
  Params(..)
) where

import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.Shelley.API qualified as C.Ledger
import Data.Default (Default (def))
import Ledger.TimeSlot (SlotConfig (..))


data Params = Params {
  pSlotConfig     :: SlotConfig,
  pProtocolParams :: C.Api.ProtocolParameters,
  pNetworkId      :: C.Api.NetworkId
}

instance Default Params where
  def = Params def
    (C.Api.fromShelleyPParams $ C.Ledger.sgProtocolParams C.Api.shelleyGenesisDefaults)
    (C.Api.Testnet $ C.Api.NetworkMagic 1)
