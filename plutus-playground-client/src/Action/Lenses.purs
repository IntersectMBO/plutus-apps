module Action.Lenses
  ( _caller
  , _blocks
  , _InSlot
  , _slot
  ) where

import Data.BigInt.Argonaut (BigInt)
import Data.Lens (Iso', Lens', iso)
import Data.Lens.Record (prop)
import Data.Newtype (unwrap, wrap)
import Type.Proxy (Proxy(..))
import Ledger.Slot (Slot)
import Prelude ((<<<))

_caller :: forall r a. Lens' { caller :: a | r } a
_caller = prop (Proxy :: _ "caller")

_blocks :: forall r a. Lens' { blocks :: a | r } a
_blocks = prop (Proxy :: _ "blocks")

_InSlot :: Iso' Slot BigInt
_InSlot = iso (_.getSlot <<< unwrap) (wrap <<< { getSlot: _ })

_slot :: forall r a. Lens' { slot :: a | r } a
_slot = prop (Proxy :: _ "slot")
