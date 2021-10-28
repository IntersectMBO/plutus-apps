module StaticData
  ( mkContractDemos
  , lookupContractDemo
  , bufferLocalStorageKey
  , keybindingsLocalStorageKey
  ) where

import Prologue
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Foldable as Foldable
import Data.Lens (Lens', view)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Data.Traversable (class Foldable)
import LocalStorage (Key(..))
import Playground.Types (ContractDemo)
import Playground.Usecases (contractDemos)

mkContractDemos :: Either JsonDecodeError (Array ContractDemo)
mkContractDemos = do
  parseDecodeJson contractDemos

lookupContractDemo :: forall f. Foldable f => String -> f ContractDemo -> Maybe ContractDemo
lookupContractDemo key = Foldable.find (\demo -> view _contractDemoName demo == key)

_contractDemoName :: Lens' ContractDemo String
_contractDemoName = _Newtype <<< prop (Proxy :: _ "contractDemoName")

bufferLocalStorageKey :: Key
bufferLocalStorageKey = Key "PlutusPlaygroundBuffer"

keybindingsLocalStorageKey :: Key
keybindingsLocalStorageKey = Key "EditorPreferences.KeyBindings"
