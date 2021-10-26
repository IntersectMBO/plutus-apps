-- copied from generated code because the phantom type parameter doesn't work
-- well for the generated typeclass instances
module Plutus.PAB.Effects.Contract.Builtin where

import Prelude
import Control.Lazy (defer)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', iso)
import Data.Show.Generic (genericShow)
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E

data Builtin :: forall k. k -> Type
data Builtin a
  = Builtin

derive instance eqBuiltin :: Eq (Builtin a)

derive instance ordBuiltin :: Ord (Builtin a)

instance showBuiltin :: Show (Builtin a) where
  show a = genericShow a

instance encodeJsonBuiltin :: EncodeJson (Builtin a) where
  encodeJson = defer \_ -> E.encode E.enum

instance decodeJsonBuiltin :: DecodeJson (Builtin a) where
  decodeJson = defer \_ -> D.decode D.enum

derive instance genericBuiltin :: Generic (Builtin a) _

instance enumBuiltin :: Enum (Builtin a) where
  succ = genericSucc
  pred = genericPred

instance boundedBuiltin :: Bounded (Builtin a) where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------
_Builtin :: forall a. Iso' (Builtin a) Unit
_Builtin = iso (const unit) (const Builtin)
