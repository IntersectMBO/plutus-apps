module Data.Functor.Foldable where

import Prologue
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Eq (class Eq1)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Newtype (class Newtype)
import Matryoshka (class Corecursive, class Recursive)
import Schema (FormArgumentF)

-- | This recursive type is isomorphic to `Data.Functor.Mu.Mu`, and
-- only exists because we want `Encode`/`Decode` instances.
newtype Fix f = Fix (f (Fix f))

derive instance newtypeFix :: Newtype (Fix f) _

derive instance genericFix :: Generic (Fix f) _

derive instance eqFix :: Eq1 f => Eq (Fix f)

instance recursiveFix :: Functor f => Recursive (Fix f) f where
  project (Fix v) = v

instance corecursiveFix :: Functor f => Corecursive (Fix f) f where
  embed v = Fix v

------------------------------------------------------------
instance encodeJsonFixFormArgumentF :: EncodeJson (Fix FormArgumentF) where
  encodeJson (Fix f) = encodeJson f

instance decodeJsonFixFormArgumentF :: DecodeJson (Fix FormArgumentF) where
  decodeJson json = Fix <$> decodeJson json

--
instance showFix :: Show (Fix FormArgumentF) where
  show value = genericShow value
