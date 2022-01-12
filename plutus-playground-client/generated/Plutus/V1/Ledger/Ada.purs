-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Ada where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.BigInt.Argonaut (BigInt)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Type.Proxy (Proxy(Proxy))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode.Aeson as E
import Data.Map as Map

newtype Ada = Lovelace { getLovelace :: BigInt }

derive instance Eq Ada

instance Show Ada where
  show a = genericShow a

instance EncodeJson Ada where
  encodeJson = defer \_ -> E.encode $ unwrap >$< (E.record
                                                 { getLovelace: E.value :: _ BigInt })

instance DecodeJson Ada where
  decodeJson = defer \_ -> D.decode $ (Lovelace <$> D.record "Lovelace" { getLovelace: D.value :: _ BigInt })

derive instance Generic Ada _

derive instance Newtype Ada _

--------------------------------------------------------------------------------

_Lovelace :: Iso' Ada {getLovelace :: BigInt}
_Lovelace = _Newtype
