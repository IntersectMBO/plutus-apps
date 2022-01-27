-- File auto generated by purescript-bridge! --
module Plutus.Trace.Tag where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
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

newtype Tag = Tag { unTag :: String }

derive instance Eq Tag

instance Show Tag where
  show a = genericShow a

instance EncodeJson Tag where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { unTag: E.value :: _ String }
    )

instance DecodeJson Tag where
  decodeJson = defer \_ -> D.decode $ (Tag <$> D.record "Tag" { unTag: D.value :: _ String })

derive instance Generic Tag _

derive instance Newtype Tag _

--------------------------------------------------------------------------------

_Tag :: Iso' Tag { unTag :: String }
_Tag = _Newtype
