{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}



module Servant.PureScript.Internal where

import           Data.Aeson
import           Data.Proxy
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import           Data.Typeable
import           GHC.Generics
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.TypeInfo
import           Servant.API
import           Servant.Foreign


-- | Our language type is parameterized, so you can choose a custom 'TypeBridge' for your translation, by
--   providing your own data type and implementing 'HasBridge' for it.
--
-- > data MyBridge
-- >
-- > myBridge :: TypeBridge
-- > myBridge = defaultBridge <|> customBridge1 <|> customBridge2
-- >
-- > instance HasBridge MyBridge where
-- >   languageBridge _ = myBridge
--
data PureScript bridgeSelector

instance (Generic a, Typeable a, HasBridge bridgeSelector) => HasForeignType (PureScript bridgeSelector) TypeInfo a where
  typeFor _ _ _ = doBridge (languageBridge (Proxy :: Proxy bridgeSelector)) (mkTypeInfo (Proxy :: Proxy a))

class HasBridge a where
  languageBridge :: Proxy a -> TypeBridge

-- | Use 'PureScript' 'DefaultBridge' if 'defaultBridge' suffices for your needs.
data DefaultBridge

-- | 'languageBridge' for 'DefaultBridge' evaluates to 'defaultBridge' - no surprise there.
instance HasBridge DefaultBridge where
  languageBridge _ = defaultBridge

apiToPureScript :: forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) TypeInfo api
  , GenerateList TypeInfo (Foreign TypeInfo api)
  , HasBridge bridgeSelector
  ) => Proxy api -> Proxy bridgeSelector -> Text
apiToPureScript _ _ = T.pack . show $ listFromAPI (Proxy :: Proxy (PureScript bridgeSelector)) (Proxy :: Proxy TypeInfo) (Proxy :: Proxy api)
