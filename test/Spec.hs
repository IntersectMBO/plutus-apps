{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}




module Main where

import           Data.Aeson
import           Data.Proxy
import           Data.Text                  (Text)
import           Data.Typeable
import           GHC.Generics
import           Language.PureScript.Bridge
import           Servant.API
import           Servant.Foreign
import Servant.PureScript.Internal

data Hello = Hello {
  message :: Text
} deriving Generic

instance FromJSON Hello
instance ToJSON Hello

type MyAPI = "hello" :> Get '[JSON] Hello

main :: IO ()
main = print $ apiToPureScript (Proxy :: Proxy MyAPI) (Proxy :: Proxy DefaultBridge)
