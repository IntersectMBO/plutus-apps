{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Counter.WebAPI where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Proxy
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text.Encoding                 as T
import qualified Data.Text.IO                       as T

import           Data.Typeable
import           GHC.Generics                       (Generic)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Network.HTTP.Types.URI             (urlDecode)
import           Servant.API
import Servant.PureScript (jsonParseUrlPiece, jsonParseHeader)
import           Servant.Subscriber.Subscribable
import           Web.HttpApiData


data Hello = Hello {
  _message :: Text
} deriving Generic

instance FromJSON Hello
instance ToJSON Hello

data AuthToken = VerySecret Text deriving (Generic, Show, Eq, Ord, Read)

instance FromJSON AuthToken

instance FromHttpApiData AuthToken where
  parseUrlPiece = jsonParseUrlPiece
  parseHeader   = jsonParseHeader


data CounterAction = CounterAdd Int | CounterSet Int deriving (Generic, Show, Eq, Ord)

instance FromJSON CounterAction

type FullAPI = AppAPI :<|> Raw

type AppAPI = Header "AuthToken" AuthToken :> "counter" :> CounterAPI


type CounterAPI = Subscribable :> Get '[JSON] Int
             :<|> ReqBody '[JSON] CounterAction :> Put '[JSON] Int


fullAPI :: Proxy FullAPI
fullAPI = Proxy

appAPI :: Proxy AppAPI
appAPI = Proxy
