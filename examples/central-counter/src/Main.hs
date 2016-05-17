{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Proxy
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text.IO                       as T
import           Data.Typeable
import           GHC.Generics                       (Generic)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Servant.API
import           Servant.PureScript

-- | TODO: For servant-purescript: make variable names lower case. (Some general fixup function, also replacing spaces and stuff.)

data Hello = Hello {
  message :: Text
} deriving Generic

instance FromJSON Hello
instance ToJSON Hello

data AuthToken = VerySecret Text deriving (Generic, Show, Eq, Ord)

data CounterAction = CounterAdd Int | CounterSet Int deriving (Generic, Show, Eq, Ord)

instance FromJSON CounterAction

type CounterAPI = Header "AuthToken" AuthToken :> "counter" :> Get '[JSON] Int
             :<|> Header "AuthToken" AuthToken :> "counter" :> ReqBody '[JSON] CounterAction :> Put '[JSON] ()


-- | We have been lazy and defined our types in the Main module,
--   we use this opportunity to show how to create a custom bridge.
fixMainModule :: BridgePart
fixMainModule = do
  typeModule ^== "Main"
  t <- view haskType
  TypeInfo (_typePackage t) "ServerTypes" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = defaultBridge <|> fixMainModule

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge


myTypes :: [SumType]
myTypes =  [
            mkSumType (Proxy :: Proxy AuthToken)
          , mkSumType (Proxy :: Proxy CounterAction)
          ]


mySettings = addReaderParam "AuthToken" defaultSettings


main :: IO ()
main = do
  writeAPIModuleWithSettings mySettings "frontend" myBridgeProxy (Proxy :: Proxy CounterAPI)
  writePSTypes "frontend" (buildBridge myBridge) myTypes
