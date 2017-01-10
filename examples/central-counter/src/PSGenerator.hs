{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Data.Aeson
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import qualified Data.Text.IO                       as T
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Servant.API
import           Servant.PureScript
import           Servant.Subscriber.Subscribable

import           Counter.WebAPI

-- | We have been lazy and defined our types in the WebAPI module,
--   we use this opportunity to show how to create a custom bridge moving those
--   types to Counter.ServerTypes.
fixTypesModule :: BridgePart
fixTypesModule = do
  typeModule ^== "Counter.WebAPI"
  t <- view haskType
  TypeInfo (_typePackage t) "Counter.ServerTypes" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = defaultBridge <|> fixTypesModule

data MyBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge


myTypes :: [SumType 'Haskell]
myTypes =  [
            mkSumType (Proxy :: Proxy AuthToken)
          , mkSumType (Proxy :: Proxy CounterAction)
          , mkSumType (Proxy :: Proxy Hello)
          ]

mySettings :: Settings
mySettings = (addReaderParam "AuthToken" defaultSettings & apiModuleName .~ "Counter.WebAPI") {
  _generateSubscriberAPI = True
  }


main :: IO ()
main = do
  let frontEndRoot = "frontend/src"
  writeAPIModuleWithSettings mySettings frontEndRoot myBridgeProxy appAPI
  writePSTypes frontEndRoot (buildBridge myBridge) myTypes

