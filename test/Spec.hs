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
import qualified Data.Set                           as Set
import           Data.Text                          (Text)
import qualified Data.Text.IO                       as T
import           Data.Typeable
import           GHC.Generics
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.PSTypes
import           Servant.API
import           Servant.Foreign
import           Servant.PureScript.CodeGen
import           Servant.PureScript.Internal
import           Text.PrettyPrint.Mainland

data Hello = Hello {
  message :: Text
} deriving Generic

instance FromJSON Hello
instance ToJSON Hello

newtype TestHeader = TestHeader Text deriving (Generic, Show, Eq)

instance ToJSON TestHeader

type MyAPI = Header "TestHeader" TestHeader :> QueryFlag "myFlag" :> QueryParam "myParam" Hello :> QueryParams "myParams" Hello :> "hello" :> ReqBody '[JSON] Hello :> Get '[JSON] Hello
  :<|> Header "TestHeader" Hello :> "testHeader" :> Get '[JSON] TestHeader
  :<|> Header "TestHeader" TestHeader :> "by" :> Get '[JSON] Int


reqs = apiToList (Proxy :: Proxy MyAPI) (Proxy :: Proxy DefaultBridge)
req = head reqs

mySettings = let
    testHeader = Param "TestHeader" (doBridge defaultBridge $ mkTypeInfo (Proxy :: Proxy TestHeader))
  in
    defaultSettings {_readerParams = Set.insert testHeader (_readerParams defaultSettings)}

fn = genFunction mySettings req

paramSettings = genParamSettings mySettings

mymodule = genModule mySettings reqs


main :: IO ()
main = putDocLn mymodule
