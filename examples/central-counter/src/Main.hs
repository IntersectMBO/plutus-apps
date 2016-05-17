{-# LANGUAGE AutoDeriveTypeable    #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

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
import           Servant.Foreign
import           Servant.PureScript.CodeGen
import           Servant.PureScript.Internal
import           Text.PrettyPrint.Mainland

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
--   we use this opportunity to show how you can change the output of 'writePSTypes'.
fixModule :: TypeInfo -> TypeInfo
fixModule ti = case ti ^. typeName of
   "Main" -> ti & typeModule .~ "ServerTypes"
   _ -> ti


counterBridge :: TypeBridge
counterBridge = defaultBridge . fixModule


myTypes :: [SumType]
myTypes =  [
            mkSumType (Proxy :: Proxy AuthToken)
          , mkSumType (Proxy :: Proxy CounterAction)
          ]


reqs = apiToList (Proxy :: Proxy CounterAPI) (Proxy :: Proxy DefaultBridge)

mySettings = let
    authTokenT = mkTypeInfo (Proxy :: Proxy AuthToken)
    authParam = Param "AuthToken" authTokenT
  in
    defaultSettings {_readerParams = Set.insert authParam (_readerParams defaultSettings)}


paramSettings = genParamSettings mySettings

mymodule = genModule mySettings reqs


main :: IO ()
main = putDocLn mymodule
