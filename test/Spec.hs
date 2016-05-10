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
import           Data.Text                   (Text)
import qualified Data.Text.IO                as T
import           Data.Typeable
import           GHC.Generics
import           Language.PureScript.Bridge
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

type MyAPI = QueryFlag "myFlag" :> QueryParam "myParam" Hello :> QueryParams "myParams" Hello :> "hello" :> Get '[JSON] Hello


reqs = apiToList (Proxy :: Proxy MyAPI) (Proxy :: Proxy DefaultBridge)
req = head reqs
fn = genFunction defaultSettings req

{--
main :: IO ()
main = T.putStrLn $ apiToPureScript (Proxy :: Proxy MyAPI) (Proxy :: Proxy DefaultBridge)
--}
