{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.PureScript where

import           Control.Lens
import           Data.Aeson
import           Data.Bifunctor
import           Data.Char
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe                          (mapMaybe, maybeToList)
import           Data.Monoid
import           Data.Proxy
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Data.Typeable
import           GHC.Generics                        hiding (to)
import           Language.PureScript.Bridge
import           Language.PureScript.Bridge.Printer
import           Language.PureScript.Bridge.PSTypes
import           Language.PureScript.Bridge.TypeInfo
import           Servant.API
import           Servant.Foreign
import           Servant.PureScript.CodeGen
import           Servant.PureScript.Internal
import           System.FilePath
import           System.IO                           (IOMode (..), withFile)
import           Text.PrettyPrint.Mainland           (hPutDocLn)

writeAPIModule :: forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) PSType api
  , GenerateList PSType (Foreign PSType api)
  , HasBridge bridgeSelector
  ) => FilePath -> Proxy bridgeSelector -> Proxy api -> IO ()
writeAPIModule = writeAPIModuleWithSettings defaultSettings

writeAPIModuleWithSettings :: forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) PSType api
  , GenerateList PSType (Foreign PSType api)
  , HasBridge bridgeSelector
  ) => Settings -> FilePath -> Proxy bridgeSelector -> Proxy api -> IO ()
writeAPIModuleWithSettings opts root pBr pAPI = do
    let apiList  = apiToList pAPI pBr
    let contents = genModule opts apiList
    withFile mPath WriteMode $ flip hPutDocLn contents
  where
    mFile = (joinPath . map T.unpack . T.splitOn "." $ _apiModuleName opts) <> ".purs"
    mPath = root </> mFile
