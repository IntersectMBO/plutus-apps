{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Servant.PureScript
  ( HasBridge,
    languageBridge,
    defaultBridge,
    defaultBridgeProxy,
    DefaultBridge,
    writeAPIModule,
    writeAPIModuleWithSettings,
    Settings (..),
    apiModuleName,
    readerParams,
    standardImports,
    defaultSettings,
    addReaderParam,
    jsonParseUrlPiece,
    jsonToUrlPiece,
    jsonParseHeader,
    jsonToHeader,
  )
where

import Control.Lens
import Data.Aeson
import Data.Bifunctor
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import Language.PureScript.Bridge
import Servant.Foreign
import Servant.PureScript.CodeGen
import Servant.PureScript.Internal
import System.Directory
import System.FilePath
import System.IO (IOMode (..), withFile)
import Text.PrettyPrint.Mainland (hPutDocLn)

-- | Standard entry point - just create a purescript module with default settings
--   for accessing the servant API.
writeAPIModule ::
  forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) PSType api,
    GenerateList PSType (Foreign PSType api),
    HasBridge bridgeSelector
  ) =>
  FilePath ->
  Proxy bridgeSelector ->
  Proxy api ->
  IO ()
writeAPIModule = writeAPIModuleWithSettings defaultSettings

writeAPIModuleWithSettings ::
  forall bridgeSelector api.
  ( HasForeign (PureScript bridgeSelector) PSType api,
    GenerateList PSType (Foreign PSType api),
    HasBridge bridgeSelector
  ) =>
  Settings ->
  FilePath ->
  Proxy bridgeSelector ->
  Proxy api ->
  IO ()
writeAPIModuleWithSettings opts root pBr pAPI = do
  writeModule (opts ^. apiModuleName)
  T.putStrLn "\nSuccessfully created your servant API purescript functions!"
  T.putStrLn "Please make sure you have purescript-servant-support and purescript-bridge-json-helpers installed\n"
  where
    apiList = apiToList pAPI pBr

    writeModule :: Text -> IO ()
    writeModule mName =
      let baseFileName = joinPath . map T.unpack . T.splitOn "." $ mName
          pursModuleFile = baseFileName <> ".purs"
          jsModuleFile = baseFileName <> ".js"
          pursModulePath = root </> pursModuleFile
          jsModulePath = root </> jsModuleFile
          mDir = takeDirectory baseFileName
          contents = genModule opts apiList
       in do
            unlessM (doesDirectoryExist mDir) $ createDirectoryIfMissing True mDir
            withFile pursModulePath WriteMode $ flip hPutDocLn contents
            withFile jsModulePath WriteMode $ \h -> do
              hPutDocLn h "'use strict';"
              hPutDocLn h ""
              hPutDocLn h "exports.encodeURIComponent = encodeURIComponent"
              hPutDocLn h ""

-- | Use this function for implementing 'parseUrlPiece' in your FromHttpApiData instances
--   in order to be compatible with the generated PS code.
--
-- >
-- > instance ToHttpApiData MyDataType where
-- >   toUrlPiece = jsonToUrlPiece
-- >   toHeader   = jsonToHeader
-- >
-- > instance FromHttpApiData MyDataType where
-- >   parseUrlPiece = jsonParseUrlPiece
-- >   parseHeader   = jsonParseHeader
-- >
jsonParseUrlPiece :: FromJSON a => Text -> Either Text a
jsonParseUrlPiece = jsonParseHeader . T.encodeUtf8

-- | Use this function for implementing 'toUrlPiece' in your ToHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonToUrlPiece :: ToJSON a => a -> Text
jsonToUrlPiece = T.decodeUtf8 . jsonToHeader

-- | Use this function for implementing 'parseHeader' in your FromHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonParseHeader :: FromJSON a => ByteString -> Either Text a
jsonParseHeader = first T.pack . eitherDecodeStrict

-- | Use this function for implementing 'toHeader' in your ToHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonToHeader :: ToJSON a => a -> ByteString
jsonToHeader = BS.toStrict . encode
