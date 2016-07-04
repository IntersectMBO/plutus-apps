{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Servant.PureScript (
  HasBridge
, languageBridge
, defaultBridge
, defaultBridgeProxy
, DefaultBridge
, writeAPIModule
, writeAPIModuleWithSettings
, Settings (..)
, apiModuleName
, readerParams
, standardImports
, defaultSettings
, addReaderParam
, jsonParseUrlPiece
, jsonToUrlPiece
, jsonParseHeader
, jsonToHeader
) where


import           Data.Aeson
import           Data.Bifunctor
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy        as BS
import           Data.Monoid
import           Data.Proxy
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Language.PureScript.Bridge
import           Network.HTTP.Types          (urlDecode, urlEncode)
import           Servant.Foreign
import           Servant.PureScript.CodeGen
import           Servant.PureScript.Internal
import           System.Directory
import           System.FilePath
import           System.IO                   (IOMode (..), withFile)
import           Text.PrettyPrint.Mainland   (hPutDocLn)

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
    unlessM (doesDirectoryExist mDir) $ createDirectoryIfMissing True mDir
    withFile mPath WriteMode $ flip hPutDocLn contents
  where
    mFile = (joinPath . map T.unpack . T.splitOn "." $ _apiModuleName opts) <> ".purs"
    mPath = root </> mFile
    mDir = takeDirectory mPath


-- | Use this function for implementing 'parseUrlPiece' in your FromHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonParseUrlPiece :: FromJSON a => Text -> Either Text a
jsonParseUrlPiece = jsonParseHeader . T.encodeUtf8

-- | Use this function for implementing 'toUrlPiece' in your ToHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonToUrlPiece :: ToJSON a => a -> Text
jsonToUrlPiece = T.decodeUtf8 . jsonToHeader

-- | Use this function for implementing 'parseHeader' in your FromHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonParseHeader :: FromJSON a => ByteString -> Either Text a
jsonParseHeader = first T.pack . eitherDecodeStrict . urlDecode True

-- | Use this function for implementing 'toHeader' in your ToHttpApiData instances
--   in order to be compatible with the generated PS code.
jsonToHeader :: ToJSON a => a -> ByteString
jsonToHeader = urlEncode True . BS.toStrict . encode
