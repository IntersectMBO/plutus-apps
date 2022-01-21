{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Applicative
import Control.Exception (bracket, bracket_)
import Control.Lens
import Data.Aeson
import Data.Proxy
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.IO as T
import Data.Typeable
import GHC.Generics
import Language.PureScript.Bridge
import Language.PureScript.Bridge.PSTypes
import Servant.API hiding (toHeader)
import Servant.Foreign hiding (toHeader)
import Servant.PureScript
import Servant.PureScript.CodeGen
import Servant.PureScript.Internal
import System.Directory (removeDirectoryRecursive, removeFile, withCurrentDirectory, doesFileExist, doesDirectoryExist)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual)
import Test.Hspec (aroundAll_, describe, hspec, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Control.Monad (when)

newtype Hello = Hello
  { message :: Text
  }
  deriving (Generic, Eq, Ord)

instance FromJSON Hello

instance ToJSON Hello

newtype TestHeader = TestHeader Text deriving (Generic, Show, Eq)

instance ToJSON TestHeader

type MyAPI =
  Header "TestHeader" TestHeader
    :> QueryParam "globalParam" Int
    :> ( ( "hello"
             :> ( ( QueryFlag "myFlag"
                      :> QueryParam "myParam" Text
                      :> QueryParams "myParams" Hello
                      :> ReqBody '[JSON] (Either (Int, String) Hello)
                      :> Get '[JSON] Hello
                  )
                    :<|> Capture "name" Text :> Get '[JSON] (Maybe Hello)
                )
         )
           :<|> Header "LocalHeader" TestHeader :> "testHeader" :> Get '[JSON] TestHeader
           :<|> "by" :> Get '[JSON] Int
       )

myApiProxy :: Proxy MyAPI
myApiProxy = Proxy

mySettings = defaultSettings 
  & addGlobalQueryParam "globalParam"
  & addGlobalHeader "TestHeader"
  & addTypes 
    [ toQueryValue . toPathSegment . equal . order . genericShow . argonaut $ mkSumType @Hello,
      toHeader . argonaut $ mkSumType @TestHeader
    ]

moduleTranslator :: BridgePart
moduleTranslator = do
  typeModule ^== "Main"
  t <- view haskType
  TypeInfo (_typePackage t) "ServerTypes" (_typeName t) <$> psTypeParameters

myBridge :: BridgePart
myBridge = defaultBridge <|> moduleTranslator

data MyBridge

instance HasBridge MyBridge where
  languageBridge _ = buildBridge myBridge

myBridgeProxy :: Proxy MyBridge
myBridgeProxy = Proxy

main :: IO ()
main = hspec $
  aroundAll_ withOutput $
    describe "output" $ do
      it "should match the golden output" $ do
        (exitCode, stdout, stderr) <-
          readProcessWithExitCode "diff" ["--color", "--unified", "--recursive", "../golden", "./src"] ""
        assertEqual stdout exitCode ExitSuccess
      it "should be buildable" $ do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertEqual (stdout <> stderr) exitCode ExitSuccess
  where
    withOutput runSpec = withCurrentDirectory "test/output" do
      exists <- doesDirectoryExist "src"
      when exists $ removeDirectoryRecursive "src"
      generateWithSettings mySettings "src" myBridgeProxy myApiProxy *> runSpec
