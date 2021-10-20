{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RoundTrip.Spec where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON (toJSON), eitherDecode, encode, fromJSON)
import Data.ByteString.Lazy (stripSuffix, hGetContents)
import Data.ByteString.Lazy.UTF8 (toString, fromString)
import Data.List (isInfixOf)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Language.PureScript.Bridge (BridgePart, Language (..), SumType, argonaut, buildBridge, defaultBridge, defaultSwitch, equal, functor, genericShow, mkSumType, order, writePSTypes, writePSTypesWith)
import Language.PureScript.Bridge.TypeParameters (A)
import RoundTrip.Types
import System.Directory (removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (..), hSetBuffering, hPutStrLn, stdout, stderr, hFlush)
import System.Process (CreateProcess (std_in, std_out), StdStream (CreatePipe), createProcess, getProcessExitCode, proc, readProcessWithExitCode, terminateProcess, waitForProcess)
import Test.HUnit (assertBool, assertEqual)
import Test.Hspec (Spec, around, aroundAll_, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck.Property (Testable (property))
import Data.Maybe (fromMaybe)
import Test.QuickCheck (verbose, once, noShrinking, withMaxSuccess)

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ equal . genericShow . order . argonaut $ mkSumType @TestData,
    equal . genericShow . order . argonaut $ mkSumType @TestSum,
    functor . equal . genericShow . order . argonaut $ mkSumType @(TestRecord A),
    equal . genericShow . order . argonaut $ mkSumType @TestNewtype,
    equal . genericShow . order . argonaut $ mkSumType @TestNewtypeRecord,
    equal . genericShow . order . argonaut $ mkSumType @TestEnum,
    equal . genericShow . order . argonaut $ mkSumType @MyUnit
  ]

roundtripSpec :: Spec
roundtripSpec = do
  aroundAll_ withProject $
    describe "writePSTypesWith" do
      it "should be buildable" do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertEqual (stdout <> stderr) exitCode ExitSuccess
      it "should not warn of unused packages buildable" do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertBool stderr $ not $ "[warn]" `isInfixOf` stderr
      it "should produce aeson-compatible argonaut instances" $ 
        property $
          \testData -> bracket runApp killApp $
            \(hin, hout, hproc) -> do
              hPutStrLn hin $ toString $ encode @TestData testData
              output <- hGetContents hout
              assertEqual (toString output) ExitSuccess =<< waitForProcess hproc
              assertEqual (toString output) (Right testData) $ eitherDecode @TestData output
  where
    runApp = do
      (Just hin, Just hout, _, hproc) <-
        createProcess (proc "spago" ["run"]) {std_in = CreatePipe, std_out = CreatePipe}
      hSetBuffering hin LineBuffering
      hSetBuffering hout LineBuffering
      pure (hin, hout, hproc)

    killApp (_, _, hproc) = terminateProcess hproc

    withProject runSpec =
      withCurrentDirectory "test/RoundTrip/app" $ generate *> runSpec

    generate = do
      writePSTypesWith
        defaultSwitch
        "src"
        (buildBridge myBridge)
        myTypes
