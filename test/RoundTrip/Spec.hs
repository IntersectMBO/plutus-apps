{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module RoundTrip.Spec where

import Control.Exception (bracket)
import Data.Aeson (FromJSON, ToJSON (toJSON), eitherDecode, encode, fromJSON)
import Data.ByteString.Lazy (hGetContents, stripSuffix)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Language.PureScript.Bridge (BridgePart, Language (..), SumType, argonaut, buildBridge, defaultBridge, defaultSwitch, equal, functor, genericShow, mkSumType, order, writePSTypes, writePSTypesWith)
import Language.PureScript.Bridge.TypeParameters (A)
import RoundTrip.Types
import System.Directory (removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.IO (BufferMode (..), hFlush, hGetLine, hPutStrLn, hSetBuffering, stderr, stdout)
import System.Process (CreateProcess (..), StdStream (CreatePipe), createProcess, getProcessExitCode, proc, readProcessWithExitCode, terminateProcess, waitForProcess)
import Test.HUnit (assertBool, assertEqual)
import Test.Hspec (Spec, around, aroundAll_, around_, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (noShrinking, once, verbose, withMaxSuccess)
import Test.QuickCheck.Property (Testable (property))

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ equal . genericShow . order . argonaut $ mkSumType @TestData,
    equal . genericShow . order . argonaut $ mkSumType @TestSum,
    equal . genericShow . order . argonaut $ mkSumType @TestRecursiveA,
    equal . genericShow . order . argonaut $ mkSumType @TestRecursiveB,
    functor . equal . genericShow . order . argonaut $ mkSumType @(TestRecord A),
    equal . genericShow . order . argonaut $ mkSumType @TestNewtype,
    equal . genericShow . order . argonaut $ mkSumType @TestNewtypeRecord,
    equal . genericShow . order . argonaut $ mkSumType @TestMultiInlineRecords,
    equal . genericShow . order . argonaut $ mkSumType @TestTwoFields,
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
      around withApp $
        it "should produce aeson-compatible argonaut instances" $
          \(hin, hout, herr, hproc) ->
            property $
              \testData -> do
                let input = toString $ encode @TestData testData
                hPutStrLn hin input
                err <- hGetLine herr
                output <- hGetLine hout
                assertEqual input "" err
                assertEqual output (Right testData) $ eitherDecode @TestData $ fromString output
  where
    withApp = bracket runApp killApp
    runApp = do
      (Just hin, Just hout, Just herr, hproc) <-
        createProcess
          (proc "spago" ["run"])
            { std_in = CreatePipe,
              std_out = CreatePipe,
              std_err = CreatePipe
            }
      hSetBuffering hin LineBuffering
      hSetBuffering hout LineBuffering
      hSetBuffering herr LineBuffering
      -- flush stderr output from build
      _ <- hGetLine herr
      -- wait for initial log message
      _ <- hGetLine hout
      pure (hin, hout, herr, hproc)

    killApp (_, _, _, hproc) = terminateProcess hproc

    withProject runSpec =
      withCurrentDirectory "test/RoundTrip/app" $ generate *> runSpec

    generate = do
      writePSTypesWith
        defaultSwitch
        "src"
        (buildBridge myBridge)
        myTypes
