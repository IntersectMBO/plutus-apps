{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module RoundTrip.Spec where

import Data.Aeson (FromJSON, ToJSON (toJSON), fromJSON, eitherDecode, encode)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Language.PureScript.Bridge (BridgePart, Language (..), SumType, buildBridge, defaultBridge, defaultSwitch, mkSumType, writePSTypes, writePSTypesWith, equal, order, genericShow, functor)
import Language.PureScript.Bridge.CodeGenSwitches (ArgonautOptions (ArgonautOptions), genArgonaut)
import Language.PureScript.Bridge.TypeParameters (A)
import RoundTrip.Types
import System.Directory (removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual)
import Test.Hspec (Spec, aroundAll_, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)
import Test.Hspec.QuickCheck (prop)

myBridge :: BridgePart
myBridge = defaultBridge

myTypes :: [SumType 'Haskell]
myTypes =
  [ equal <*> (genericShow <*> (order <*> mkSumType)) $ Proxy @TestData,
    equal <*> (genericShow <*> (order <*> mkSumType)) $ Proxy @TestSum,
    functor <*> (equal <*> (genericShow <*> (order <*> mkSumType))) $ Proxy @(TestRecord A),
    equal <*> (genericShow <*> (order <*> mkSumType)) $ Proxy @TestNewtype,
    equal <*> (genericShow <*> (order <*> mkSumType)) $ Proxy @TestNewtypeRecord,
    equal <*> (genericShow <*> (order <*> mkSumType)) $ Proxy @TestNestedSum,
    equal <*> (genericShow <*> (order <*> mkSumType)) $ Proxy @TestEnum
  ]

roundtripSpec :: Spec
roundtripSpec = do
  aroundAll_ withProject $
    describe "writePSTypesWith" do
      it "should be buildable" do
        (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["build"] ""
        assertEqual (stdout <> stderr) exitCode ExitSuccess
      prop "should produce aeson-compatible argonaut instances" $
        \testData -> do
          (exitCode, stdout, stderr) <- readProcessWithExitCode "spago" ["run"] $ toString $ encode @TestData testData
          assertEqual stdout exitCode ExitSuccess
          assertEqual stdout (eitherDecode (fromString stdout)) $ Right testData
  where
    withProject runSpec =
      withCurrentDirectory "test/RoundTrip/app" $ generate *> runSpec

    generate = do
      writePSTypesWith
        (defaultSwitch <> genArgonaut ArgonautOptions)
        "src"
        (buildBridge myBridge)
        myTypes
