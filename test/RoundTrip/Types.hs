{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

module RoundTrip.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy (Proxy (..))
import GHC.Generics (Generic)
import Language.PureScript.Bridge (BridgePart, Language (..), SumType, buildBridge, defaultBridge, mkSumType, writePSTypes, writePSTypesWith, defaultSwitch)
import Language.PureScript.Bridge.CodeGenSwitches (genArgonaut, ArgonautOptions (ArgonautOptions))
import Language.PureScript.Bridge.TypeParameters (A)
import System.Directory (removeDirectoryRecursive, removeFile, withCurrentDirectory)
import System.Exit (ExitCode (ExitSuccess))
import System.Process (readProcessWithExitCode)
import Test.HUnit (assertEqual)
import Test.Hspec (Spec, aroundAll_, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

data TestData
  = Maybe (Maybe TestSum)
  | Either (Either String TestSum)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TestData

instance ToJSON TestData

data TestSum
  = Nullary
  | Bool Bool
  | Int Int
  | Number Double
  | String String
  | Array [String]
  | Record (TestRecord Int)
  | NestedRecord (TestRecord (TestRecord Int))
  | NT TestNewtype
  | NTRecord TestNewtypeRecord
  | Unit ()
  | Pair (Int, String)
  | Triple (Int, String, Bool)
  | Quad (Int, String, Bool, Double)
  | NestedSum TestNestedSum
  | Enum TestEnum
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TestSum

instance ToJSON TestSum

data TestRecord a = TestRecord
  { field1 :: String,
    field2 :: a
  }
  deriving (Show, Eq, Ord, Generic)

instance (FromJSON a) => FromJSON (TestRecord a)

instance (ToJSON a) => ToJSON (TestRecord a)

newtype TestNewtype = TestNewtype (TestRecord String)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TestNewtype

instance ToJSON TestNewtype

newtype TestNewtypeRecord = TestNewtypeRecord {unTestNewtypeRecord :: TestNewtype}
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TestNewtypeRecord

instance ToJSON TestNewtypeRecord

data TestNestedSum
  = Case1 String
  | Case2 Int
  | Case3 (TestRecord Int)
  deriving (Show, Eq, Ord, Generic)

instance FromJSON TestNestedSum

instance ToJSON TestNestedSum

data TestEnum
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Show, Eq, Ord, Bounded, Enum, Generic)

instance FromJSON TestEnum

instance ToJSON TestEnum
