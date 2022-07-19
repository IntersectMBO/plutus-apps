{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Plutus.Blockfrost.Types where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Cardano.Api (NetworkId)

newtype BlockfrostConfig =
    BlockfrostConfig { bfTokenPath :: FilePath }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data BlockfrostEnv = BlockfrostEnv { envBfTokenPath :: FilePath
                                   , envNetworkId   :: NetworkId }
