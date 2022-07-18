{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Plutus.Blockfrost.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text
import GHC.Generics (Generic)

data BlockfrostConfig =
    BlockfrostConfig { bfTokenPath :: Text }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
