{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}

module Plutus.Blockfrost.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Text (Text)
import GHC.Generics (Generic)

import Cardano.Api (NetworkId)

import Blockfrost.Client

newtype BlockfrostConfig =
    BlockfrostConfig { bfTokenPath :: FilePath }
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data BlockfrostEnv = BlockfrostEnv { envBfTokenPath :: FilePath
                                   , envNetworkId   :: NetworkId
                                   }

data TxResponse = TxResponse { _txHash        :: TxHash
                             , _invalidBefore :: Maybe Text
                             , _invalidAfter  :: Maybe Text
                             , _utxosInputs   :: [UtxoInput]
                             , _utxosOutpus   :: [UtxoOutput]
                             , _datumsMap     :: Map Text ScriptDatum
                             , _redeemersMap  :: Map Integer (ValidationPurpose, ScriptDatum)
                             , _scriptsMap    :: Map Text ScriptCBOR
                             }
