{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TypeApplications   #-}

module Plutus.Blockfrost.Utils where

import Data.Aeson
import Data.String

import Blockfrost.Client as Blockfrost
import Plutus.V1.Ledger.Api qualified (DatumHash, RedeemerHash)
import Plutus.V1.Ledger.Scripts qualified as PS

class Show a => ToBlockfrostScriptHash a where
  toBlockfrostScriptHash :: a -> Blockfrost.ScriptHash
  toBlockfrostScriptHash = fromString . show

instance ToBlockfrostScriptHash PS.ValidatorHash
instance ToBlockfrostScriptHash PS.MintingPolicyHash
instance ToBlockfrostScriptHash PS.StakeValidatorHash

class Show a => ToBlockfrostDatumHash a where
  toBlockfrostDatumHash :: a -> Blockfrost.DatumHash
  toBlockfrostDatumHash = fromString . show

instance ToBlockfrostDatumHash Plutus.V1.Ledger.Api.DatumHash
instance ToBlockfrostDatumHash Plutus.V1.Ledger.Api.RedeemerHash

fromSucceed :: Result a -> a
fromSucceed (Error a)   = error $ show a
fromSucceed (Success a) = a

