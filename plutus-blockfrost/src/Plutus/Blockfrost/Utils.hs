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
import Plutus.V1.Ledger.Scripts qualified as PS

fromSucceed :: Result a -> a
fromSucceed (Error a)   = error $ show a
fromSucceed (Success a) = a


toBlockfrostDatumHash :: PS.DatumHash -> Blockfrost.DatumHash
toBlockfrostDatumHash = fromString . show
