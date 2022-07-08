{-# LANGUAGE DeriveAnyClass #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Ledger.Typed.Scripts.Orphans where

import Data.Aeson (FromJSON, ToJSON)
import Ledger.Tx.Orphans ()
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as PV1

deriving instance ToJSON (PV1.TypedValidator a)
deriving instance FromJSON (PV1.TypedValidator a)
deriving instance ToJSON PV1.ConnectionError
deriving instance FromJSON PV1.ConnectionError
