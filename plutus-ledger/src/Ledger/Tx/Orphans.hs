{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Ledger.Tx.Orphans where

import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value (Object), object, (.:))
import Data.Aeson.Types (parseFail, prependFailure, typeMismatch)

import Cardano.Api qualified as C

import Ledger.Address.Orphans ()
import Ledger.Builtins.Orphans ()
import Ledger.Credential.Orphans ()
import Ledger.Scripts.Orphans ()
import Ledger.Tx.Orphans.V1 ()
import Ledger.Tx.Orphans.V2 ()
import Ledger.Value.Orphans ()

instance ToJSON (C.Tx C.BabbageEra) where
  toJSON tx =
    object [ "tx" .= C.serialiseToTextEnvelope Nothing tx ]

instance FromJSON (C.Tx C.BabbageEra) where
  parseJSON (Object v) = do
   envelope <- v .: "tx"
   either (const $ parseFail "Failed to parse BabbageEra 'tx' field from CardanoTx")
          pure
          $ C.deserialiseFromTextEnvelope (C.AsTx C.AsBabbageEra) envelope
  parseJSON invalid =
    prependFailure "parsing CardanoTx failed, " (typeMismatch "Object" invalid)
