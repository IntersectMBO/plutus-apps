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


module Ledger.Tx.Orphans.V2 where

import Ledger.Scripts.Orphans ()

import Codec.Serialise (Serialise)
import Data.Aeson (FromJSON, ToJSON)
import Ledger.Credential.Orphans ()

import Ledger.Address.Orphans ()
import Ledger.Builtins.Orphans ()
import Ledger.Value.Orphans ()

import Plutus.V2.Ledger.Api

deriving anyclass instance ToJSON OutputDatum
deriving anyclass instance FromJSON OutputDatum
deriving anyclass instance Serialise OutputDatum

deriving anyclass instance ToJSON TxOut
deriving anyclass instance FromJSON TxOut
deriving anyclass instance Serialise TxOut
