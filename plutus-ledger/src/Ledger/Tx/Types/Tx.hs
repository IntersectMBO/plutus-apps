{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ledger.Tx.Types.Tx (
    Tx(..),
    inputs,
    collateralInputs,
    outputs,
    validRange,
    signatures,
    fee,
    mint,
    mintScripts,
    scriptWitnesses,
    datumWitnesses,
    metadata,
    pubKeyTxInputs,
    scriptTxInputs,
) where

import Codec.CBOR.Write qualified as Write
import Codec.Serialise (Serialise, encode)
import Control.DeepSeq (NFData)
import Control.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteArray qualified as BA
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Ledger.Crypto
import Ledger.DCert.Orphans ()
import Ledger.Slot
import Ledger.Tx.Orphans ()
import Ledger.Tx.Types.Certificate
import Ledger.Tx.Types.TxInput
import Ledger.Tx.Types.Withdrawal
import Plutus.V1.Ledger.Api (BuiltinByteString, TxOut)
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Value as V
import PlutusTx.Lattice


