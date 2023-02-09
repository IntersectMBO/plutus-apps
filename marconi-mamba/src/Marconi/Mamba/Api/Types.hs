{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This module provides support for writing handlers for JSON-RPC endpoints
module Marconi.Mamba.Api.Types  where

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Exception (Exception)
import Control.Lens (makeClassy)
import Data.Aeson (ToJSON (toEncoding, toJSON), defaultOptions, genericToEncoding, object, (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Char qualified as Char
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (Settings)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.ChainIndex.Types as Export (TargetAddresses)

-- | Type represents http port for JSON-RPC
type RpcPortNumber = Int

data CliArgs = CliArgs
  { socket          :: FilePath             -- ^ POSIX socket file to communicate with cardano node
  , dbPath          :: FilePath             -- ^ filepath to local sqlite for utxo index table
  , httpPort        :: Maybe Int            -- ^ optional tcp/ip port number for JSON-RPC http server
  , networkId       :: C.NetworkId          -- ^ cardano network id
  , targetAddresses :: TargetAddresses      -- ^ white-space sepparated list of Bech32 Cardano Shelley addresses
  } deriving (Show)

newtype UtxoIndexerWrapper = UtxoIndexerWrapper
    { unWrap :: TMVar Utxo.UtxoIndexer           -- ^ for query thread to access in-memory utxos
    }

data UtxoIndexerEnv = UtxoIndexerEnv
    { _uiIndexer    :: UtxoIndexerWrapper
    , _uiQaddresses :: TargetAddresses        -- ^ user provided addresses to filter
    }
makeClassy ''UtxoIndexerEnv

-- | JSON-RPC configuration
data JsonRpcEnv = JsonRpcEnv
    { _httpSettings :: Settings               -- ^ HTTP server setting
    , _queryEnv     :: UtxoIndexerEnv         -- ^ used for query sqlite
    }
makeClassy ''JsonRpcEnv


data QueryExceptions
    = AddressNotInListError QueryExceptions
    | AddressConversionError QueryExceptions
    | TxRefConversionError QueryExceptions
    | QueryError String
    deriving stock Show
    deriving anyclass  Exception

-- from cardano-node: https://github.com/input-output-hk/cardano-node/blob/master/cardano-api/src/Cardano/Api/ScriptData.hs#L444-L447
-- | JSON strings that are base16 encoded and prefixed with 'bytesPrefix' will
-- be encoded as CBOR bytestrings.
bytesPrefix :: Text
bytesPrefix = "0x"

instance ToJSON ByteString  where
  toJSON bs
      | Right s <- Text.decodeUtf8' bs, Text.all Char.isPrint s = Aeson.String s
      | otherwise
      = Aeson.String (bytesPrefix <> Text.decodeLatin1 (Base16.encode bs))

instance ToJSON Utxo.Utxo where
  toJSON (Utxo.Utxo addr tId tIx dtum dtumHash val scrpt scrptHash) = object
    ["address"            .=  addr
    , "txId"              .= tId
    , "txIx"              .= tIx
    , "datum"             .= (C.serialiseToCBOR <$> dtum)
    , "datumHash"         .= dtumHash
    , "value"             .= val
    , "inlineScript"      .= (scriptToCBOR <$>scrpt)
    , "inlineScriptHash"  .= scrptHash
    ]

data UtxoReport = UtxoReport
    { urAddress :: Text
    , urReport  :: [Utxo.UtxoRow]
    } deriving (Eq, Ord, Generic)

instance ToJSON UtxoReport where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON C.AddressAny where
    toJSON  = Aeson.String . C.serialiseAddress

instance ToJSON (Utxo.StorableQuery Utxo.UtxoHandle) where
  toJSON (Utxo.UtxoAddress addr) = toJSON addr

instance ToJSON C.BlockNo

instance ToJSON Utxo.UtxoRow where
  toJSON (Utxo.UtxoRow u b s h) = object
    [ "utxo" .= u
    ,  "blockNo" .= b
    ,  "slotNo" .= s
    ,  "blockHeaderHash" .= h]

-- | convert to Script to CBOR bytestring
scriptToCBOR :: Shelley.ScriptInAnyLang -> ByteString
scriptToCBOR (Shelley.ScriptInAnyLang(C.SimpleScriptLanguage C.SimpleScriptV1) script) =
  C.serialiseToCBOR script
scriptToCBOR (Shelley.ScriptInAnyLang(C.SimpleScriptLanguage C.SimpleScriptV2) script) =
  C.serialiseToCBOR script
scriptToCBOR (Shelley.ScriptInAnyLang(C.PlutusScriptLanguage C.PlutusScriptV1) script) =
  C.serialiseToCBOR script
scriptToCBOR (Shelley.ScriptInAnyLang(C.PlutusScriptLanguage C.PlutusScriptV2) script) =
  C.serialiseToCBOR script
