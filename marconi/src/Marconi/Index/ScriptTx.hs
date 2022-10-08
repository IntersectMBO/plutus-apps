{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Marconi.Index.ScriptTx where

import Codec.Serialise (deserialiseOrFail)
import Control.Lens.Operators ((^.))
import Data.ByteString qualified as BS
import Data.Foldable (forM_, toList)
import Data.Maybe (catMaybes, fromJust)
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import GHC.Generics (Generic)

import Cardano.Api (SlotNo)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
-- TODO Remove the following dependencies (and also cardano-ledger-*
-- package dependencies in cabal file) when fromShelleyBasedScript is
-- exported from cardano-node PR:
-- https://github.com/input-output-hk/cardano-node/pull/4386
import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Core qualified
import Cardano.Ledger.Crypto qualified as LedgerCrypto
import Cardano.Ledger.Keys qualified as LedgerShelley
import Cardano.Ledger.Shelley.Scripts qualified as LedgerShelley
import Cardano.Ledger.ShelleyMA.Timelocks qualified as Timelock

import RewindableIndex.Index.VSqlite (SqliteIndex)
import RewindableIndex.Index.VSqlite qualified as Ix


newtype Depth = Depth Int

newtype ScriptAddress = ScriptAddress Shelley.ScriptHash
  deriving (Show, Eq)
newtype TxCbor = TxCbor BS.ByteString
  deriving (Show)
  deriving newtype (SQL.ToField, SQL.FromField)

-- * SQLite

data ScriptTxRow = ScriptTxRow
  { scriptAddress :: !ScriptAddress
  , txCbor        :: !TxCbor
  } deriving (Generic)

instance SQL.ToField ScriptAddress where
  toField (ScriptAddress hash)  = SQL.SQLBlob . Shelley.serialiseToRawBytes $ hash
instance SQL.FromField ScriptAddress where
  fromField f = SQL.fromField f >>=
    either
      (const cantDeserialise)
      (\b -> maybe cantDeserialise (return . ScriptAddress) $ Shelley.deserialiseFromRawBytes Shelley.AsScriptHash b) . deserialiseOrFail
    where
      cantDeserialise = SQL.returnError SQL.ConversionFailed f "Cannot deserialise address."

instance SQL.ToRow ScriptTxRow where
  toRow o = [SQL.toField $ scriptAddress o, SQL.toField $ txCbor o]

-- * Indexer

type Query = ScriptAddress
type Result = [TxCbor]

data ScriptTxUpdate = ScriptTxUpdate
  { txScripts :: [(TxCbor, [ScriptAddress])]
  , slotNo    :: !SlotNo
  } deriving (Show)

type ScriptTxIndex = SqliteIndex ScriptTxUpdate () Query Result


toUpdate :: forall era . C.IsCardanoEra era => [C.Tx era] -> SlotNo -> ScriptTxUpdate
toUpdate txs = ScriptTxUpdate txScripts'
  where
    txScripts' = map (\tx -> (TxCbor $ C.serialiseToCBOR tx, getTxScripts tx)) txs

getTxBodyScripts :: forall era . C.TxBody era -> [ScriptAddress]
getTxBodyScripts body = let
    hashesMaybe :: [Maybe C.ScriptHash]
    hashesMaybe = case body of
      Shelley.ShelleyTxBody shelleyBasedEra _ scripts _ _ _ -> flip map scripts $ \script ->
        case fromShelleyBasedScript shelleyBasedEra script of
          Shelley.ScriptInEra _ script' -> Just $ C.hashScript script'
      _ -> [] -- Byron transactions have no scripts
    hashes = catMaybes hashesMaybe :: [Shelley.ScriptHash]
  in map ScriptAddress hashes

getTxScripts :: forall era . C.Tx era -> [ScriptAddress]
getTxScripts (C.Tx txBody _ws) = getTxBodyScripts txBody

open :: (ScriptTxIndex -> ScriptTxUpdate -> IO [()]) -> FilePath -> Depth -> IO ScriptTxIndex
open onInsert dbPath (Depth k) = do
  ix <- fromJust <$> Ix.newBoxed query store onInsert k ((k + 1) * 2) dbPath
  let c = ix ^. Ix.handle
  SQL.execute_ c "CREATE TABLE IF NOT EXISTS script_transactions (scriptAddress TEXT NOT NULL, txCbor BLOB NOT NULL)"
  SQL.execute_ c "CREATE INDEX IF NOT EXISTS script_address ON script_transactions (scriptAddress)"
  pure ix

  where
    store :: ScriptTxIndex -> IO ()
    store ix = do
      buffered <- Ix.getBuffer $ ix ^. Ix.storage
      let rows = do
            ScriptTxUpdate txScriptAddrs _slotNo <- buffered
            (txCbor', scriptAddrs) <- txScriptAddrs
            scriptAddr <- scriptAddrs
            pure $ ScriptTxRow scriptAddr txCbor'
      SQL.execute_ (ix ^. Ix.handle) "BEGIN"
      forM_ rows $
        SQL.execute (ix ^. Ix.handle) "INSERT INTO script_transactions (scriptAddress, txCbor) VALUES (?, ?)"
      SQL.execute_ (ix ^. Ix.handle) "COMMIT"

query :: ScriptTxIndex -> Query -> [ScriptTxUpdate] -> IO Result
query ix scriptAddress' memory = let
    filterByScriptAddress :: [ScriptTxUpdate] -> [TxCbor]
    filterByScriptAddress updates' = do
      ScriptTxUpdate update _slotNo <- updates'
      map fst $ filter (\(_, addrs) -> scriptAddress' `elem` addrs) update
  in do
  persisted :: [SQL.Only TxCbor] <- SQL.query (ix ^. Ix.handle)
    "SELECT txCbor FROM script_transactions WHERE scriptAddress = ?" (SQL.Only scriptAddress')

  buffered <- Ix.getBuffer $ ix ^. Ix.storage
  let
    both :: [TxCbor]
    both = filterByScriptAddress memory
      <> filterByScriptAddress buffered
      <> map (\(SQL.Only txCbor') -> txCbor') persisted

  return both

-- * Copy-paste
--
-- | TODO: Remove when the following function is exported from Cardano.Api.Script
-- PR: https://github.com/input-output-hk/cardano-node/pull/4386
fromShelleyBasedScript  :: Shelley.ShelleyBasedEra era
                        -> Cardano.Ledger.Core.Script (Shelley.ShelleyLedgerEra era)
                        -> Shelley.ScriptInEra era
fromShelleyBasedScript era script =
  case era of
    Shelley.ShelleyBasedEraShelley ->
      Shelley.ScriptInEra Shelley.SimpleScriptV1InShelley $
      Shelley.SimpleScript Shelley.SimpleScriptV1 $
      fromShelleyMultiSig script
    Shelley.ShelleyBasedEraAllegra ->
      Shelley.ScriptInEra Shelley.SimpleScriptV2InAllegra $
      Shelley.SimpleScript Shelley.SimpleScriptV2 $
      fromAllegraTimelock Shelley.TimeLocksInSimpleScriptV2 script
    Shelley.ShelleyBasedEraMary ->
      Shelley.ScriptInEra Shelley.SimpleScriptV2InMary $
      Shelley.SimpleScript Shelley.SimpleScriptV2 $
      fromAllegraTimelock Shelley.TimeLocksInSimpleScriptV2 script
    Shelley.ShelleyBasedEraAlonzo ->
      case script of
        Alonzo.TimelockScript s ->
          Shelley.ScriptInEra Shelley.SimpleScriptV2InAlonzo $
          Shelley.SimpleScript Shelley.SimpleScriptV2 $
          fromAllegraTimelock Shelley.TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript Alonzo.PlutusV1 s ->
          Shelley.ScriptInEra Shelley.PlutusScriptV1InAlonzo $
          Shelley.PlutusScript Shelley.PlutusScriptV1 $
          Shelley.PlutusScriptSerialised s
        Alonzo.PlutusScript Alonzo.PlutusV2 _ ->
          error "fromShelleyBasedScript: PlutusV2 not supported in Alonzo era"
    Shelley.ShelleyBasedEraBabbage ->
      case script of
        Alonzo.TimelockScript s ->
          Shelley.ScriptInEra Shelley.SimpleScriptV2InBabbage $
          Shelley.SimpleScript Shelley.SimpleScriptV2 $
          fromAllegraTimelock Shelley.TimeLocksInSimpleScriptV2 s
        Alonzo.PlutusScript Alonzo.PlutusV1 s ->
          Shelley.ScriptInEra Shelley.PlutusScriptV1InBabbage $
          Shelley.PlutusScript Shelley.PlutusScriptV1 $
          Shelley.PlutusScriptSerialised s
        Alonzo.PlutusScript Alonzo.PlutusV2 s ->
          Shelley.ScriptInEra Shelley.PlutusScriptV2InBabbage $
          Shelley.PlutusScript Shelley.PlutusScriptV2 $
          Shelley.PlutusScriptSerialised s

  where
  fromAllegraTimelock :: Shelley.TimeLocksSupported lang
                      -> Timelock.Timelock LedgerCrypto.StandardCrypto
                      -> Shelley.SimpleScript lang
  fromAllegraTimelock timelocks = go
    where
      go (Timelock.RequireSignature kh) = Shelley.RequireSignature
                                            (Shelley.PaymentKeyHash (LedgerShelley.coerceKeyRole kh))
      go (Timelock.RequireTimeExpire t) = Shelley.RequireTimeBefore timelocks t
      go (Timelock.RequireTimeStart  t) = Shelley.RequireTimeAfter  timelocks t
      go (Timelock.RequireAllOf      s) = Shelley.RequireAllOf (map go (toList s))
      go (Timelock.RequireAnyOf      s) = Shelley.RequireAnyOf (map go (toList s))
      go (Timelock.RequireMOf      i s) = Shelley.RequireMOf i (map go (toList s))

  fromShelleyMultiSig :: LedgerShelley.MultiSig LedgerCrypto.StandardCrypto -> Shelley.SimpleScript lang
  fromShelleyMultiSig = go
    where
      go (LedgerShelley.RequireSignature kh)
                                  = Shelley.RequireSignature
                                      (Shelley.PaymentKeyHash (LedgerShelley.coerceKeyRole kh))
      go (LedgerShelley.RequireAllOf s) = Shelley.RequireAllOf (map go s)
      go (LedgerShelley.RequireAnyOf s) = Shelley.RequireAnyOf (map go s)
      go (LedgerShelley.RequireMOf m s) = Shelley.RequireMOf m (map go s)
