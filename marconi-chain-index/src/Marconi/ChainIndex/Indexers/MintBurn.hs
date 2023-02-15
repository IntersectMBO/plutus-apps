{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans     #-}

{- | Mint/burn event indexer, the result of which is an sqlite database
   'mintburn.db' which has a table 'minting_policy_events' with the
   following fields:

     - slotNo          INT NOT NULL
     - blockHeaderHash INT NOT NULL
     - txId            BLOB NOT NULL
     - policyId        BLOB NOT NULL
     - assetName       TEXT NOT NULL
     - quantity        INT NOT NULL
     - redeemerIx      INT NOT NULL
     - redeemerData    BLOB NOT NULL
-}

module Marconi.ChainIndex.Indexers.MintBurn where

import Control.Lens (view, (&), (^.))
import Control.Lens qualified as L
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Short qualified as Short
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Word (Word64)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Data qualified as LA
import Cardano.Ledger.Alonzo.Scripts qualified as LA
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Alonzo.TxWitness qualified as LA
import Cardano.Ledger.Babbage.Tx qualified as LB
import Cardano.Ledger.Mary.Value qualified as LM
import Ouroboros.Consensus.Shelley.Eras qualified as OEra

import Marconi.ChainIndex.Orphans ()
import Marconi.Core.Storable qualified as RI

-- * Event

data TxMintEvent = TxMintEvent
  { txMintEventSlotNo          :: C.SlotNo
  , txMintEventBlockHeaderHash :: C.Hash C.BlockHeader
  , txMintEventTxAssets        :: NE.NonEmpty (C.TxId, NE.NonEmpty MintAsset)
  } deriving (Show, Eq, Ord)

data MintAsset = MintAsset
  { mintAssetPolicyId     :: C.PolicyId
  , mintAssetAssetName    :: C.AssetName
  , mintAssetQuantity     :: C.Quantity
  , mintAssetRedeemerIdx  :: Word64
  , mintAssetRedeemerData :: C.ScriptData
  } deriving (Show, Eq, Ord)

toUpdate :: C.BlockInMode C.CardanoMode -> Maybe TxMintEvent
toUpdate (C.BlockInMode (C.Block (C.BlockHeader slotNo blockHeaderHash _blockNo) txs) _) =
  case mapMaybe txMints txs of
    x : xs -> Just $ TxMintEvent slotNo blockHeaderHash (x NE.:| xs)
    _      -> Nothing

txMints :: C.Tx era -> Maybe (C.TxId, NE.NonEmpty MintAsset)
txMints (C.Tx txb _) = case txbMints txb of
  x : xs -> Just (C.getTxId txb, x NE.:| xs )
  _      -> Nothing

txbMints :: C.TxBody era -> [MintAsset]
txbMints txb = case txb of
  C.ShelleyTxBody era shelleyTx _ _ _ _ -> case era of
    C.ShelleyBasedEraShelley -> []
    C.ShelleyBasedEraAllegra -> []
    C.ShelleyBasedEraMary -> []
    C.ShelleyBasedEraAlonzo -> do
      (policyId, assetName, quantity, index', redeemer) <- getPolicyData txb $ LA.mint shelleyTx
      pure $ MintAsset policyId assetName quantity index' redeemer
    C.ShelleyBasedEraBabbage -> do
      (policyId, assetName, quantity, index', redeemer) <- getPolicyData txb $ LB.mint shelleyTx
      pure $ MintAsset policyId assetName quantity index' redeemer
  _ -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.

-- * Helpers

txRedeemers :: C.TxBody era -> Map.Map LA.RdmrPtr (LA.Data (C.ShelleyLedgerEra era), LA.ExUnits)
txRedeemers (C.ShelleyTxBody _ _ _ txScriptData _ _) = case txScriptData of
  C.TxBodyScriptData _proof _datum redeemers -> LA.unRedeemers redeemers
  C.TxBodyNoScriptData                       -> mempty
txRedeemers _ = mempty

mintRedeemers :: C.TxBody era -> [(Word64, (LA.Data (C.ShelleyLedgerEra era), LA.ExUnits))]
mintRedeemers txb = txRedeemers txb
  & Map.toList
  & filter (\(LA.RdmrPtr tag _, _) -> tag == LA.Mint)
  & map (\(LA.RdmrPtr _ w, a) -> (w, a))

getPolicyData :: C.TxBody era -> LM.Value OEra.StandardCrypto -> [(C.PolicyId, C.AssetName, C.Quantity, Word64, C.ScriptData)]
getPolicyData txb (LM.Value _ m) = do
  let
    policyIdList = Map.toList m
    getPolicyId index' = policyIdList !! fromIntegral index'
  ((maryPolicyID, assets), index'', (redeemer, _)) <- map (\(index', data_) -> (getPolicyId index', index', data_)) $ mintRedeemers txb
  (assetName, quantity) :: (LM.AssetName, Integer) <- Map.toList assets
  pure $ (fromMaryPolicyID maryPolicyID, fromMaryAssetName assetName, C.Quantity quantity, index'', fromAlonzoData redeemer)

-- ** Copy-paste

fromMaryPolicyID :: LM.PolicyID OEra.StandardCrypto -> C.PolicyId
fromMaryPolicyID (LM.PolicyID sh) = C.PolicyId (C.fromShelleyScriptHash sh) -- from cardano-api:src/Cardano/Api/Value.hs

fromMaryAssetName :: LM.AssetName -> C.AssetName
fromMaryAssetName (LM.AssetName n) = C.AssetName $ Short.fromShort n -- from cardano-api:src/Cardano/Api/Value.hs

fromAlonzoData :: LA.Data ledgerera -> C.ScriptData
fromAlonzoData = C.fromPlutusData . LA.getPlutusData -- from cardano-api:src/Cardano/Api/ScriptData.hs

-- * Sqlite

sqliteInit :: SQL.Connection -> IO ()
sqliteInit c = liftIO $ do
  SQL.execute_ c
    " CREATE TABLE IF NOT EXISTS        \
    \   minting_policy_events           \
    \   ( slotNo          INT NOT NULL  \
    \   , blockHeaderHash INT NOT NULL  \
    \   , txId            BLOB NOT NULL \
    \   , policyId        BLOB NOT NULL \
    \   , assetName       TEXT NOT NULL \
    \   , quantity        INT NOT NULL  \
    \   , redeemerIx      INT NOT NULL  \
    \   , redeemerData    BLOB NOT NULL)"
  SQL.execute_ c
    " CREATE INDEX IF NOT EXISTS               \
    \    minting_policy_events__txId_policyId  \
    \ ON minting_policy_events (txId, policyId)"

sqliteInsert :: SQL.Connection -> [TxMintEvent] -> IO ()
sqliteInsert c es = SQL.executeMany c template $ toRows =<< toList es
  where
    template =
      "INSERT INTO minting_policy_events \
      \ ( slotNo, blockHeaderHash, txId  \
      \ , policyId, assetName, quantity  \
      \ , redeemerIx, redeemerData )     \
      \ VALUES (?, ?, ?, ?, ?, ?, ?, ?)  "

    toRows :: TxMintEvent -> [[SQL.SQLData]]
    toRows e = do
      (txId, txMintAssets) <- NE.toList $ txMintEventTxAssets e
      mintAsset <- NE.toList txMintAssets
      pure
        [ SQL.toField $ txMintEventSlotNo e
        , SQL.toField $ txMintEventBlockHeaderHash e
        , SQL.toField txId
        , SQL.toField $ mintAssetPolicyId mintAsset
        , SQL.toField $ mintAssetAssetName mintAsset
        , SQL.toField $ mintAssetQuantity mintAsset
        , SQL.toField $ mintAssetRedeemerIdx mintAsset
        , SQL.toField $ mintAssetRedeemerData mintAsset
        ]

type Row = (C.SlotNo, C.Hash C.BlockHeader, C.TxId, C.PolicyId, C.AssetName, C.Quantity, Word64, C.ScriptData)

-- | Input rows must be sorted by C.SlotNo.
fromRows :: [Row] -> [TxMintEvent]
fromRows rows =  do
  rs@(r :| _) <- NE.groupBy ((==) `on` slotNo) rows -- group by SlotNo
  pure $ TxMintEvent (slotNo r) (hash r) $ do
    rs' <- NE.groupBy1 ((==) `on` txId) rs -- group by TxId
    pure (txId r, rowToMintAsset <$> rs')
  where
    slotNo = view L._1 :: Row -> C.SlotNo
    hash = view L._2 :: Row -> C.Hash C.BlockHeader
    txId = view L._3 :: Row -> C.TxId
    rowToMintAsset :: Row -> MintAsset
    rowToMintAsset row = MintAsset (row^.L._4) (row^.L._5) (row^.L._6) (row^.L._7) (row^.L._8)

sqliteSelectByTxIdPolicyId :: SQL.Connection -> (SQL.Query, [NamedParam]) -> C.TxId -> C.PolicyId -> IO [TxMintEvent]
sqliteSelectByTxIdPolicyId sqlCon (conditions, params) txId policyId =
  fromRows <$> SQL.queryNamed sqlCon query ([":txId" := txId, ":policyId" := policyId] <> params)
  where query =
          " SELECT slotNo, blockHeaderHash, txId, policyId                    \
          \      , assetName, quantity, redeemerIx, redeemerData              \
          \   FROM minting_policy_events                                      \
          \  WHERE txId = :txId AND policyId = :policyId " <> conditions <> " \
          \  ORDER BY slotNo, txId                                            "

sqliteSelectAll :: SQL.Connection -> (SQL.Query, [NamedParam]) -> IO [TxMintEvent]
sqliteSelectAll sqlCon (conditions, params) = fromRows <$> SQL.queryNamed sqlCon query params
  where
    whereClause = if conditions == "" then "" else " WHERE " <> conditions <> " "
    query =
      "   SELECT slotNo, blockHeaderHash, txId, policyId       \
      \        , assetName, quantity, redeemerIx, redeemerData \
      \     FROM minting_policy_events                         \
      \     " <> whereClause <> "                              \
      \ ORDER BY slotNo, txId                                  "

intervalToWhereClause :: RI.QueryInterval C.ChainPoint -> (SQL.Query, [NamedParam])
intervalToWhereClause qi = case qi of
  RI.QEverything -> ("", [])
  RI.QInterval from to
    | Just from' <- slotMaybe from, Just to' <- slotMaybe to -> ("slotNo BETWEEN :fromSlot AND :toSlot" , [":fromSlot" := from', ":toSlot" := to'])
    | Just to' <- slotMaybe to -> ("slotNo <= :toSlot" , [":toSlot" := to'])
    | otherwise -> ("FALSE", [])
    where
      slotMaybe :: C.ChainPoint -> Maybe C.SlotNo
      slotMaybe = \case
        C.ChainPoint slotNo _ -> Just slotNo
        C.ChainPointAtGenesis -> Nothing

groupBySlotAndHash :: [TxMintEvent] -> [TxMintEvent]
groupBySlotAndHash events = events
  & sort
  & groupBy (\e1 e2 -> txMintEventSlotNo e1 == txMintEventSlotNo e2 && txMintEventBlockHeaderHash e1 == txMintEventBlockHeaderHash e2)
  & concatMap (\case e : es -> [ TxMintEvent (txMintEventSlotNo e) (txMintEventBlockHeaderHash e) $ txMintEventTxAssets =<< (e :| es) ]
                     _      -> [])

-- * Indexer

data MintBurnHandle = MintBurnHandle
  { sqlConnection :: SQL.Connection
  , securityParam :: Int
  }

type MintBurnIndexer = RI.State MintBurnHandle

type instance RI.StorablePoint MintBurnHandle = C.ChainPoint

type instance RI.StorableMonad MintBurnHandle = IO

newtype instance RI.StorableEvent MintBurnHandle
  = MintBurnEvent TxMintEvent

data instance RI.StorableQuery MintBurnHandle
  = ByTxIdAndPolicyId C.TxId C.PolicyId
  | Everything

data instance RI.StorableResult MintBurnHandle
  = MintBurnResult [Row]

instance RI.Queryable MintBurnHandle where
  queryStorage queryInterval memoryEvents (MintBurnHandle sqlCon _k) query = case query of
    Everything                      -> toResult <$> sqliteSelectAll sqlCon interval
    ByTxIdAndPolicyId txId policyId -> toResult <$> sqliteSelectByTxIdPolicyId sqlCon interval txId policyId
    where
      toResult storedEvents = MintBurnResult $ do
        TxMintEvent slotNo blockHeaderHash txAssets <- storedEvents <> map coerce (toList memoryEvents)
        (txId, mintAssets) <- NE.toList txAssets
        MintAsset policyId assetName quantity redeemerIx redeemerData <- NE.toList mintAssets
        pure (slotNo, blockHeaderHash, txId, policyId, assetName, quantity, redeemerIx, redeemerData)

      interval = intervalToWhereClause queryInterval

instance RI.HasPoint (RI.StorableEvent MintBurnHandle) C.ChainPoint where
  getPoint (MintBurnEvent e) = C.ChainPoint (txMintEventSlotNo e) (txMintEventBlockHeaderHash e)

instance RI.Buffered MintBurnHandle where
  persistToStorage events h@(MintBurnHandle sqlCon _k) = do
    sqliteInsert sqlCon (map coerce $ toList events)
    pure h

  getStoredEvents (MintBurnHandle sqlCon k) =
    map MintBurnEvent . fromRows <$> SQL.query sqlCon query (SQL.Only k)
    where
      query =
        " SELECT slotNo, blockHeaderHash, txId, policyId, assetName, quantity  \
        \        redeemerIx, redeemerData                                      \
        \   FROM minting_policy_events                                         \
        \  WHERE slotNo >= (SELECT MAX(slotNo) - ? FROM minting_policy_events) \
        \  ORDER BY slotNo, txId                                               "

instance RI.Resumable MintBurnHandle where
  resumeFromStorage h = do
    events <- RI.getStoredEvents h
    pure $ map getChainPoint events <> [C.ChainPointAtGenesis]
    where
      getChainPoint (MintBurnEvent e) = C.ChainPoint (txMintEventSlotNo e) (txMintEventBlockHeaderHash e)

instance RI.Rewindable MintBurnHandle where
  rewindStorage cp h@(MintBurnHandle sqlCon _k) = doRewind >> pure (Just h)
    where
      doRewind = case cp of
        C.ChainPoint slotNo _ ->
          SQL.execute  sqlCon "DELETE FROM minting_policy_events WHERE slotNo > ?" (SQL.Only slotNo)
        C.ChainPointAtGenesis ->
          SQL.execute_ sqlCon "DELETE FROM minting_policy_events"

open :: FilePath -> Int -> IO MintBurnIndexer
open dbPath bufferSize = do
  c <- SQL.open dbPath
  SQL.execute_ c "PRAGMA journal_mode=WAL"
  sqliteInit c
  RI.emptyState bufferSize (MintBurnHandle c bufferSize)
