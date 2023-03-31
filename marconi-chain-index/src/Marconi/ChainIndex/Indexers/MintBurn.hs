{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Data qualified as LA
import Cardano.Ledger.Alonzo.Scripts qualified as LA
import Cardano.Ledger.Alonzo.Tx qualified as LA
import Cardano.Ledger.Alonzo.TxWitness qualified as LA
import Cardano.Ledger.Babbage.Tx qualified as LB
import Cardano.Ledger.Mary.Value qualified as LM
import Control.Lens (makeLenses, view, (&), (^.))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.=))
import Data.ByteString.Short qualified as Short
import Data.Coerce (coerce)
import Data.Foldable (toList)
import Data.Function (on)
import Data.List (groupBy, sort)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Data.Word (Word64)
import Database.SQLite.Simple (NamedParam ((:=)))
import Database.SQLite.Simple qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Types (SecurityParam)
import Marconi.Core.Storable qualified as RI
import Ouroboros.Consensus.Shelley.Eras qualified as OEra

-- * Event

data TxMintEvent = TxMintEvent
  { txMintEventSlotNo          :: !C.SlotNo
  , txMintEventBlockHeaderHash :: !(C.Hash C.BlockHeader)
  , txMintEventTxAssets        :: !(NE.NonEmpty (C.TxId, NE.NonEmpty MintAsset))
  } deriving (Show, Eq, Ord)

data MintAsset = MintAsset
  { mintAssetPolicyId     :: !C.PolicyId
  , mintAssetAssetName    :: !C.AssetName
  , mintAssetQuantity     :: !C.Quantity
  , mintAssetRedeemerIdx  :: !Word64
  , mintAssetRedeemerData :: !C.ScriptData
  } deriving (Show, Eq, Ord)

toUpdate :: C.BlockInMode C.CardanoMode -> Maybe TxMintEvent
toUpdate (C.BlockInMode (C.Block (C.BlockHeader slotNo blockHeaderHash _blockNo) txs) _) =
  case mapMaybe txMints txs of
    x : xs -> Just $ TxMintEvent slotNo blockHeaderHash (x NE.:| xs)
    []     -> Nothing

txMints :: C.Tx era -> Maybe (C.TxId, NE.NonEmpty MintAsset)
txMints (C.Tx txb _) = case txbMints txb of
  x : xs -> Just (C.getTxId txb, x NE.:| xs )
  []     -> Nothing

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
  _byronTxBody -> [] -- ByronTxBody is not exported but as it's the only other data constructor then _ matches it.

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

getPolicyData
    :: C.TxBody era
    -> LM.Value OEra.StandardCrypto
    -> [(C.PolicyId, C.AssetName, C.Quantity, Word64, C.ScriptData)]
getPolicyData txb (LM.Value _ m) = do
  let
    policyIdList = Map.toList m
    getPolicyId index' = policyIdList !! fromIntegral index'
  ((maryPolicyID, assets), index'', (redeemer, _)) <- map (\(index', data_) -> (getPolicyId index', index', data_)) $ mintRedeemers txb
  (assetName, quantity) :: (LM.AssetName, Integer) <- Map.toList assets
  pure (fromMaryPolicyID maryPolicyID, fromMaryAssetName assetName, C.Quantity quantity, index'', fromAlonzoData redeemer)

-- ** Copy-paste

fromMaryPolicyID :: LM.PolicyID OEra.StandardCrypto -> C.PolicyId
fromMaryPolicyID (LM.PolicyID sh) = C.PolicyId (C.fromShelleyScriptHash sh) -- from cardano-api:src/Cardano/Api/Value.hs

fromMaryAssetName :: LM.AssetName -> C.AssetName
fromMaryAssetName (LM.AssetName n) = C.AssetName $ Short.fromShort n -- from cardano-api:src/Cardano/Api/Value.hs

fromAlonzoData :: LA.Data ledgerera -> C.ScriptData
fromAlonzoData = C.fromPlutusData . LA.getPlutusData -- from cardano-api:src/Cardano/Api/ScriptData.hs

-- * Sqlite

data TxMintRow = TxMintRow
    { _txMintRowSlotNo          :: !C.SlotNo
    , _txMintRowBlockHeaderHash :: !(C.Hash C.BlockHeader)
    , _txMintRowTxId            :: !C.TxId
    , _txMintRowPolicyId        :: !C.PolicyId
    , _txMintRowAssetName       :: !C.AssetName
    , _txMintRowQuantity        :: !C.Quantity
    , _txMintRowRedeemerIdx     :: !Word64
    , _txMintRowRedeemerData    :: !C.ScriptData
    }
    deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

makeLenses 'TxMintRow

instance FromJSON TxMintRow where
    parseJSON (Object v) =
        TxMintRow
            <$> v .: "slotNo"
            <*> v .: "blockHeaderHash"
            <*> v .: "txId"
            <*> v .: "policyId"
            <*> v .: "assetName"
            <*> v .: "quantity"
            <*> v .: "redeemerIdx"
            <*> v .: "redeemerData"
    parseJSON _ = mempty

instance ToJSON TxMintRow where
  toJSON (TxMintRow slotNo bhh txId policyId assetName qty redIdx redData) = object
    [ "slotNo" .= slotNo
    , "blockHeaderHash" .= bhh
    , "txId" .= txId
    , "policyId" .= policyId
    , "assetName" .= assetName
    , "quantity" .= qty
    , "redeemerIdx" .= redIdx
    , "redeemerData" .= redData
    ]

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

toRows :: TxMintEvent -> [TxMintRow]
toRows e = do
  (txId, txMintAssets) <- NE.toList $ txMintEventTxAssets e
  mintAsset <- NE.toList txMintAssets
  pure $ TxMintRow
    (txMintEventSlotNo e)
    (txMintEventBlockHeaderHash e)
    txId
    (mintAssetPolicyId mintAsset)
    (mintAssetAssetName mintAsset)
    (mintAssetQuantity mintAsset)
    (mintAssetRedeemerIdx mintAsset)
    (mintAssetRedeemerData mintAsset)

-- | Input rows must be sorted by C.SlotNo.
fromRows :: [TxMintRow] -> [TxMintEvent]
fromRows rows =  do
  rs@(r :| _) <- NE.groupBy ((==) `on` slotNo) rows -- group by SlotNo
  pure $ TxMintEvent (slotNo r) (hash r) $ do
    rs' <- NE.groupBy1 ((==) `on` txId) rs -- group by TxId
    pure (txId r, rowToMintAsset <$> rs')
  where
    slotNo = view txMintRowSlotNo :: TxMintRow -> C.SlotNo
    hash = view txMintRowBlockHeaderHash :: TxMintRow -> C.Hash C.BlockHeader
    txId = view txMintRowTxId :: TxMintRow -> C.TxId
    rowToMintAsset :: TxMintRow -> MintAsset
    rowToMintAsset row =
        MintAsset
            (row ^. txMintRowPolicyId)
            (row ^. txMintRowAssetName)
            (row ^. txMintRowQuantity)
            (row ^. txMintRowRedeemerIdx)
            (row ^. txMintRowRedeemerData)

queryStoredTxMintEvents
    :: SQL.Connection
    -> ([SQL.Query], [NamedParam])
    -> IO [TxMintEvent]
queryStoredTxMintEvents sqlCon (conditions, params) =
  fmap fromRows $ SQL.queryNamed sqlCon (SQL.Query query) params
  where
    allConditions = Text.intercalate " AND " $ fmap SQL.fromQuery conditions
    whereClause = if allConditions == "" then "" else "WHERE " <> allConditions
    query =
          " SELECT slotNo, blockHeaderHash, txId, policyId       \
          \      , assetName, quantity, redeemerIx, redeemerData \
          \   FROM minting_policy_events                         \
          \   " <> whereClause <> "                              \
          \  ORDER BY slotNo, txId                               "

intervalToWhereClause :: RI.QueryInterval C.ChainPoint -> ([SQL.Query], [NamedParam])
intervalToWhereClause qi = case qi of
  RI.QEverything -> ([], [])
  RI.QInterval from to
    | Just from' <- slotMaybe from, Just to' <- slotMaybe to -> (["slotNo BETWEEN :fromSlot AND :toSlot"] , [":fromSlot" := from', ":toSlot" := to'])
    | Just to' <- slotMaybe to -> (["slotNo <= :toSlot"] , [":toSlot" := to'])
    | otherwise -> (["FALSE"], [])
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
                     []     -> [])

-- * Indexer

data MintBurnHandle = MintBurnHandle
  { sqlConnection :: !SQL.Connection
  , securityParam :: !SecurityParam
  }

type MintBurnIndexer = RI.State MintBurnHandle

type instance RI.StorablePoint MintBurnHandle = C.ChainPoint

type instance RI.StorableMonad MintBurnHandle = IO

newtype instance RI.StorableEvent MintBurnHandle
  = MintBurnEvent TxMintEvent
  deriving (Show)

data instance RI.StorableQuery MintBurnHandle
  = QueryByAssetId C.PolicyId C.AssetName (Maybe C.SlotNo)
  | QueryAllMintBurn (Maybe C.SlotNo)
  deriving (Show)

newtype instance RI.StorableResult MintBurnHandle
  = MintBurnResult [TxMintRow]
  deriving (Show)

instance RI.Queryable MintBurnHandle where
  queryStorage queryInterval memoryEvents (MintBurnHandle sqlCon _k) query =
      case query of
        QueryAllMintBurn Nothing -> toResult Nothing <$> queryStoredTxMintEvents sqlCon interval
        QueryAllMintBurn (Just slotNo) -> do
            let conditions = interval <> (["slotNo <= :slotNo"], [":slotNo" := slotNo])
            toResult (Just slotNo) <$> queryStoredTxMintEvents sqlCon conditions
        QueryByAssetId policyId assetName Nothing -> do
            let conditions = interval
                          <> ( ["policyId = :policyId AND assetName = :assetName"]
                             , [":policyId" := policyId, ":assetName" := assetName]
                             )
            toResult Nothing <$> queryStoredTxMintEvents sqlCon conditions
        QueryByAssetId policyId assetName (Just slotNo) -> do
            let conditions = interval
                          <> ( ["slotNo <= :slotNo AND policyId = :policyId AND assetName = :assetName"]
                             , [":slotNo" := slotNo, ":policyId" := policyId, ":assetName" := assetName]
                             )
            toResult (Just slotNo) <$> queryStoredTxMintEvents sqlCon conditions
        where
          toResult querySlotNo storedEvents = MintBurnResult $ do
            let memoryEventsList = fmap coerce $ toList memoryEvents
            let filteredMemoryEvents =
                    case querySlotNo of
                      Nothing -> memoryEventsList
                      Just sn -> filter (\e -> txMintEventSlotNo e <= sn) memoryEventsList
            TxMintEvent slotNo blockHeaderHash txAssets <- filteredMemoryEvents <> storedEvents
            (txId, mintAssets) <- NE.toList txAssets
            MintAsset policyId assetName quantity redeemerIx redeemerData <- NE.toList mintAssets
            pure $
                TxMintRow
                    slotNo
                    blockHeaderHash
                    txId
                    policyId
                    assetName
                    quantity
                    redeemerIx
                    redeemerData

          interval = intervalToWhereClause queryInterval

instance RI.HasPoint (RI.StorableEvent MintBurnHandle) C.ChainPoint where
  getPoint (MintBurnEvent e) = C.ChainPoint (txMintEventSlotNo e) (txMintEventBlockHeaderHash e)

instance RI.Buffered MintBurnHandle where
  persistToStorage events h@(MintBurnHandle sqlCon _k) = do
    sqliteInsert sqlCon (map coerce $ toList events)
    pure h

  getStoredEvents (MintBurnHandle sqlCon k) = do
    fmap MintBurnEvent . fromRows <$> SQL.query sqlCon query (SQL.Only k)
    where
      query =
        " SELECT slotNo, blockHeaderHash, txId, policyId, assetName, quantity, \
        \        redeemerIx, redeemerData                                      \
        \   FROM minting_policy_events                                         \
        \  WHERE slotNo >= (SELECT MAX(slotNo) - ? FROM minting_policy_events) \
        \  ORDER BY slotNo DESC, txId                                          "

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

open :: FilePath -> SecurityParam -> IO MintBurnIndexer
open dbPath bufferSize = do
  c <- SQL.open dbPath
  SQL.execute_ c "PRAGMA journal_mode=WAL"
  sqliteInit c
  RI.emptyState (fromEnum bufferSize) (MintBurnHandle c bufferSize)
