{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

-- | Module for indexing the stakepool delegation per epoch in the Cardano blockchain.
--
-- This module will create the SQL tables:
--
-- + table: epoch_spd
--
-- @
--    |---------+--------+----------+--------+-----------------+---------|
--    | epochNo | poolId | lovelace | slotNo | blockHeaderHash | blockNo |
--    |---------+--------+----------+--------+-----------------+---------|
-- @
--
-- To create this table, we need to compute the `LedgerState` from `ouroboros-network` (called
-- `NewEpochState` in `cardano-ledger`) at each 'Rollforward' chain sync event. Using the
-- 'LegderState', we can easily compute the epochNo as well as the stake pool delegation for that
-- epoch.
--
-- The main issue with this indexer is that building the LedgerState and saving it on disk for being
-- able to resume is VERY resource intensive. Syncing time for this indexer is over 20h and uses
-- about ~16GB of RAM (which will keep increasing as the blockchain continues to grow).
--
-- Here is a synopsis of what this indexer does.
--
-- We assume that the construction of 'LedgerState' is done outside of this indexer (this module).
--
--   * the 'Storable.insert' function is called with the *last* event of an epoch (therefore, the
--   last 'LedgerState' before starting a new epoch). We do that because we only care about the SPD
--   (Stake Pool Delegation) from the last block before a new epoch.
--
-- Once the 'Storable.StorableEvent' is stored on disk, we perform various steps:
--
--   1. we save the SPD for the current epoch in the `epoch_spd` table
--   2. we save the 'LedgerState's in the filesystem as binary files (the ledger state file path has
--   the format: `ledgerState_<SLOT_NO>_<BLOCK_HEADER_HASH>_<BLOCK_NO>.bin`). We only store a
--   'LedgerState' if it's rollbackable or if the last one of a given epoch. This step is necessary
--   for resuming the indexer.
--   3. we delete immutable 'LedgerState' binary files expect latest one (this step is necessary for
--
-- The indexer provides the following queries:
--
--   * C.EpochNo -> SPD (the actualy query that clients will be interested in)
--   * C.ChainPoint -> LedgerState (query that is necessary for resuming)
module Marconi.ChainIndex.Indexers.EpochStakepoolSize
  ( -- * EpochSPDIndex
    EpochSPDIndex
  , EpochSPDDepth (..)
  , EpochSPDHandle
  , EpochSPDRow (..)
  , StorableEvent(..)
  , StorableQuery(..)
  , StorableResult(..)
  , toStorableEvent
  , open
  , getEpochNo
  ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Coin qualified as Ledger
import Cardano.Ledger.Compactible qualified as Ledger
import Cardano.Ledger.Era qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Slotting.Slot (EpochNo)
import Codec.CBOR.Read qualified as CBOR
import Codec.CBOR.Write qualified as CBOR
import Control.Monad (filterM, forM_, when)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (Object), object, (.:), (.=))
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy qualified as BS
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy))
import Data.Foldable (toList)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (Down (Down))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Tuple (swap)
import Data.VMap qualified as VMap
import Data.Word (Word64)
import Database.SQLite.Simple qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans (decodeLedgerState, encodeLedgerState)
import Marconi.ChainIndex.Utils (isBlockRollbackable)
import Marconi.Core.Storable (Buffered (persistToStorage), HasPoint (getPoint), QueryInterval, Queryable (queryStorage),
                              Resumable, Rewindable (rewindStorage), State, StorableEvent, StorableMonad, StorablePoint,
                              StorableQuery, StorableResult, emptyState)
import Marconi.Core.Storable qualified as Storable
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import System.Directory (listDirectory, removeFile)
import System.FilePath (dropExtension, (</>))
import Text.RawString.QQ (r)
import Text.Read (readMaybe)

data EpochSPDHandle = EpochSPDHandle
    { _epochSDPHandleConnection         :: !SQL.Connection
    , _epochSDPHandleLedgerStateDirPath :: !FilePath
    , _epochSDPHandleSecurityParam      :: !Word64
    }

type instance StorableMonad EpochSPDHandle = IO

data instance StorableEvent EpochSPDHandle =
    EpochSPDEvent
        { epochSPDEventLedgerState :: Maybe (O.LedgerState (O.CardanoBlock O.StandardCrypto))
        , epochSPDEventEpochNo :: Maybe C.EpochNo
        , epochSPDEventSPD :: Map C.PoolId C.Lovelace
        , epochSPDEventSlotNo :: C.SlotNo
        , epochSPDEventBlockHeaderHash :: C.Hash C.BlockHeader
        , epochSPDEventBlockNo :: C.BlockNo
        , epochSPDEventChainTip :: C.ChainTip -- ^ Actual tip of the chain
        , epochSPDEventIsFirstEventOfEpoch :: Bool
        }
    deriving (Eq, Show)

type instance StorablePoint EpochSPDHandle = C.ChainPoint

instance HasPoint (StorableEvent EpochSPDHandle) C.ChainPoint where
  getPoint (EpochSPDEvent _ _ _ s bhh _ _ _) = C.ChainPoint s bhh

data instance StorableQuery EpochSPDHandle =
    SPDByEpochNoQuery C.EpochNo
  | LedgerStateAtPointQuery C.ChainPoint

data instance StorableResult EpochSPDHandle =
    SPDByEpochNoResult [EpochSPDRow]
  | LedgerStateAtPointResult (Maybe (O.LedgerState (O.CardanoBlock O.StandardCrypto)))
    deriving (Eq, Show)

newtype EpochSPDDepth = EpochSPDDepth Int

type EpochSPDIndex = State EpochSPDHandle

toStorableEvent
    :: O.LedgerState (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
    -> C.SlotNo
    -> C.Hash C.BlockHeader
    -> C.BlockNo
    -> C.ChainTip
    -> Word64 -- ^ Security param
    -> Bool -- ^ Is the last event of the current epoch
    -> StorableEvent EpochSPDHandle
toStorableEvent ledgerState slotNo bhh bn chainTip securityParam isFirstEventOfEpoch = do
    let doesStoreLedgerState = isBlockRollbackable securityParam bn chainTip || isFirstEventOfEpoch
    EpochSPDEvent
        (if doesStoreLedgerState then Just ledgerState else Nothing)
        (getEpochNo ledgerState)
        (getStakeMap ledgerState)
        slotNo
        bhh
        bn
        chainTip
        isFirstEventOfEpoch

-- | From LedgerState, get epoch stake pool delegation: a mapping of pool ID to amount staked in
-- lovelace. We do this by getting the '_pstakeSet' stake snapshot and then use '_delegations' and
-- '_stake' to resolve it into the desired mapping.
getStakeMap
    :: O.LedgerState (O.CardanoBlock O.StandardCrypto)
    -> Map C.PoolId C.Lovelace
getStakeMap ledgerState' = case ledgerState' of
  O.LedgerStateByron _    -> mempty
  O.LedgerStateShelley st -> getStakeMapFromShelleyBlock st
  O.LedgerStateAllegra st -> getStakeMapFromShelleyBlock st
  O.LedgerStateMary st    -> getStakeMapFromShelleyBlock st
  O.LedgerStateAlonzo st  -> getStakeMapFromShelleyBlock st
  O.LedgerStateBabbage st -> getStakeMapFromShelleyBlock st
  where
    getStakeMapFromShelleyBlock
      :: forall proto era c
       . (c ~ Ledger.Crypto era, c ~ O.StandardCrypto)
      => O.LedgerState (O.ShelleyBlock proto era)
      -> Map C.PoolId C.Lovelace
    getStakeMapFromShelleyBlock st = spd
      where
        nes = O.shelleyLedgerState st :: Ledger.NewEpochState era

        stakeSnapshot = Ledger._pstakeSet . Ledger.esSnapshots . Ledger.nesEs $ nes :: Ledger.SnapShot c

        stakes = Ledger.unStake
               $ Ledger._stake stakeSnapshot

        delegations :: VMap.VMap VMap.VB VMap.VB (Ledger.Credential 'Ledger.Staking c) (Ledger.KeyHash 'Ledger.StakePool c)
        delegations = Ledger._delegations stakeSnapshot

        spd :: Map C.PoolId C.Lovelace
        spd = Map.fromListWith (+)
            $ map swap
            $ catMaybes
            $ VMap.elems
            $ VMap.mapWithKey
                (\cred spkHash ->
                    (\c -> ( C.Lovelace $ coerce $ Ledger.fromCompact c
                           , C.StakePoolKeyHash spkHash
                           )
                    )
                    <$> VMap.lookup cred stakes)
                delegations

getEpochNo :: O.LedgerState (O.CardanoBlock O.StandardCrypto) -> Maybe EpochNo
getEpochNo ledgerState' = case ledgerState' of
  O.LedgerStateByron _st  -> Nothing
  O.LedgerStateShelley st -> getEpochNoFromShelleyBlock st
  O.LedgerStateAllegra st -> getEpochNoFromShelleyBlock st
  O.LedgerStateMary st    -> getEpochNoFromShelleyBlock st
  O.LedgerStateAlonzo st  -> getEpochNoFromShelleyBlock st
  O.LedgerStateBabbage st -> getEpochNoFromShelleyBlock st
  where
    getEpochNoFromShelleyBlock = Just . Ledger.nesEL . O.shelleyLedgerState

data EpochSPDRow = EpochSPDRow
    { epochSPDRowEpochNo         :: !C.EpochNo
    , epochSPDRowPoolId          :: !C.PoolId
    , epochSPDRowLovelace        :: !C.Lovelace
    , epochSPDRowSlotNo          :: !C.SlotNo
    , epochSPDRowBlockHeaderHash :: !(C.Hash C.BlockHeader)
    , epochSPDRowBlockNo         :: !C.BlockNo
    } deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

instance FromJSON EpochSPDRow where
    parseJSON (Object v) =
        EpochSPDRow
            <$> (C.EpochNo <$> v .: "epochNo")
            <*> v .: "poolId"
            <*> v .: "lovelace"
            <*> (C.SlotNo <$> v .: "slotNo")
            <*> v .: "blockHeaderHash"
            <*> (C.BlockNo <$> v .: "blockNo")
    parseJSON _ = mempty

instance ToJSON EpochSPDRow where
  toJSON (EpochSPDRow (C.EpochNo epochNo)
                      poolId
                      lovelace
                      (C.SlotNo slotNo)
                      blockHeaderHash
                      (C.BlockNo blockNo)) =
      object
        [ "epochNo" .= epochNo
        , "poolId" .= poolId
        , "lovelace" .= lovelace
        , "slotNo" .= slotNo
        , "blockHeaderHash" .= blockHeaderHash
        , "blockNo" .= blockNo
        ]

instance Buffered EpochSPDHandle where
    -- We should only store on disk SPD from the last slot of each epoch.
    persistToStorage
        :: Foldable f
        => f (StorableEvent EpochSPDHandle)
        -> EpochSPDHandle
        -> IO EpochSPDHandle
    persistToStorage events h@(EpochSPDHandle c ledgerStateDirPath securityParam) = do
        let eventsList = toList events

        SQL.execute_ c "BEGIN"
        forM_ (concatMap eventToEpochSPDRows $ filter epochSPDEventIsFirstEventOfEpoch eventsList) $ \row ->
          SQL.execute c
              [r|INSERT INTO epoch_spd
                  ( epochNo
                  , poolId
                  , lovelace
                  , slotNo
                  , blockHeaderHash
                  , blockNo
                  ) VALUES (?, ?, ?, ?, ?, ?)|] row
        SQL.execute_ c "COMMIT"

        -- We store the LedgerState if one of following conditions hold:
        --   * the LedgerState cannot be rollbacked and is the last of an epoch
        --   * the LedgerState can be rollbacked
        let writeLedgerState ledgerState (C.SlotNo slotNo) blockHeaderHash (C.BlockNo blockNo) isRollbackable = do
                let fname = ledgerStateDirPath
                        </> "ledgerState_"
                         <> (if isRollbackable then "volatile_" else "")
                         <> show slotNo
                         <> "_"
                         <> Text.unpack (C.serialiseToRawBytesHexText blockHeaderHash)
                         <> "_"
                         <> show blockNo
                         <> ".bin"
                -- TODO We should delete the file is the write operation was interrumpted by the
                -- user. Tried using something like `onException`, but it doesn't run the cleanup
                -- function. Not sure how to do the cleanup here without restoring doing it outside
                -- the thread where this indexer is running.
                BS.writeFile fname (CBOR.toLazyByteString $ encodeLedgerState ledgerState)
        forM_ eventsList
              $ \(EpochSPDEvent
                    maybeLedgerState
                    maybeEpochNo
                    _
                    slotNo
                    blockHeaderHash
                    blockNo
                    chainTip
                    isFirstEventOfEpoch) -> do
            case (maybeEpochNo, maybeLedgerState) of
              (Just _, Just ledgerState) -> do
                  let isRollbackable = isBlockRollbackable securityParam blockNo chainTip
                  when (isRollbackable || isFirstEventOfEpoch) $ do
                    writeLedgerState ledgerState slotNo blockHeaderHash blockNo isRollbackable
              -- We don't store any 'LedgerState' if the era doesn't have epochs (Byron era) or if
              -- we don't have access to the 'LedgerState'.
              _noLedgerStateOrEpochNo -> pure ()

        -- Remove all immutable LedgerStates from the filesystem expect the most recent immutable
        -- one which is from the first slot of latest epoch.
        -- A 'LedgerState' is considered immutable if its 'blockNo' is '< latestBlockNo - securityParam'.
        case NE.nonEmpty eventsList of
          Nothing -> pure ()
          Just nonEmptyEvents -> do
              let chainTip =
                      NE.head
                      $ NE.sortWith (\case C.ChainTipAtGenesis -> Down Nothing;
                                           C.ChainTip _ _ bn   -> Down (Just bn)
                                    )
                      $ fmap epochSPDEventChainTip nonEmptyEvents

              ledgerStateFilePaths <-
                  mapMaybe (\fp -> fmap (fp,) $ chainTipsFromLedgerStateFilePath fp)
                  <$> listDirectory ledgerStateDirPath

              -- Delete volatile LedgerState which have become immutable.
              let oldVolatileLedgerStateFilePaths =
                      fmap fst
                      $ filter (\(_, (isVolatile, _, _, blockNo)) ->
                          isVolatile && not (isBlockRollbackable securityParam blockNo chainTip))
                      ledgerStateFilePaths
              forM_ oldVolatileLedgerStateFilePaths $ \fp -> removeFile $ ledgerStateDirPath </> fp

              -- Delete all immutable LedgerStates expect the latest one
              let immutableLedgerStateFilePaths =
                      filter (\(_, (isVolatile, _, _, _)) -> not isVolatile) ledgerStateFilePaths
              case NE.nonEmpty immutableLedgerStateFilePaths of
                Nothing -> pure ()
                Just nonEmptyLedgerStateFilePaths -> do
                  let oldImmutableLedgerStateFilePaths =
                          fmap (\(fp, _, _) -> fp)
                          $ filter (\(_, _, isImmutableBlock) -> isImmutableBlock)
                          $ NE.tail
                          $ NE.sortWith (\(_, (_, _, blockNo), isImmutableBlock) ->
                              Down (blockNo, isImmutableBlock))
                          $ fmap (\(fp, (_, slotNo, bhh, blockNo)) ->
                              ( fp
                              , (slotNo, bhh, blockNo)
                              , not $ isBlockRollbackable securityParam blockNo chainTip)
                              )
                          nonEmptyLedgerStateFilePaths
                  forM_ oldImmutableLedgerStateFilePaths
                    $ \fp -> removeFile $ ledgerStateDirPath </> fp

        pure h

    -- | Buffering is not in use in this indexer and we don't need to retrieve stored events in our
    -- implementation. Therefore, this function returns an empty list.
    getStoredEvents
        :: EpochSPDHandle
        -> IO [StorableEvent EpochSPDHandle]
    getStoredEvents EpochSPDHandle {} = do
        pure []

eventToEpochSPDRows
    :: StorableEvent EpochSPDHandle
    -> [EpochSPDRow]
eventToEpochSPDRows (EpochSPDEvent _ maybeEpochNo m slotNo blockHeaderHash blockNo _ _) =
    mapMaybe
        (\(keyHash, lovelace) ->
            fmap (\epochNo -> EpochSPDRow
                                  epochNo
                                  keyHash
                                  lovelace
                                  slotNo
                                  blockHeaderHash
                                  blockNo) maybeEpochNo)
        $ Map.toList m

instance Queryable EpochSPDHandle where
    queryStorage
        :: Foldable f
        => QueryInterval C.ChainPoint
        -> f (StorableEvent EpochSPDHandle)
        -> EpochSPDHandle
        -> StorableQuery EpochSPDHandle
        -> IO (StorableResult EpochSPDHandle)

    queryStorage _ events (EpochSPDHandle c _ _) (SPDByEpochNoQuery epochNo) = do
        case List.find (\e -> epochSPDEventEpochNo e == Just epochNo) (toList events) of
          Just e ->
              pure $ SPDByEpochNoResult $ eventToEpochSPDRows e
          Nothing -> do
              res :: [EpochSPDRow] <- SQL.query c
                  [r|SELECT epochNo, poolId, lovelace, slotNo, blockHeaderHash, blockNo
                     FROM epoch_spd
                     WHERE epochNo = ?
                  |] (SQL.Only epochNo)
              pure $ SPDByEpochNoResult res

    queryStorage _ _ EpochSPDHandle {} (LedgerStateAtPointQuery C.ChainPointAtGenesis) = do
        pure $ LedgerStateAtPointResult Nothing
    queryStorage
            _
            events
            (EpochSPDHandle _ ledgerStateDirPath _)
            (LedgerStateAtPointQuery (C.ChainPoint slotNo _)) = do
        case List.find (\e -> epochSPDEventSlotNo e == slotNo) (toList events) of
            Nothing -> do
                ledgerStateFilePaths <- listDirectory ledgerStateDirPath
                let ledgerStateFilePath =
                        List.find
                            (\fp -> fmap (\(_, sn, _, _) -> sn)
                                         (chainTipsFromLedgerStateFilePath fp) == Just slotNo
                            )
                            ledgerStateFilePaths
                case ledgerStateFilePath of
                  Nothing -> pure $ LedgerStateAtPointResult Nothing
                  Just fp -> do
                      ledgerStateBs <- BS.readFile $ ledgerStateDirPath </> fp
                      let ledgerState =
                              either
                                (const Nothing)
                                (Just . snd)
                                $ CBOR.deserialiseFromBytes decodeLedgerState ledgerStateBs
                      pure $ LedgerStateAtPointResult ledgerState
            Just event -> pure $ LedgerStateAtPointResult $ epochSPDEventLedgerState event

instance Rewindable EpochSPDHandle where
    rewindStorage
        :: C.ChainPoint
        -> EpochSPDHandle
        -> IO (Maybe EpochSPDHandle)
    rewindStorage C.ChainPointAtGenesis h@(EpochSPDHandle c ledgerStateDirPath _) = do
        SQL.execute_ c "DELETE FROM epoch_spd"

        ledgerStateFilePaths <- listDirectory ledgerStateDirPath
        forM_ ledgerStateFilePaths (\f -> removeFile $ ledgerStateDirPath </> f)
        pure $ Just h
    rewindStorage (C.ChainPoint sn _) h@(EpochSPDHandle c ledgerStateDirPath _) = do
        SQL.execute c "DELETE FROM epoch_spd WHERE slotNo > ?" (SQL.Only sn)

        ledgerStateFilePaths <- listDirectory ledgerStateDirPath
        forM_ ledgerStateFilePaths $ \fp -> do
            case chainTipsFromLedgerStateFilePath fp of
              Nothing                              -> pure ()
              Just (_, slotNo, _, _) | slotNo > sn -> removeFile $ ledgerStateDirPath </> fp
              Just _                               -> pure ()

        pure $ Just h

instance Resumable EpochSPDHandle where
    resumeFromStorage
        :: EpochSPDHandle
        -> IO [C.ChainPoint]
    resumeFromStorage (EpochSPDHandle c ledgerStateDirPath _) = do
        ledgerStateFilepaths <- listDirectory ledgerStateDirPath
        let ledgerStateChainPoints =
                fmap (\(_, sn, bhh, _) -> (sn, bhh))
                $ mapMaybe chainTipsFromLedgerStateFilePath ledgerStateFilepaths

        resumablePoints <- flip filterM ledgerStateChainPoints $ \(slotNo, _) -> do
            result :: [[C.SlotNo]] <- SQL.query c
                [r|SELECT slotNo
                   FROM epoch_spd
                   WHERE slotNo = ? LIMIT 1 |] (SQL.Only slotNo)
            pure $ not $ null result

        pure
            $ List.sortOn Down
            $ fmap (uncurry C.ChainPoint) resumablePoints ++ [C.ChainPointAtGenesis]

chainTipsFromLedgerStateFilePath :: FilePath -> Maybe (Bool, C.SlotNo, C.Hash C.BlockHeader, C.BlockNo)
chainTipsFromLedgerStateFilePath ledgerStateFilepath =
    case Text.splitOn "_" (Text.pack $ dropExtension ledgerStateFilepath) of
      [_, slotNoStr, bhhStr, blockNoStr] -> do
          (False,,,)
            <$> parseSlotNo slotNoStr
            <*> parseBlockHeaderHash bhhStr
            <*> parseBlockNo blockNoStr
      [_, "volatile", slotNoStr, bhhStr, blockNoStr] -> do
          (True,,,)
            <$> parseSlotNo slotNoStr
            <*> parseBlockHeaderHash bhhStr
            <*> parseBlockNo blockNoStr
      _anyOtherFailure -> Nothing
 where
     parseSlotNo slotNoStr = C.SlotNo <$> readMaybe (Text.unpack slotNoStr)
     parseBlockHeaderHash bhhStr = do
          bhhBs <- either (const Nothing) Just $ Base16.decode $ Text.encodeUtf8 bhhStr
          C.deserialiseFromRawBytes (C.proxyToAsType Proxy) bhhBs
     parseBlockNo blockNoStr = C.BlockNo <$> readMaybe (Text.unpack blockNoStr)

open
  :: FilePath
  -- ^ SQLite database file path
  -> FilePath
  -- ^ Directory from which we will save the various 'LedgerState' as different points in time.
  -> Word64
  -> IO (State EpochSPDHandle)
open dbPath ledgerStateDirPath securityParam = do
    c <- SQL.open dbPath
    SQL.execute_ c "PRAGMA journal_mode=WAL"
    SQL.execute_ c
        [r|CREATE TABLE IF NOT EXISTS epoch_spd
            ( epochNo INT NOT NULL
            , poolId BLOB NOT NULL
            , lovelace INT NOT NULL
            , slotNo INT NOT NULL
            , blockHeaderHash BLOB NOT NULL
            , blockNo INT NOT NULL
            )|]
    emptyState 1 (EpochSPDHandle c ledgerStateDirPath securityParam)
