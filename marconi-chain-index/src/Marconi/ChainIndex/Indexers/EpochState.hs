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
-- + table: epoch_sdd
--
-- @
--    |---------+--------+----------+--------+-----------------+---------|
--    | epochNo | poolId | lovelace | slotNo | blockHeaderHash | blockNo |
--    |---------+--------+----------+--------+-----------------+---------|
-- @
--
-- + table: epoch_nonce
--
-- @
--    |---------+-------+--------+-----------------+---------|
--    | epochNo | nonce | slotNo | blockHeaderHash | blockNo |
--    |---------+-------+--------+-----------------+---------|
-- @
--
-- To create those tables, we need to compute the `ExtLedgerState` from `ouroboros-network`
-- (combination of `LedgerState, called `NewEpochState` in `cardano-ledger`, and the `HeaderState`)
-- at each 'Rollforward' chain sync event. Using the `ExtLegderState`, we can easily compute the
-- epoch nonce as well as the stake pool delegation for that epoch.
--
-- The main issue with this indexer is that building the ExtLedgerState and saving it on disk for being
-- able to resume is VERY resource intensive. Syncing time for this indexer is over 20h and uses
-- about ~16GB of RAM (which will keep increasing as the blockchain continues to grow).
--
-- Here is a synopsis of what this indexer does.
--
-- We assume that the construction of 'LedgerState' is done outside of this indexer (this module).
--
--   * the 'Storable.insert' function is called with the *last* event of an epoch (therefore, the
--   last 'LedgerState' before starting a new epoch). We do that because we only care about the SDD
--   (Stake Pool Delegation) from the last block before a new epoch.
--
-- Once the 'Storable.StorableEvent' is stored on disk, we perform various steps:
--
--   1. we save the SDD for the current epoch in the `epoch_sdd` table
--   2. we save the 'LedgerState's in the filesystem as binary files (the ledger state file path has
--   the format: `ledgerState_<SLOT_NO>_<BLOCK_HEADER_HASH>_<BLOCK_NO>.bin`). We only store a
--   'LedgerState' if it's rollbackable or if the last one of a given epoch. This step is necessary
--   for resuming the indexer.
--   3. we delete immutable 'LedgerState' binary files expect latest one (this step is necessary for
--
-- The indexer provides the following queries:
--
--   * C.EpochNo -> SDD (the actualy query that clients will be interested in)
--   * C.ChainPoint -> LedgerState (query that is necessary for resuming)
module Marconi.ChainIndex.Indexers.EpochState
  ( -- * EpochStateIndex
    EpochStateIndex
  , EpochStateDepth (..)
  , EpochStateHandle
  , EpochSDDRow (..)
  , EpochNonceRow (..)
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
import Cardano.Protocol.TPraos.API qualified as Shelley
import Cardano.Protocol.TPraos.Rules.Tickn qualified as Shelley
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
import Data.Maybe (catMaybes, listToMaybe, mapMaybe)
import Data.Ord (Down (Down))
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Tuple (swap)
import Data.VMap qualified as VMap
import Data.Word (Word64)
import Database.SQLite.Simple qualified as SQL
import GHC.Generics (Generic)
import Marconi.ChainIndex.Orphans ()
import Marconi.ChainIndex.Utils (isBlockRollbackable)
import Marconi.Core.Storable (Buffered (persistToStorage), HasPoint (getPoint), QueryInterval, Queryable (queryStorage),
                              Resumable, Rewindable (rewindStorage), State, StorableEvent, StorableMonad, StorablePoint,
                              StorableQuery, StorableResult, emptyState)
import Marconi.Core.Storable qualified as Storable
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Config qualified as O
import Ouroboros.Consensus.HeaderValidation qualified as O
import Ouroboros.Consensus.Ledger.Extended qualified as O
import Ouroboros.Consensus.Protocol.Praos qualified as O
import Ouroboros.Consensus.Protocol.TPraos qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O
import Ouroboros.Consensus.Storage.Serialisation qualified as O
import System.Directory (listDirectory, removeFile)
import System.FilePath (dropExtension, (</>))
import Text.RawString.QQ (r)
import Text.Read (readMaybe)

data EpochStateHandle = EpochStateHandle
    { _epochStateHandleTopLevelCfg        :: !(O.TopLevelConfig (O.CardanoBlock O.StandardCrypto))
    , _epochStateHandleConnection         :: !SQL.Connection
    , _epochStateHandleLedgerStateDirPath :: !FilePath
    , _epochStateHandleSecurityParam      :: !Word64
    }

type instance StorableMonad EpochStateHandle = IO

data instance StorableEvent EpochStateHandle =
    EpochStateEvent
        { epochStateEventLedgerState :: Maybe (O.ExtLedgerState (O.HardForkBlock (O.CardanoEras O.StandardCrypto)))
        , epochStateEventEpochNo :: Maybe C.EpochNo
        , epochStateEventNonce :: Ledger.Nonce
        , epochStateEventSDD :: Map C.PoolId C.Lovelace
        , epochStateEventSlotNo :: C.SlotNo
        , epochStateEventBlockHeaderHash :: C.Hash C.BlockHeader
        , epochStateEventBlockNo :: C.BlockNo
        , epochStateEventChainTip :: C.ChainTip -- ^ Actual tip of the chain
        , epochStateEventIsFirstEventOfEpoch :: Bool
        }
    deriving (Eq, Show)

type instance StorablePoint EpochStateHandle = C.ChainPoint

instance HasPoint (StorableEvent EpochStateHandle) C.ChainPoint where
  getPoint (EpochStateEvent _ _ _ _ s bhh _ _ _) = C.ChainPoint s bhh

data instance StorableQuery EpochStateHandle =
    SDDByEpochNoQuery C.EpochNo
  | NonceByEpochNoQuery C.EpochNo
  | LedgerStateAtPointQuery C.ChainPoint

data instance StorableResult EpochStateHandle =
    SDDByEpochNoResult [EpochSDDRow]
  | NonceByEpochNoResult (Maybe EpochNonceRow)
  | LedgerStateAtPointResult (Maybe (O.ExtLedgerState (O.CardanoBlock O.StandardCrypto)))
    deriving (Eq, Show)

newtype EpochStateDepth = EpochStateDepth Int

type EpochStateIndex = State EpochStateHandle

toStorableEvent
    :: O.ExtLedgerState (O.HardForkBlock (O.CardanoEras O.StandardCrypto))
    -> C.SlotNo
    -> C.Hash C.BlockHeader
    -> C.BlockNo
    -> C.ChainTip
    -> Word64 -- ^ Security param
    -> Bool -- ^ Is the last event of the current epoch
    -> StorableEvent EpochStateHandle
toStorableEvent extLedgerState slotNo bhh bn chainTip securityParam isFirstEventOfEpoch = do
    let doesStoreLedgerState = isBlockRollbackable securityParam bn chainTip || isFirstEventOfEpoch
    EpochStateEvent
        (if doesStoreLedgerState then Just extLedgerState else Nothing)
        (getEpochNo extLedgerState)
        (getEpochNonce extLedgerState)
        (getStakeMap extLedgerState)
        slotNo
        bhh
        bn
        chainTip
        isFirstEventOfEpoch

-- | From LedgerState, get epoch stake pool delegation: a mapping of pool ID to amount staked in
-- lovelace. We do this by getting the '_pstakeSet' stake snapshot and then use '_delegations' and
-- '_stake' to resolve it into the desired mapping.
getStakeMap
    :: O.ExtLedgerState (O.CardanoBlock O.StandardCrypto)
    -> Map C.PoolId C.Lovelace
getStakeMap extLedgerState = case O.ledgerState extLedgerState of
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
    getStakeMapFromShelleyBlock st = sdd
      where
        nes = O.shelleyLedgerState st :: Ledger.NewEpochState era

        stakeSnapshot = Ledger._pstakeSet . Ledger.esSnapshots . Ledger.nesEs $ nes :: Ledger.SnapShot c

        stakes = Ledger.unStake
               $ Ledger._stake stakeSnapshot

        delegations :: VMap.VMap VMap.VB VMap.VB (Ledger.Credential 'Ledger.Staking c) (Ledger.KeyHash 'Ledger.StakePool c)
        delegations = Ledger._delegations stakeSnapshot

        sdd :: Map C.PoolId C.Lovelace
        sdd = Map.fromListWith (+)
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

getEpochNo
    :: O.ExtLedgerState (O.CardanoBlock O.StandardCrypto)
    -> Maybe EpochNo
getEpochNo extLedgerState = case O.ledgerState extLedgerState of
  O.LedgerStateByron _st  -> Nothing
  O.LedgerStateShelley st -> getEpochNoFromShelleyBlock st
  O.LedgerStateAllegra st -> getEpochNoFromShelleyBlock st
  O.LedgerStateMary st    -> getEpochNoFromShelleyBlock st
  O.LedgerStateAlonzo st  -> getEpochNoFromShelleyBlock st
  O.LedgerStateBabbage st -> getEpochNoFromShelleyBlock st
  where
    getEpochNoFromShelleyBlock = Just . Ledger.nesEL . O.shelleyLedgerState

data EpochSDDRow = EpochSDDRow
    { epochSDDRowEpochNo         :: !C.EpochNo
    , epochSDDRowPoolId          :: !C.PoolId
    , epochSDDRowLovelace        :: !C.Lovelace
    , epochSDDRowSlotNo          :: !C.SlotNo
    , epochSDDRowBlockHeaderHash :: !(C.Hash C.BlockHeader)
    , epochSDDRowBlockNo         :: !C.BlockNo
    } deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

instance FromJSON EpochSDDRow where
    parseJSON (Object v) =
        EpochSDDRow
            <$> (C.EpochNo <$> v .: "epochNo")
            <*> v .: "poolId"
            <*> v .: "lovelace"
            <*> (C.SlotNo <$> v .: "slotNo")
            <*> v .: "blockHeaderHash"
            <*> (C.BlockNo <$> v .: "blockNo")
    parseJSON _ = mempty

instance ToJSON EpochSDDRow where
  toJSON (EpochSDDRow (C.EpochNo epochNo)
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

-- | Get Nonce per epoch given an extended ledger state. The Nonce is only available starting at
-- Shelley era. Byron era has the neutral nonce.
getEpochNonce :: O.ExtLedgerState (O.CardanoBlock O.StandardCrypto) -> Ledger.Nonce
getEpochNonce extLedgerState =
    case O.headerStateChainDep (O.headerState extLedgerState) of
      O.ChainDepStateByron _    -> Ledger.NeutralNonce
      O.ChainDepStateShelley st -> extractNonce st
      O.ChainDepStateAllegra st -> extractNonce st
      O.ChainDepStateMary st    -> extractNonce st
      O.ChainDepStateAlonzo st  -> extractNonce st
      O.ChainDepStateBabbage st -> extractNoncePraos st
  where
    extractNonce :: O.TPraosState c -> Ledger.Nonce
    extractNonce =
      Shelley.ticknStateEpochNonce . Shelley.csTickn . O.tpraosStateChainDepState

    extractNoncePraos :: O.PraosState c -> Ledger.Nonce
    extractNoncePraos = O.praosStateEpochNonce

data EpochNonceRow = EpochNonceRow
    { epochNonceRowEpochNo         :: !C.EpochNo
    , epochNonceRowNonce           :: !Ledger.Nonce
    , epochNonceRowSlotNo          :: !C.SlotNo
    , epochNonceRowBlockHeaderHash :: !(C.Hash C.BlockHeader)
    , epochNonceRowBlockNo         :: !C.BlockNo
    } deriving (Eq, Ord, Show, Generic, SQL.FromRow, SQL.ToRow)

instance FromJSON EpochNonceRow where
    parseJSON (Object v) =
        EpochNonceRow
            <$> (C.EpochNo <$> v .: "epochNo")
            <*> (Ledger.Nonce <$> v .: "nonce")
            <*> (C.SlotNo <$> v .: "slotNo")
            <*> v .: "blockHeaderHash"
            <*> (C.BlockNo <$> v .: "blockNo")
    parseJSON _ = mempty

instance ToJSON EpochNonceRow where
  toJSON (EpochNonceRow (C.EpochNo epochNo)
                        nonce
                        (C.SlotNo slotNo)
                        blockHeaderHash
                        (C.BlockNo blockNo)) =
      let nonceValue = case nonce of Ledger.NeutralNonce -> Nothing
                                     Ledger.Nonce n      -> Just n
       in object
        [ "epochNo" .= epochNo
        , "nonce" .= nonceValue
        , "slotNo" .= slotNo
        , "blockHeaderHash" .= blockHeaderHash
        , "blockNo" .= blockNo
        ]

instance Buffered EpochStateHandle where
    -- We should only store on disk SDD from the last slot of each epoch.
    persistToStorage
        :: Foldable f
        => f (StorableEvent EpochStateHandle)
        -> EpochStateHandle
        -> IO EpochStateHandle
    persistToStorage events h@(EpochStateHandle topLevelConfig c ledgerStateDirPath securityParam) = do
        let eventsList = toList events

        SQL.execute_ c "BEGIN"
        forM_ (concatMap eventToEpochSDDRows $ filter epochStateEventIsFirstEventOfEpoch eventsList) $ \row ->
          SQL.execute c
              [r|INSERT INTO epoch_sdd
                  ( epochNo
                  , poolId
                  , lovelace
                  , slotNo
                  , blockHeaderHash
                  , blockNo
                  ) VALUES (?, ?, ?, ?, ?, ?)|] row
        SQL.execute_ c "COMMIT"

        SQL.execute_ c "BEGIN"
        forM_ (mapMaybe eventToEpochNonceRow $ filter epochStateEventIsFirstEventOfEpoch eventsList) $ \row ->
          SQL.execute c
              [r|INSERT INTO epoch_nonce
                  ( epochNo
                  , nonce
                  , slotNo
                  , blockHeaderHash
                  , blockNo
                  ) VALUES (?, ?, ?, ?, ?)|] row
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
                let codecConfig = O.configCodec topLevelConfig
                BS.writeFile fname
                    $ CBOR.toLazyByteString
                    $ O.encodeExtLedgerState
                        (O.encodeDisk codecConfig)
                        (O.encodeDisk codecConfig)
                        (O.encodeDisk codecConfig)
                        ledgerState
        forM_ eventsList
              $ \(EpochStateEvent
                    maybeLedgerState
                    maybeEpochNo
                    _
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
                      $ fmap epochStateEventChainTip nonEmptyEvents

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
        :: EpochStateHandle
        -> IO [StorableEvent EpochStateHandle]
    getStoredEvents EpochStateHandle {} = do
        pure []

eventToEpochSDDRows
    :: StorableEvent EpochStateHandle
    -> [EpochSDDRow]
eventToEpochSDDRows (EpochStateEvent _ maybeEpochNo _ m slotNo blockHeaderHash blockNo _ _) =
    mapMaybe
        (\(keyHash, lovelace) ->
            fmap (\epochNo -> EpochSDDRow
                                  epochNo
                                  keyHash
                                  lovelace
                                  slotNo
                                  blockHeaderHash
                                  blockNo) maybeEpochNo)
        $ Map.toList m

eventToEpochNonceRow
    :: StorableEvent EpochStateHandle
    -> Maybe EpochNonceRow
eventToEpochNonceRow (EpochStateEvent _ maybeEpochNo nonce _ slotNo blockHeaderHash blockNo _ _) =
    fmap (\epochNo -> EpochNonceRow
                          epochNo
                          nonce
                          slotNo
                          blockHeaderHash
                          blockNo) maybeEpochNo

instance Queryable EpochStateHandle where
    queryStorage
        :: Foldable f
        => QueryInterval C.ChainPoint
        -> f (StorableEvent EpochStateHandle)
        -> EpochStateHandle
        -> StorableQuery EpochStateHandle
        -> IO (StorableResult EpochStateHandle)

    queryStorage _ events (EpochStateHandle _ c _ _) (SDDByEpochNoQuery epochNo) = do
        case List.find (\e -> epochStateEventEpochNo e == Just epochNo) (toList events) of
          Just e ->
              pure $ SDDByEpochNoResult $ eventToEpochSDDRows e
          Nothing -> do
              res :: [EpochSDDRow] <- SQL.query c
                  [r|SELECT epochNo, poolId, lovelace, slotNo, blockHeaderHash, blockNo
                     FROM epoch_sdd
                     WHERE epochNo = ?
                  |] (SQL.Only epochNo)
              pure $ SDDByEpochNoResult res

    queryStorage _ events (EpochStateHandle _ c _ _) (NonceByEpochNoQuery epochNo) = do
        case List.find (\e -> epochStateEventEpochNo e == Just epochNo) (toList events) of
          Just e ->
              pure $ NonceByEpochNoResult $ eventToEpochNonceRow e
          Nothing -> do
              res :: [EpochNonceRow] <- SQL.query c
                  [r|SELECT epochNo, nonce, slotNo, blockHeaderHash, blockNo
                     FROM epoch_nonce
                     WHERE epochNo = ?
                  |] (SQL.Only epochNo)
              pure $ NonceByEpochNoResult $ listToMaybe res

    queryStorage _ _ EpochStateHandle {} (LedgerStateAtPointQuery C.ChainPointAtGenesis) = do
        pure $ LedgerStateAtPointResult Nothing
    queryStorage
            _
            events
            (EpochStateHandle topLevelConfig _ ledgerStateDirPath _)
            (LedgerStateAtPointQuery (C.ChainPoint slotNo _)) = do
        case List.find (\e -> epochStateEventSlotNo e == slotNo) (toList events) of
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
                      let codecConfig = O.configCodec topLevelConfig
                          ledgerState =
                              either
                                (const Nothing)
                                (Just . snd)
                                $ CBOR.deserialiseFromBytes
                                    ( O.decodeExtLedgerState (O.decodeDisk codecConfig)
                                                             (O.decodeDisk codecConfig)
                                                             (O.decodeDisk codecConfig)
                                    ) ledgerStateBs
                      pure $ LedgerStateAtPointResult ledgerState
            Just event -> pure $ LedgerStateAtPointResult $ epochStateEventLedgerState event

instance Rewindable EpochStateHandle where
    rewindStorage
        :: C.ChainPoint
        -> EpochStateHandle
        -> IO (Maybe EpochStateHandle)
    rewindStorage C.ChainPointAtGenesis h@(EpochStateHandle _ c ledgerStateDirPath _) = do
        SQL.execute_ c "DELETE FROM epoch_sdd"
        SQL.execute_ c "DELETE FROM epoch_nonce"

        ledgerStateFilePaths <- listDirectory ledgerStateDirPath
        forM_ ledgerStateFilePaths (\f -> removeFile $ ledgerStateDirPath </> f)
        pure $ Just h
    rewindStorage (C.ChainPoint sn _) h@(EpochStateHandle _ c ledgerStateDirPath _) = do
        SQL.execute c "DELETE FROM epoch_sdd WHERE slotNo > ?" (SQL.Only sn)
        SQL.execute c "DELETE FROM epoch_nonce WHERE slotNo > ?" (SQL.Only sn)

        ledgerStateFilePaths <- listDirectory ledgerStateDirPath
        forM_ ledgerStateFilePaths $ \fp -> do
            case chainTipsFromLedgerStateFilePath fp of
              Nothing                              -> pure ()
              Just (_, slotNo, _, _) | slotNo > sn -> removeFile $ ledgerStateDirPath </> fp
              Just _                               -> pure ()

        pure $ Just h

instance Resumable EpochStateHandle where
    resumeFromStorage
        :: EpochStateHandle
        -> IO [C.ChainPoint]
    resumeFromStorage (EpochStateHandle _ c ledgerStateDirPath _) = do
        ledgerStateFilepaths <- listDirectory ledgerStateDirPath
        let ledgerStateChainPoints =
                fmap (\(_, sn, bhh, _) -> (sn, bhh))
                $ mapMaybe chainTipsFromLedgerStateFilePath ledgerStateFilepaths

        epochSDDChainPoints <- flip filterM ledgerStateChainPoints $ \(slotNo, _) -> do
            result :: [[C.SlotNo]] <- SQL.query c
                [r|SELECT slotNo
                   FROM epoch_sdd
                   WHERE slotNo = ? LIMIT 1 |] (SQL.Only slotNo)
            pure $ not $ null result

        epochNonceChainPoints <- flip filterM ledgerStateChainPoints $ \(slotNo, _) -> do
            result :: [[C.SlotNo]] <- SQL.query c
                [r|SELECT slotNo
                   FROM epoch_nonce
                   WHERE slotNo = ? LIMIT 1 |] (SQL.Only slotNo)
            pure $ not $ null result

        let resumablePoints = Set.toList
                            $ Set.intersection (Set.fromList epochSDDChainPoints)
                                               (Set.fromList epochNonceChainPoints)
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
  :: O.TopLevelConfig (O.CardanoBlock O.StandardCrypto)
  -> FilePath
  -- ^ SQLite database file path
  -> FilePath
  -- ^ Directory from which we will save the various 'LedgerState' as different points in time.
  -> Word64
  -> IO (State EpochStateHandle)
open topLevelConfig dbPath ledgerStateDirPath securityParam = do
    c <- SQL.open dbPath
    SQL.execute_ c "PRAGMA journal_mode=WAL"
    SQL.execute_ c
        [r|CREATE TABLE IF NOT EXISTS epoch_sdd
            ( epochNo INT NOT NULL
            , poolId BLOB NOT NULL
            , lovelace INT NOT NULL
            , slotNo INT NOT NULL
            , blockHeaderHash BLOB NOT NULL
            , blockNo INT NOT NULL
            )|]
    SQL.execute_ c
        [r|CREATE TABLE IF NOT EXISTS epoch_nonce
            ( epochNo INT NOT NULL
            , nonce BLOB NOT NULL
            , slotNo INT NOT NULL
            , blockHeaderHash BLOB NOT NULL
            , blockNo INT NOT NULL
            )|]
    emptyState 1 (EpochStateHandle topLevelConfig c ledgerStateDirPath securityParam)
