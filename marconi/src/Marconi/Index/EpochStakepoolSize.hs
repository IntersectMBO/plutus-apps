{-# OPTIONS_GHC -Wno-orphans      #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Marconi.Index.EpochStakepoolSize where

import Control.Monad.Trans.Class (lift)
import Data.Coerce (coerce)
import Data.Foldable (forM_)
import Data.Function (on, (&))
import Data.List (groupBy)
import Data.Map qualified as M
import Data.Maybe qualified as P
import Data.Sequence qualified as Seq
import Data.Tuple (swap)
import Data.VMap qualified as VMap
import Database.SQLite.Simple qualified as SQL
import Database.SQLite.Simple.FromField qualified as SQL
import Database.SQLite.Simple.ToField qualified as SQL
import Streaming.Prelude qualified as S

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Streaming qualified as CS

import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Compactible qualified as L
import Cardano.Ledger.Credential qualified as LC
import Cardano.Ledger.Era qualified as LE
import Cardano.Ledger.Keys qualified as LK
import Cardano.Ledger.Shelley.EpochBoundary qualified as Shelley
import Cardano.Ledger.Shelley.LedgerState qualified as SL
import Cardano.Ledger.Shelley.LedgerState qualified as Shelley
import Ouroboros.Consensus.Cardano.Block qualified as O
import Ouroboros.Consensus.Shelley.Ledger qualified as O

import Cardano.Streaming.Helpers (getEpochNo)

-- * Event

newtype Event = Event (C.EpochNo, M.Map C.PoolId C.Lovelace)

-- | Convert a stream of ledger states for every block to a stream of
-- ledger states for every epoch. We also skip the Byron era because
-- it doesn't have any staking information.
toEvents :: S.Stream (S.Of C.LedgerState) IO r -> S.Stream (S.Of Event) IO r
toEvents source = source
  & S.mapMaybe toNoByron
  & firstEventOfEveryEpoch
  where
    -- Skip Byron era as it doesn't have staking.
    toNoByron :: C.LedgerState -> Maybe Event
    toNoByron ls = if
      | Just epochNo <- getEpochNo ls
      , Just m <- getStakeMap ls -> Just $ Event (epochNo, m)
      | otherwise -> Nothing

    -- We get LedgerState at every block from the ledgerStates
    -- streamer but we only want the first one of every epoch, so we
    -- zip them and only emit ledger states at epoch boundaries.
    firstEventOfEveryEpoch :: S.Stream (S.Of Event) IO r -> S.Stream (S.Of Event) IO r
    firstEventOfEveryEpoch source' = source'
      & S.slidingWindow 2
      & S.mapMaybe (\case
                  (Event (e0, _) Seq.:<| t@(Event (e1, _)) Seq.:<| Seq.Empty)
                    | succ e0 == e1 -> Just t
                    | e0 == e1 -> Nothing
                    | otherwise -> error $ "This should never happen: consequent epochs wider apart than by one: " <> show (e0, e1)
                  _ -> error "This should never happen"
              )

-- | From LedgerState get epoch stakepool size: a mapping of pool ID
-- to amount staked. We do this by getting the _pstatkeSet stake
-- snapshot and then use _delegations and _stake to resolve it into
-- the desired mapping.
getStakeMap :: C.LedgerState -> Maybe (M.Map C.PoolId C.Lovelace)
getStakeMap ledgerState' = case ledgerState' of
  C.LedgerStateByron _st                  -> Nothing
  C.LedgerStateShelley st                 -> fromState st
  C.LedgerStateAllegra st                 -> fromState st
  C.LedgerStateMary st                    -> fromState st
  C.LedgerStateAlonzo st                  -> fromState st
  -- TODO Pattern LedgerStateBabbage missing in cardano-node
  -- https://github.com/input-output-hk/cardano-node/blob/release/1.35/cardano-api/src/Cardano/Api/LedgerState.hs#L252-L281,
  -- swap this to a pattern when it appears.
  C.LedgerState (O.LedgerStateBabbage st) -> fromState st
  where
    fromState
      :: forall proto era c
       . (c ~ LE.Crypto era, c ~ O.StandardCrypto)
      => O.LedgerState (O.ShelleyBlock proto era)
      -> Maybe (M.Map C.PoolId C.Lovelace)
    fromState st = Just res
      where
        nes = O.shelleyLedgerState st :: SL.NewEpochState era

        stakeSnapshot = Shelley._pstakeSet . Shelley.esSnapshots . Shelley.nesEs $ nes :: Shelley.SnapShot c
        stakes = Shelley.unStake $ Shelley._stake stakeSnapshot :: VMap.VMap VMap.VB VMap.VP (LC.Credential 'LK.Staking c) (L.CompactForm L.Coin)

        delegations :: VMap.VMap VMap.VB VMap.VB (LC.Credential 'LK.Staking c) (LK.KeyHash 'LK.StakePool c)
        delegations = Shelley._delegations stakeSnapshot

        res :: M.Map C.PoolId C.Lovelace
        res = M.fromListWith (+) $ map swap $ P.catMaybes $ VMap.elems $
          VMap.mapWithKey (\cred spkHash -> (\c -> (C.Lovelace $ coerce $ L.fromCompact c, f spkHash)) <$> VMap.lookup cred stakes) delegations

        f :: (LK.KeyHash 'LK.StakePool c) -> C.PoolId
        f xk = C.StakePoolKeyHash xk

indexer
  :: FilePath -> FilePath -> SQL.Connection
  -> S.Stream (S.Of Event) IO r
indexer conf socket dbCon =
    CS.ledgerStates conf socket C.QuickValidation
  & toEvents
  & sqlite dbCon

-- * Sqlite back-end

-- | Consume a stream of events and write them to the database. Also
-- passes events on after they are persisted -- useful to knwow when
-- something has been persisted.
sqlite :: SQL.Connection -> S.Stream (S.Of Event) IO r -> S.Stream (S.Of Event) IO r
sqlite c source = do
  lift $ SQL.execute_ c
      "CREATE TABLE IF NOT EXISTS stakepool_delegation (poolId BLOB NOT NULL, lovelace INT NOT NULL, epochNo INT NOT NULL)"
  loop source

  where
    toRows :: Event -> [(C.EpochNo, C.PoolId, C.Lovelace)]
    toRows (Event (epochNo, m)) = map (\(keyHash, lovelace) -> (epochNo, keyHash, lovelace)) $ M.toList m

    loop :: S.Stream (S.Of Event) IO r -> S.Stream (S.Of Event) IO r
    loop source' = lift (S.next source') >>= \case
        Left r -> pure r
        Right (event, source'') -> do
          lift $ forM_ (toRows event) $ \row ->
            SQL.execute c "INSERT INTO stakepool_delegation (epochNo, poolId, lovelace) VALUES (?, ?, ?)" row
          S.yield event
          loop source''


instance SQL.ToField C.EpochNo where
  toField (C.EpochNo word64) = SQL.toField word64
instance SQL.FromField C.EpochNo where
  fromField f = C.EpochNo <$> SQL.fromField f

instance SQL.ToField C.Lovelace where
  toField = SQL.toField @Integer . coerce
instance SQL.FromField C.Lovelace where
  fromField = coerce . SQL.fromField @Integer

instance SQL.FromField C.PoolId where
  fromField f = do
    bs <- SQL.fromField f
    case C.deserialiseFromRawBytes (C.AsHash C.AsStakePoolKey) bs of
      Just h -> pure h
      _      -> SQL.returnError SQL.ConversionFailed f " PoolId"

instance SQL.ToField C.PoolId where
  toField = SQL.toField . C.serialiseToRawBytes

queryByEpoch :: SQL.Connection -> C.EpochNo -> IO Event
queryByEpoch c epochNo = do
  xs :: [(C.PoolId, C.Lovelace)] <- SQL.query c "SELECT poolId, lovelace FROM stakepool_delegation WHERE epochNo = ?" (SQL.Only epochNo)
  return $ Event (epochNo, M.fromList xs)

queryPoolId :: SQL.Connection -> C.PoolId -> IO [(C.EpochNo, C.Lovelace)]
queryPoolId c poolId = do
  SQL.query c "SELECT epochNo, lovelace FROM stakepool_delegation WHERE poolId = ?" (SQL.Only poolId)

queryAll :: SQL.Connection -> IO [Event]
queryAll c = do
  all' :: [(C.EpochNo, C.PoolId, C.Lovelace)] <- SQL.query_ c "SELECT epochNo, poolId, lovelace FROM stakepool_delegation ORDER BY epochNo ASC"
  let
    lastTwo (_, a, b) = (a, b)
    result = all'
      & groupBy ((==) `on` (\(e, _, _) -> e))
      & map (\case xs@((e, _, _) : _) -> Just $ Event (e, M.fromList $ map lastTwo xs); _ -> Nothing)
      & P.catMaybes
  pure result
