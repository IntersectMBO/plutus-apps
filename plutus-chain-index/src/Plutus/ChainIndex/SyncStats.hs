{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.ChainIndex.SyncStats where

import Cardano.BM.Tracing (ToObject)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Extras (LogMsg, logInfo)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Units (Second, fromMicroseconds)
import Data.Time.Units.Extra ()
import GHC.Generics (Generic)
import Ledger (Slot (Slot))
import Plutus.ChainIndex (Point (PointAtGenesis), tipAsPoint)
import Plutus.ChainIndex qualified as CI
import Plutus.ChainIndex.Lib (ChainSyncEvent (Resume, RollBackward, RollForward))
import Prettyprinter (Pretty (pretty), comma, viaShow, (<+>))
import System.Clock (TimeSpec, toNanoSecs)
import Text.Printf (printf)

data SyncStats = SyncStats
    { syncStatsAppliedBlocks    :: Integer -- ^ Number of applied blocks
    , syncStatsAppliedRollbacks :: Integer -- ^ Number of rollbacks
    , syncStatsChainSyncPoint   :: CI.Point -- ^ Chain index syncing tip
    , syncStatsNodePoint        :: CI.Point -- ^ Current tip of the node
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToObject)

instance Semigroup SyncStats where
    SyncStats n1 m1 ct1 nt1 <> SyncStats n2 m2 ct2 nt2 =
        SyncStats (n1 + n2) (m1 + m2) (ct1 <> ct2) (nt1 <> nt2)

instance Monoid SyncStats where
    mempty = SyncStats 0 0 mempty mempty

data SyncLog = SyncLog
    { syncStateSyncLog  :: SyncState -- ^ State of the syncing
    , syncStatsSyncLog  :: SyncStats -- ^ Stats of the syncing
    , syncPeriodSyncLog :: Second -- ^ Period in seconds used to accumulate log events
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToObject)

instance Pretty SyncLog where
  pretty = \case
    SyncLog syncState (SyncStats numRollForward numRollBackwards chainSyncPoint _) period ->
        let currentTipMsg NotSyncing = ""
            currentTipMsg _          = "Current tip is" <+> pretty chainSyncPoint
         in
            pretty syncState
                <+> "Processed"
                <+> pretty numRollForward
                <+> "blocks"
                <> comma
                <+> pretty numRollBackwards
                <+> "rollbacks in the last"
                <+> viaShow period
                <> "."
                <+> currentTipMsg syncState

data SyncState = Synced | Syncing Double | NotSyncing
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToObject)

isSyncStateSynced :: SyncState -> Bool
isSyncStateSynced Synced = True
isSyncStateSynced _      = False

isSyncStateNotSyncing :: SyncState -> Bool
isSyncStateNotSyncing NotSyncing = True
isSyncStateNotSyncing _          = False

isSyncStateSyncing :: SyncState -> Bool
isSyncStateSyncing (Syncing _) = True
isSyncStateSyncing _           = False

instance Pretty SyncState where
  pretty = \case
    Synced      -> "Still in sync."
    Syncing pct -> "Syncing (" <> pretty (printf "%.2f" pct :: String) <> "%)."
    NotSyncing  -> "Not syncing."

-- | Log syncing summary.
logProgress :: forall effs. (Member (LogMsg SyncLog) effs) => [ChainSyncEvent] -> TimeSpec -> Eff effs ()
logProgress events period = do
    let syncStats = foldl (<>) mempty $ map convertEventToSyncStats events
    let syncState = getSyncStateFromStats syncStats
    let syncLog = SyncLog syncState syncStats (fromMicroseconds $ toNanoSecs period `div` 1000)
    case syncState of
      NotSyncing -> pure () -- logWarn syncLog
      _          -> logInfo syncLog

-- | Get the 'SyncState' for a 'SyncState'.
--
-- TODO: The syncing percentage is valid when the node is already fully synced.
-- But when the node and chain-index are started at the same time, the syncing
-- percentage is not a valid number considering the actual tip of the node.
-- Find a better way to calculate this percentage.
getSyncStateFromStats :: SyncStats -> SyncState
getSyncStateFromStats (SyncStats _ _ chainSyncPoint nodePoint) =
    getSyncState chainSyncPoint nodePoint

getSyncState :: CI.Point -> CI.Point -> SyncState
getSyncState chainIndexSyncPoint nodePoint =
    case (chainIndexSyncPoint, nodePoint) of
        (_, PointAtGenesis) -> NotSyncing
        (CI.PointAtGenesis, CI.Point _ _) -> Syncing 0
        (CI.Point (Slot chainSyncSlot) _, CI.Point (Slot nodeSlot) _)
          -- TODO: MAGIC number here. Is there a better number?
          -- 100 represents the number of slots before the
          -- node where we consider the chain-index to be synced.
          | nodeSlot - chainSyncSlot < 100 -> Synced
        (CI.Point (Slot chainSyncSlot) _, CI.Point (Slot nodeSlot) _) ->
            let pct = ((100 :: Double) * fromIntegral chainSyncSlot) / fromIntegral nodeSlot
             in Syncing pct

convertEventToSyncStats :: ChainSyncEvent -> SyncStats
convertEventToSyncStats (RollForward (CI.Block chainSyncTip _) nodeTip) =
    SyncStats 1 0 (tipAsPoint chainSyncTip) (tipAsPoint nodeTip)
convertEventToSyncStats (RollBackward chainSyncPoint nodeTip) =
    SyncStats 0 1 chainSyncPoint (tipAsPoint nodeTip)
convertEventToSyncStats (Resume chainSyncPoint) =
    SyncStats 0 0 chainSyncPoint mempty
