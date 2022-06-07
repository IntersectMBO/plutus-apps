{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Plutus.Script.Evaluation.Dump
  (dumpScriptEvents
  ) where

import Cardano.Api qualified as Cardano
import Codec.Serialise qualified as CBOR
import Control.Exception (handle, throwIO)
import Control.Monad (unless)
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.State (evalStateT, get, put)
import Data.ByteString.Base16 qualified as B16
import Data.Foldable (traverse_)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Data.Word (Word64)
import Plutus.Script.Evaluation.Options qualified as O
import Plutus.Script.Evaluation.Types (Block, Checkpoint (Checkpoint), ScriptEvent, ScriptM,
                                       StreamerState (StreamerState, ssCount, ssEvents))
import Plutus.Streaming (ApplyBlockException, ChainSyncEvent (RollBackward, RollForward), ledgerStateEvents,
                         withChainSyncEventStream)
import PyF (fmt)
import Streaming (MFunctor (hoist), MonadIO (liftIO), Of, Stream)
import Streaming.Prelude qualified as S
import System.Directory.Extra (listFiles, removeFile)
import System.FilePath (isExtensionOf, takeBaseName, (<.>), (</>))
import System.IO (hPrint, stderr)

-- | Stream blocks from a local node, and periodically dump ledger events
-- and checkpoint ledger state.
dumpScriptEvents :: O.Options -> IO ()
dumpScriptEvents opts = do
  (env, ledgerStateAtGenesis) <-
    either (fail . Text.unpack . Cardano.renderInitialLedgerStateError) pure
      =<< runExceptT (Cardano.initialLedgerState (O.optsConfigPath opts))
  let dir = O.optsDir opts
      go :: [FilePath] -> IO ()
      go fps = do
        (chainPoint, ledgerState, onApplyBlockException) <- case fps of
          -- No checkpoint to use, so start from Genesis.
          [] -> pure (Cardano.ChainPointAtGenesis, ledgerStateAtGenesis, throwIO)
          -- Try the latest checkpoint, and if we get an `ApplyBlockException` (which likely
          -- means the checkpointed block was rolled back), try the next one.
          latestStateFile : rest -> do
            Checkpoint chainPoint ledgerState <- CBOR.readFileDeserialise latestStateFile
            cleanupStateAndEventFiles dir latestStateFile
            putStrLn
              [fmt|
Starting from checkpoint in {latestStateFile}
  slot: {maybe "Genesis" show (Cardano.chainPointToSlotNo chainPoint)}
  hash: {maybe "Genesis" renderBlockHash (Cardano.chainPointToHeaderHash chainPoint)}
|]
            pure (chainPoint, ledgerState, \(e :: ApplyBlockException) -> hPrint stderr e >> go rest)

        handle onApplyBlockException
          . withChainSyncEventStream
            (O.optsSocketPath opts)
            (O.optsNetworkId opts)
            chainPoint
          $ \blockStream -> do
            let eventStream ::
                  Stream
                    (Of (ChainSyncEvent Block, (Cardano.LedgerState, [Cardano.LedgerEvent])))
                    ScriptM
                    ()
                eventStream =
                  hoist liftIO $
                    ledgerStateEvents env ledgerState Cardano.QuickValidation blockStream
            flip evalStateT (StreamerState 0 []) $
              runStream dir (O.optsBlocksPerFile opts) (O.optsEventsPerFile opts) eventStream

  go =<< listStateFiles dir

runStream ::
  forall r.
  FilePath ->
  -- | Blocks per file
  Word64 ->
  -- | Events per file
  Word64 ->
  Stream
    (Of (ChainSyncEvent Block, (Cardano.LedgerState, [Cardano.LedgerEvent])))
    ScriptM
    r ->
  ScriptM r
runStream dir blocksPerFile eventsPerFile stream = do
  S.mapM_ (uncurry (uncurry . checkpoint)) stream
  where
    checkpoint :: ChainSyncEvent Block -> Cardano.LedgerState -> [Cardano.LedgerEvent] -> ScriptM ()
    checkpoint ev ledgerState ledgerEvents = case ev of
      RollForward block _tip -> do
        streamerState <- get
        if ssCount streamerState >= blocksPerFile ||
              fromIntegral (length (ssEvents streamerState)) >= eventsPerFile
          then do
            time <- liftIO getCurrentTime
            let eventFile = dir </> iso8601Show time <.> eventsFileExt
            let scriptEvents = ssEvents streamerState
            unless (null scriptEvents) . liftIO $ CBOR.writeFileSerialise eventFile scriptEvents
            -- Writing state (checkpoint) file after events file ensures the events of a
            -- checkpoint are persisted.
            let stateFile = dir </> iso8601Show time <.> stateFileExt
                chainPoint = blockChainPoint block
            liftIO $ CBOR.writeFileSerialise stateFile (Checkpoint chainPoint ledgerState)
            put $ StreamerState 0 []
            liftIO $
              putStrLn
                [fmt|
Created new checkpoint in {stateFile}
  number of blocks: {ssCount streamerState}
  number of ledger events: {length ledgerEvents}
  number of script evaluation events: {length scriptEvents}
  slot: {maybe "Genesis" show (Cardano.chainPointToSlotNo chainPoint)}
  hash: {maybe "Genesis" renderBlockHash (Cardano.chainPointToHeaderHash chainPoint)}
|]
          else do
            put $
              streamerState
                { ssEvents = mapMaybe toScriptEvent ledgerEvents ++ ssEvents streamerState,
                  ssCount = ssCount streamerState + 1
                }
      RollBackward {} ->
        -- Nothing special needs to be done on `RollBackward`, since there's no harm
        -- dumping events in blocks that are rolled back. In fact it's a good thing - it
        -- gives us more data to test with.
        pure ()

stateFileExt, eventsFileExt :: String
stateFileExt = "state"
eventsFileExt = "event"

blockChainPoint :: Block -> Cardano.ChainPoint
blockChainPoint (Cardano.BlockInMode (Cardano.Block (Cardano.BlockHeader slot hash _) _) _) =
  Cardano.ChainPoint slot hash

toScriptEvent :: Cardano.LedgerEvent -> Maybe ScriptEvent
toScriptEvent = error "Not implemented: need https://github.com/input-output-hk/cardano-node/pull/3984"
  -- \case
  --   Cardano.SuccessfulPlutusScript ds -> Just (ScriptEventSuccess ds)
  --   Cardano.FailedPlutusScript ds     -> Just (ScriptEventFailure ds)
  --   _                                 -> Nothing

listStateFiles :: FilePath -> IO [FilePath]
listStateFiles =
  fmap (sortBy (flip compare) . filter (stateFileExt `isExtensionOf`)) . listFiles

-- | Remove the state and event files whose timestamps are greater than the given state file
cleanupStateAndEventFiles :: FilePath -> FilePath -> IO ()
cleanupStateAndEventFiles dir stateFile = do
  newerStateAndEventFiles <-
    takeWhile (\f -> takeBaseName f > takeBaseName stateFile)
      . filter (\f -> stateFileExt `isExtensionOf` f || eventsFileExt `isExtensionOf` f)
      <$> listFiles dir
  traverse_ removeFile newerStateAndEventFiles

renderBlockHash :: Cardano.Hash Cardano.BlockHeader -> Text.Text
renderBlockHash = Text.decodeLatin1 . B16.encode . Cardano.serialiseToRawBytes
