module Main where

import Cardano.Api qualified as Cardano
import Common (Options (Options, optionsChainPoint, optionsNetworkId, optionsSocketPath), parseOptions)
import Control.Monad.Trans.State (StateT, evalStateT, get)
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Plutus.Streaming (ChainSyncEvent (RollBackward, RollForward), withChainSyncEventStream)
import Streaming (MFunctor (hoist), MonadIO (liftIO))
import Streaming.Prelude qualified as S

--
-- Main
--

main :: IO ()
main = do
  Options {optionsSocketPath, optionsNetworkId, optionsChainPoint} <- parseOptions

  withChainSyncEventStream optionsSocketPath optionsNetworkId optionsChainPoint $ \connectInfo stream -> do
    evalStateT (S.stdoutLn . S.mapM (toHashSlotEpoch connectInfo) $ hoist liftIO stream)
      =<< getEraHistory connectInfo
  where
    toHashSlotEpoch ::
      Cardano.LocalNodeConnectInfo Cardano.CardanoMode ->
      ChainSyncEvent (Cardano.BlockInMode Cardano.CardanoMode) ->
      StateT (Cardano.EraHistory Cardano.CardanoMode) IO String
    toHashSlotEpoch connectInfo event = do
      eraHistory <- get
      let go onPastHorizon slot hash history =
            case Cardano.slotToEpoch slot history of
              Left pastHorizonErr -> onPastHorizon pastHorizonErr
              Right (epoch, _, _) -> do
                pure $
                  "Block hash: "
                    <> show (renderBlockHash hash)
                    <> ", slot: "
                    <> show slot
                    <> ", epoch: "
                    <> show epoch
          toHashSlotEpoch' slot hash =
            go
              ( \_pastHorizonErr ->
                  -- When we get `PastHorizonException` the first time, update the `EraHistory`
                  -- and retry. If we get it the second time, then fail.
                  go (\err -> fail $ "Past horizon: " <> show err) slot hash =<< getEraHistory connectInfo
              )
              slot
              hash
              eraHistory
      case event of
        RollForward (Cardano.BlockInMode (Cardano.Block (Cardano.BlockHeader slot hash _) _txs) _era) _ct ->
          ("RollForward: " <>) <$> toHashSlotEpoch' slot hash
        RollBackward (Cardano.ChainPoint slot hash) _ ->
          ("RollBackward: " <>) <$> toHashSlotEpoch' slot hash
        RollBackward Cardano.ChainPointAtGenesis _ ->
          pure "RollBackward to Genesis"

getEraHistory ::
  (MonadIO m, MonadFail m) =>
  Cardano.LocalNodeConnectInfo Cardano.CardanoMode ->
  m (Cardano.EraHistory Cardano.CardanoMode)
getEraHistory connectInfo = do
  history <- liftIO $ Cardano.executeLocalStateQueryExpr connectInfo Nothing $ \_ ->
    Cardano.queryExpr (Cardano.QueryEraHistory Cardano.CardanoModeIsMultiEra)
  either (\err -> fail $ "Failed to get era history: " <> show err) pure history

renderBlockHash :: Cardano.Hash Cardano.BlockHeader -> Text.Text
renderBlockHash = Text.decodeLatin1 . Base16.encode . Cardano.serialiseToRawBytes
