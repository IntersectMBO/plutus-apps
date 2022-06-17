module Main where

import Cardano.Api qualified
import Common (Options (Options, optionsChainPoint, optionsNetworkId, optionsSocketPath), parseOptions, printJson,
               workaround)
import Data.Foldable (toList)
import Ledger qualified
import Orphans ()
import Plutus.Script.Utils.V1.Scripts qualified
import Plutus.Streaming (withChainSyncEventStream)
import Streaming.Prelude qualified as S

--
-- Accessory functions
--

transactions ::
  Cardano.Api.BlockInMode Cardano.Api.CardanoMode ->
  [Ledger.CardanoTx]
transactions (Cardano.Api.BlockInMode (Cardano.Api.Block _ txs) eim) =
  map (\tx -> Ledger.CardanoApiTx (workaround (Ledger.SomeTx tx) eim)) txs

txInsAndOuts ::
  Ledger.CardanoTx ->
  (Ledger.TxId, [Ledger.TxOutRef], [Ledger.TxOutRef])
txInsAndOuts tx = (txId, inRefs, outRefs)
  where
    txId = Ledger.getCardanoTxId tx
    inRefs = map Ledger.txInRef $ toList $ Ledger.getCardanoTxInputs tx
    outRefs = map snd $ Ledger.getCardanoTxOutRefs tx

datums ::
  Ledger.CardanoTx ->
  [(Plutus.Script.Utils.V1.Scripts.DatumHash, Plutus.Script.Utils.V1.Scripts.Datum)]
datums tx = do
  let txIns = toList $ Ledger.getCardanoTxInputs tx
  (Ledger.TxIn _ (Just (Ledger.ConsumeScriptAddress _validator _redeemer datum))) <- txIns
  pure (Plutus.Script.Utils.V1.Scripts.datumHash datum, datum)

--
-- Main
--

main :: IO ()
main = do
  Options {optionsSocketPath, optionsNetworkId, optionsChainPoint} <- parseOptions

  withChainSyncEventStream optionsSocketPath optionsNetworkId optionsChainPoint $
    printJson
      . S.map -- Each ChainSyncEvent
        ( fmap -- Inside the payload of RollForward events
            ( fmap -- Each transaction
                (\tx -> (txInsAndOuts tx, datums tx))
                . transactions
            )
        )
