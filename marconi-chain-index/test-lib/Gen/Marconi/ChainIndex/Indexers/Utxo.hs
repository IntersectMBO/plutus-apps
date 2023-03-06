{-# LANGUAGE DerivingStrategies #-}

module Gen.Marconi.ChainIndex.Indexers.Utxo
    ( genUtxoEvents
    , genShelleyEraUtxoEvents
    , genUtxoEventsWithTxs
    )
where

import Cardano.Api qualified as C
import Control.Monad (forM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Gen.Cardano.Api.Typed qualified as CGen
import Gen.Marconi.ChainIndex.Mockchain (BlockHeader (BlockHeader), MockBlock (MockBlock), genMockchain)
import Hedgehog (Gen)
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (UtxoEvent), TxOutBalance (TxOutBalance), Utxo, UtxoHandle,
                                         _address, convertTxOutToUtxo, txOutBalanceFromTx)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo

-- | Generates a list of UTXO events.
--
-- This generators has the following properties:
--
--   * that any generated UTXO is unique
--   * for any spent tx output, there must be a UTXO created in a previous event
genUtxoEvents :: Gen [StorableEvent UtxoHandle]
genUtxoEvents = genUtxoEvents' convertTxOutToUtxo

genUtxoEvents'
  :: (C.TxId -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo)
  -> Gen [StorableEvent UtxoHandle]
genUtxoEvents' txOutToUtxo = fmap fst <$> genUtxoEventsWithTxs' txOutToUtxo

-- | Generates a list of UTXO events, along with the list of transactions that generated each of
-- them.
--
-- This generators has the following properties:
--
--   * that any generated UTXO is unique
--   * for any spent tx output, there must be a UTXO created in a previous event
genUtxoEventsWithTxs :: Gen [(StorableEvent UtxoHandle, MockBlock C.BabbageEra)]
genUtxoEventsWithTxs = genUtxoEventsWithTxs' convertTxOutToUtxo

genUtxoEventsWithTxs'
    :: (C.TxId -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo)
    -> Gen [(StorableEvent UtxoHandle, MockBlock C.BabbageEra)]
genUtxoEventsWithTxs' txOutToUtxo = do
    fmap (\block -> (getStorableEventFromBlock block, block)) <$> genMockchain
  where
    getStorableEventFromBlock :: MockBlock C.BabbageEra -> StorableEvent UtxoHandle
    getStorableEventFromBlock (MockBlock (BlockHeader slotNo blockHeaderHash _blockNo) txs) =
        let (TxOutBalance utxos spentTxOuts) = foldMap txOutBalanceFromTx txs
            utxoMap = foldMap getUtxosFromTx txs
            resolvedUtxos = Set.fromList
                          $ mapMaybe (`Map.lookup` utxoMap)
                          $ Set.toList utxos
         in UtxoEvent resolvedUtxos spentTxOuts (C.ChainPoint slotNo blockHeaderHash)

    getUtxosFromTx :: C.Tx C.BabbageEra -> Map C.TxIn Utxo
    getUtxosFromTx (C.Tx txBody@(C.TxBody txBodyContent) _) =
        let txId = C.getTxId txBody
         in Map.fromList
                $ fmap (\(txIx, txOut) -> ( C.TxIn txId (C.TxIx txIx)
                                          , txOutToUtxo txId (C.TxIx txIx) txOut))
                $ zip [0..]
                $ C.txOuts txBodyContent

-- | Override the Utxo address with ShelleyEra address
-- This is used to generate ShelleyEra Utxo events
utxoAddressOverride
  :: C.Address C.ShelleyAddr
  -> Utxo
  -> Utxo
utxoAddressOverride addr utxo = utxo { _address = C.toAddressAny addr }

-- | Generate ShelleyEra Utxo Events
genShelleyEraUtxoEvents :: Gen [StorableEvent Utxo.UtxoHandle]
genShelleyEraUtxoEvents = do
  events <- genUtxoEvents
  forM events (\e -> do
                  let utxos = Utxo.ueUtxos e
                  us <- forM (Set.toList utxos) (\u -> do
                                 a <- CGen.genAddressShelley
                                 pure $ utxoAddressOverride a u)
                  pure e {Utxo.ueUtxos=Set.fromList us}
              )
