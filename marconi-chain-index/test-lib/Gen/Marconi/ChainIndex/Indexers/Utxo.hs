{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}

module Gen.Marconi.ChainIndex.Indexers.Utxo
    ( genUtxoEvents
    , genShelleyEraUtxoEvents
    , genUtxoEventsWithTxs
    , genEventWithShelleyAddressAtChainPoint
    )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (forM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Gen.Cardano.Api.Typed qualified as CGen
import Gen.Marconi.ChainIndex.Mockchain (MockBlock (MockBlock), MockBlockHeader (MockBlockHeader), genMockchain)
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (UtxoEvent), Utxo (Utxo), UtxoHandle, _address)
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
    getStorableEventFromBlock (MockBlock (MockBlockHeader slotNo blockHeaderHash _blockNo) txs) =
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

-- | The effect of a transaction (or a number of them) on the tx output set.
data TxOutBalance =
  TxOutBalance
    { _tobUnspent :: !(Set C.TxIn)
    -- ^ Outputs newly added by the transaction(s)
    , _tobSpent   :: !(Set C.TxIn)
    -- ^ Outputs spent by the transaction(s)
    }
    deriving stock (Eq, Show, Generic)

instance Semigroup TxOutBalance where
    tobL <> tobR =
        TxOutBalance
            { _tobUnspent = _tobUnspent tobR
                         <> (_tobUnspent tobL `Set.difference` _tobSpent tobR)
            , _tobSpent = _tobSpent tobL <> _tobSpent tobR
            }

instance Monoid TxOutBalance where
    mappend = (<>)
    mempty = TxOutBalance mempty mempty

txOutBalanceFromTx :: C.Tx era -> TxOutBalance
txOutBalanceFromTx (C.Tx txBody@(C.TxBody txBodyContent) _) =
    let txId = C.getTxId txBody
        txInputs = Set.fromList $ fst <$> C.txIns txBodyContent
        utxoRefs = Set.fromList
                 $ fmap (\(txIx, _) -> C.TxIn txId $ C.TxIx txIx)
                 $ zip [0..]
                 $ C.txOuts txBodyContent
     in TxOutBalance utxoRefs txInputs

convertTxOutToUtxo :: C.TxId -> C.TxIx -> C.TxOut C.CtxTx C.BabbageEra -> Utxo
convertTxOutToUtxo txId txIx (C.TxOut (C.AddressInEra _ addr) val txOutDatum refScript) =
    let (scriptDataHash, scriptData) =
            case txOutDatum of
              C.TxOutDatumNone       -> (Nothing, Nothing)
              C.TxOutDatumHash _ dh  -> (Just dh, Nothing)
              C.TxOutDatumInTx _ d   -> (Just $ C.hashScriptData d, Just d)
              C.TxOutDatumInline _ d -> (Just $ C.hashScriptData d, Just d)
        (scriptHash, script) =
            case refScript of
              C.ReferenceScriptNone -> (Nothing, Nothing)
              C.ReferenceScript _ scriptInAnyLang@(C.ScriptInAnyLang _ s) ->
                  (Just $ C.hashScript s, Just scriptInAnyLang)
     in Utxo
            (C.toAddressAny addr)
            txId
            txIx
            scriptData
            scriptDataHash
            (C.txOutValueToValue val)
            script
            scriptHash
-- | Override the Utxo address with ShelleyEra address
-- This is used to generate ShelleyEra Utxo events
utxoAddressOverride
  :: C.Address C.ShelleyAddr
  -> Utxo
  -> Utxo
utxoAddressOverride addr utxo =  utxo {_address=C.toAddressAny addr}

-- | Generate ShelleyEra Utxo Events
genShelleyEraUtxoEvents :: Gen [StorableEvent UtxoHandle]
genShelleyEraUtxoEvents = do
  addr <- CGen.genAddressShelley
  genUtxoEvents' (\_id _ix _tx ->
                    utxoAddressOverride addr $ convertTxOutToUtxo _id _ix _tx)

-- TODO Must be reworked following implementation in 'genUtxoEvents'.
genEventWithShelleyAddressAtChainPoint :: C.ChainPoint -> Gen (Utxo.StorableEvent Utxo.UtxoHandle)
genEventWithShelleyAddressAtChainPoint ueChainPoint = do
  txOutRefs <- Gen.list (Range.linear 1 5) CGen.genTxIn
  ueUtxos <- Set.fromList <$> forM txOutRefs genUtxo
  ueInputs <- Gen.set (Range.linear 1 5) CGen.genTxIn
  pure $ Utxo.UtxoEvent {..}

genUtxo :: C.TxIn -> Gen Utxo.Utxo
genUtxo txOutRef = CGen.genAddressShelley >>= genUtxo' txOutRef . C.toAddressAny

genUtxo' :: C.TxIn -> C.AddressAny -> Gen Utxo.Utxo
genUtxo' (C.TxIn _txId _txIx) _address = do
  sc <- CGen.genTxOutDatumHashTxContext C.BabbageEra
  let (_datum, _datumHash)  = Utxo.getScriptDataAndHash sc
  script            <- CGen.genReferenceScript C.ShelleyEra
  _value            <- CGen.genValueForTxOut
  let (_inlineScript, _inlineScriptHash)=  Utxo.getRefScriptAndHash script
  pure $ Utxo.Utxo {..}
