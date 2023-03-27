{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
module Plutus.ChainIndex.MarconiSpec (tests) where

import Control.Monad.IO.Class (MonadIO, liftIO)

import Hedgehog.Internal.Property (Property, forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Control.Concurrent (readMVar)
import Control.Lens (_1, folded, to, toListOf, (^.), (^..), (^?))
import Control.Monad (void)
import Control.Tracer (nullTracer)
import Data.Default (def)
import Gen.Marconi.ChainIndex.Mockchain (MockBlock (MockBlock), genMockchain)
import Hedgehog (Gen)
import Hedgehog qualified
import Ledger (TxIn)
import Marconi.ChainIndex.Indexers.Utxo (StorableEvent (..), UtxoHandle)
import Marconi.ChainIndex.Indexers.Utxo qualified as Utxo
import Marconi.Core.Storable qualified as Storable
import Plutus.ChainIndex (ChainIndexTxOut, ChainSyncBlock (Block), appendBlocks, citoAddress, citxInputs, citxOutputs,
                          utxoSetAtAddress)
import Plutus.ChainIndex.Compatibility (tipFromCardanoBlockHeader)
import Plutus.ChainIndex.Marconi (ChainIndexIndexers (ChainIndexIndexers), ChainIndexIndexersMVar,
                                  RunRequirements (RunRequirements), boxChainIndexIndexers, runChainIndexEffects,
                                  utxosIndexerMVar)
import Plutus.ChainIndex.Types (ChainSyncBlock (blockTxs), chainIndexTxOutputs)
import Plutus.Contract.CardanoAPI (fromCardanoTx)

tests :: TestTree
tests = testGroup "Plutus.ChainIndex.MarconiSpec"
    [ testGroup "testSetAtAddress"
        [ testPropertyNamed "Indexer do store blocks txOuts" "checkTxOutStorage"
            checkTxOutStorage
        , testPropertyNamed "Indexer do store blocks txIn" "checkTxInStorage"
            checkTxInStorage
        ]
    ]

genBlocks :: Gen [ChainSyncBlock]
genBlocks = fmap fromMockBlock <$> genMockchain
    where
        fromMockBlock :: MockBlock C.BabbageEra -> ChainSyncBlock
        fromMockBlock (MockBlock header txs) =
            Block
                (tipFromCardanoBlockHeader header)
                ((,def) . fromCardanoTx C.BabbageEraInCardanoMode <$> txs)

newChainIndexIndexers :: IO ChainIndexIndexersMVar
newChainIndexIndexers = do
    indexers <- ChainIndexIndexers
        <$> Utxo.open ":memory:" (Utxo.Depth 10)
    boxChainIndexIndexers indexers

getUtxoEvents :: MonadIO m => ChainIndexIndexersMVar -> m [StorableEvent UtxoHandle]
getUtxoEvents indexers =
    liftIO $ readMVar (indexers ^. utxosIndexerMVar) >>= Storable.getEvents

allTxOuts :: ChainSyncBlock -> [ChainIndexTxOut]
allTxOuts =
    toListOf (to blockTxs . folded . _1 . citxOutputs . chainIndexTxOutputs)

allTxIns :: ChainSyncBlock -> [TxIn]
allTxIns =
    toListOf (to blockTxs . folded . _1 . citxInputs . folded)


checkTxOutStorage :: Property
checkTxOutStorage = property $ do
    blocks <- forAll genBlocks
    indexers <- liftIO newChainIndexIndexers
    let txOutAddr = blocks ^? folded . to blockTxs . folded . _1
                  . citxOutputs . chainIndexTxOutputs . to citoAddress

    maybe
        Hedgehog.success
        (\addr -> do
            void $ liftIO $ runChainIndexEffects (RunRequirements nullTracer indexers) $ do
                 appendBlocks blocks
                 utxoSetAtAddress def addr
            events <- getUtxoEvents indexers
            let originalTxOuts = blocks >>= allTxOuts
            Hedgehog.annotateShow events
            Hedgehog.annotateShow originalTxOuts
            let eventUtxos = events ^.. folded . to ueUtxos . folded
            length eventUtxos Hedgehog.=== length originalTxOuts
        )
        txOutAddr

checkTxInStorage :: Property
checkTxInStorage = property $ do
    blocks <- forAll genBlocks
    indexers <- liftIO newChainIndexIndexers
    let txOutAddr = blocks ^? folded . to blockTxs . folded . _1
                  . citxOutputs . chainIndexTxOutputs . to citoAddress
    maybe
        Hedgehog.success
        (\addr -> do
            void $ liftIO $ runChainIndexEffects (RunRequirements nullTracer indexers) $ do
                 appendBlocks blocks
                 utxoSetAtAddress def addr
            events <- getUtxoEvents indexers
            let originalTxIns = blocks >>= allTxIns
            Hedgehog.annotateShow events
            Hedgehog.annotateShow originalTxIns
            let eventTxIns = events ^.. folded . to ueInputs . folded
            length eventTxIns Hedgehog.=== length originalTxIns
        )
        txOutAddr

