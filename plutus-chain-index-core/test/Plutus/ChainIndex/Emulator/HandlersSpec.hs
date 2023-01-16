{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Plutus.ChainIndex.Emulator.HandlersSpec (tests) where

import Control.Lens
import Control.Monad (forM)
import Control.Monad.Freer (Eff, interpret, reinterpret, runM)
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Extras (LogMessage, LogMsg, handleLogWriter)
import Control.Monad.Freer.State (State, runState)
import Control.Monad.Freer.Writer (runWriter)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Set qualified as S
import Generators qualified as Gen
import Plutus.ChainIndex (ChainIndexLog, ChainSyncBlock (Block), Page (pageItems), PageQuery (PageQuery),
                          Tip (Tip, TipAtGenesis), TxProcessOption (TxProcessOption, tpoStoreTx), appendBlocks,
                          citxTxId, tipSlot, txFromTxId, unspentTxOutFromRef, utxoSetMembership, utxoSetWithCurrency)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse), isUtxo)
import Plutus.ChainIndex.ChainIndexError (ChainIndexError)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect, ChainIndexQueryEffect, getTip)
import Plutus.ChainIndex.Emulator.Handlers (ChainIndexEmulatorState, handleControl, handleQuery)
import Plutus.ChainIndex.Tx (ChainIndexTxOut (citoValue), txOuts)
import Plutus.V1.Ledger.Value (AssetClass (AssetClass))

import Hedgehog (Property, assert, forAll, property, (===))
import Ledger.Tx.CardanoAPI (fromCardanoAssetId)
import Ledger.Value.CardanoAPI qualified as Value
import Test.Tasty
import Test.Tasty.Hedgehog (testPropertyNamed)
import Util (utxoSetFromBlockAddrs)

tests :: TestTree
tests = do
  testGroup "chain index emulator handlers"
    [ testGroup "txFromTxId"
      [ testPropertyNamed "get tx from tx id" "txFromTxIdSpec" txFromTxIdSpec
      ]
    , testGroup "noTxSlot"
      [ testPropertyNamed "Adding empty slot updates the tip" "noTxSlot" noTxSlot
      ]
    , testGroup "utxoSetAtAddress"
      [ testPropertyNamed "each txOutRef should be unspent" "eachTxOutRefAtAddressShouldBeUnspentSpec" eachTxOutRefAtAddressShouldBeUnspentSpec
      ]
    , testGroup "unspentTxOutFromRef"
      [ testPropertyNamed "get unspent tx out from ref" "eachTxOutRefAtAddressShouldHaveTxOutSpec" eachTxOutRefAtAddressShouldHaveTxOutSpec
      ]
    , testGroup "utxoSetWithCurrency"
      [ testPropertyNamed "each txOutRef should be unspent" "eachTxOutRefWithCurrencyShouldBeUnspentSpec" eachTxOutRefWithCurrencyShouldBeUnspentSpec
      , testPropertyNamed "should restrict to non-ADA currencies" "cantRequestForTxOutRefsWithAdaSpec" cantRequestForTxOutRefsWithAdaSpec
      ]
    , testGroup "BlockProcessOption"
      [ testPropertyNamed "do not store txs" "doNotStoreTxs" doNotStoreTxs
      ]

    ]

-- | Tests we can correctly query a tx in the database using a tx id. We also
-- test with an non-existant tx id.
txFromTxIdSpec :: Property
txFromTxIdSpec = property $ do
  (tip, block@(fstTx:_)) <- forAll $ Gen.evalTxGenState Gen.genNonEmptyBlock
  unknownTxId <- forAll Gen.genRandomTxId
  txs <- liftIO $ runEmulatedChainIndex mempty $ do
    appendBlocks [Block tip (map (, def) block)]
    tx <- txFromTxId (view citxTxId fstTx)
    tx' <- txFromTxId unknownTxId
    pure (tx, tx')

  case txs of
    Right (Just tx, Nothing) -> fstTx === tx
    _                        -> Hedgehog.assert False

-- | Test that when a new slot is appended without any blocks, the tip is still updated to the new slot.
noTxSlot :: Property
noTxSlot = property $ do
  (tip, block) <- forAll $ Gen.evalTxGenState Gen.genNonEmptyBlock
  case tip of
    TipAtGenesis -> pure ()
    (Tip _ blockId blockNo) -> do
      let newSlot = succ $ tipSlot tip
      res <- liftIO $ runEmulatedChainIndex mempty $ do
          appendBlocks [Block tip (map (, def) block)]
          appendBlocks [Block (Tip newSlot blockId blockNo) []]
          tipSlot <$> getTip

      case res of
        Right slot' -> newSlot === slot'
        Left _      -> Hedgehog.assert False

-- | After generating and appending a block in the chain index, verify that
-- querying the chain index with each of the addresses in the block returns
-- unspent 'TxOutRef's.
eachTxOutRefAtAddressShouldBeUnspentSpec :: Property
eachTxOutRefAtAddressShouldBeUnspentSpec = property $ do
  ((tip, block), state) <- forAll $ Gen.runTxGenState Gen.genNonEmptyBlock

  result <- liftIO $ runEmulatedChainIndex mempty $ do
    -- Append the generated block in the chain index
    appendBlocks [Block tip (map (, def) block)]
    utxoSetFromBlockAddrs block

  case result of
    Left _           -> Hedgehog.assert False
    Right utxoGroups -> S.fromList (concat utxoGroups) === view Gen.txgsUtxoSet state

-- | After generating and appending a block in the chain index, verify that
-- querying the chain index with each of the addresses in the block returns
-- unspent 'TxOutRef's with presented 'TxOut's.
eachTxOutRefAtAddressShouldHaveTxOutSpec :: Property
eachTxOutRefAtAddressShouldHaveTxOutSpec = property $ do
  ((tip, block), _) <- forAll $ Gen.runTxGenState Gen.genNonEmptyBlock

  result <- liftIO $ runEmulatedChainIndex mempty $ do
    -- Append the generated block in the chain index
    appendBlocks [Block tip (map (, def) block)]
    utxos <- utxoSetFromBlockAddrs block
    traverse unspentTxOutFromRef (concat utxos)

  case result of
    Left _        -> Hedgehog.assert False
    Right utxouts -> Hedgehog.assert $ all isJust utxouts

-- | After generating and appending a block in the chain index, verify that
-- querying the chain index with each of the asset classes in the block returns
-- unspent 'TxOutRef's.
eachTxOutRefWithCurrencyShouldBeUnspentSpec :: Property
eachTxOutRefWithCurrencyShouldBeUnspentSpec = property $ do
  ((tip, block), state) <- forAll $ Gen.runTxGenState Gen.genNonEmptyBlock

  let assetClasses =
        fmap (\(aid, _) -> fromCardanoAssetId aid)
             $ Value.valueToList
             $ view (traverse . to txOuts . traverse . to citoValue) block

  result <- liftIO $ runEmulatedChainIndex mempty $ do
    -- Append the generated block in the chain index
    appendBlocks [Block tip (map (, def) block)]

    forM assetClasses $ \ac -> do
      let pq = PageQuery 200 Nothing
      UtxosResponse _ utxoRefs <- utxoSetWithCurrency pq ac
      pure $ pageItems utxoRefs

  case result of
    Left _           -> Hedgehog.assert False
    Right utxoGroups -> S.fromList (concat utxoGroups) === view Gen.txgsUtxoSet state

-- | Requesting UTXOs containing Ada should not return anything because every
-- transaction output must have a minimum of 1 ADA. So asking for UTXOs with ADA
-- will always return all UTXOs).
cantRequestForTxOutRefsWithAdaSpec :: Property
cantRequestForTxOutRefsWithAdaSpec = property $ do
  (tip, block) <- forAll $ Gen.evalTxGenState Gen.genNonEmptyBlock

  result <- liftIO $ runEmulatedChainIndex mempty $ do
    -- Append the generated block in the chain index
    appendBlocks [Block tip (map (, def) block)]

    let pq = PageQuery 200 Nothing
    UtxosResponse _ utxoRefs <- utxoSetWithCurrency pq (AssetClass ("", ""))
    pure $ pageItems utxoRefs

  case result of
    Left _         -> Hedgehog.assert False
    Right utxoRefs -> Hedgehog.assert $ null utxoRefs

-- | Do not store txs through BlockProcessOption.
-- The UTxO set must still be stored.
-- But cannot be fetched through addresses as addresses are not stored.
doNotStoreTxs :: Property
doNotStoreTxs = property $ do
  ((tip, block), state) <- forAll $ Gen.runTxGenState Gen.genNonEmptyBlock
  result <- liftIO $ runEmulatedChainIndex mempty $ do
    appendBlocks [Block tip (map (, TxProcessOption{tpoStoreTx=False}) block)]
    tx <- txFromTxId (view citxTxId (head block))
    utxosFromAddr <- utxoSetFromBlockAddrs block
    utxosStored <- traverse utxoSetMembership (S.toList (view Gen.txgsUtxoSet state))
    pure (tx, concat utxosFromAddr, utxosStored)
  case result of
    Right (Nothing, [], utxosStored) -> Hedgehog.assert $ and (isUtxo <$> utxosStored)
    _                                -> Hedgehog.assert False

-- | Run an emulated chain index effect against a starting state
runEmulatedChainIndex
  :: ChainIndexEmulatorState
  -> Eff '[ ChainIndexControlEffect
          , ChainIndexQueryEffect
          , State ChainIndexEmulatorState
          , Error ChainIndexError
          , LogMsg ChainIndexLog
          , IO
          ] a
  -> IO (Either ChainIndexError a)
runEmulatedChainIndex appState effect = do
  r <- effect
    & interpret handleControl
    & interpret handleQuery
    & runState appState
    & runError
    & reinterpret
         (handleLogWriter @ChainIndexLog
                          @(Seq (LogMessage ChainIndexLog)) $ unto pure)
    & runWriter @(Seq (LogMessage ChainIndexLog))
    & runM
  pure $ fmap fst $ fst r
