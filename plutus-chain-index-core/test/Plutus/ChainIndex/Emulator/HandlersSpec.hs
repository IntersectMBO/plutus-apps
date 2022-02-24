{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Plutus.ChainIndex.Emulator.HandlersSpec (tests) where

import Control.Lens
import Control.Monad (forM)
import Control.Monad.Freer (Eff, interpret, reinterpret, runM)
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Extras (LogMessage, LogMsg (..), handleLogWriter)
import Control.Monad.Freer.State (State, runState)
import Control.Monad.Freer.Writer (runWriter)
import Control.Monad.IO.Class (liftIO)
import Data.Default (def)
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import Data.Set qualified as S
import Generators qualified as Gen
import Ledger (outValue)
import Plutus.ChainIndex (ChainIndexLog, Page (pageItems), PageQuery (PageQuery), appendBlocks, unspentTxOutFromRef,
                          utxoSetWithCurrency)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.ChainIndexError (ChainIndexError)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect, ChainIndexQueryEffect)
import Plutus.ChainIndex.Emulator.Handlers (ChainIndexEmulatorState, handleControl, handleQuery)
import Plutus.ChainIndex.Tx (_ValidTx, citxOutputs)
import Plutus.ChainIndex.Types (ChainSyncBlock (..))
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), flattenValue)

import Hedgehog (Property, assert, forAll, property, (===))
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)
import Util (utxoSetFromBlockAddrs)

tests :: TestTree
tests = do
  testGroup "chain index emulator handlers"
    [ testGroup "utxoSetAtAddress"
      [ testProperty "each txOutRef should be unspent" eachTxOutRefAtAddressShouldBeUnspentSpec
      ]
    , testGroup "unspentTxOutFromRef"
      [ testProperty "get unspent tx out from ref" eachTxOutRefAtAddressShouldHaveTxOutSpec
      ]
    , testGroup "utxoSetWithCurrency"
      [ testProperty "each txOutRef should be unspent" eachTxOutRefWithCurrencyShouldBeUnspentSpec
      , testProperty "should restrict to non-ADA currencies" cantRequestForTxOutRefsWithAdaSpec
      ]
    ]

-- | After generating and appending a block in the chain index, verify that
-- querying the chain index with each of the addresses in the block returns
-- unspent 'TxOutRef's.
eachTxOutRefAtAddressShouldBeUnspentSpec :: Property
eachTxOutRefAtAddressShouldBeUnspentSpec = property $ do
  ((tip, block), state) <- forAll $ Gen.runTxGenState Gen.genNonEmptyBlock

  result <- liftIO $ runEmulatedChainIndex mempty $ do
    -- Append the generated block in the chain index
    appendBlocks [(Block tip (map (, def) block))]
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
    appendBlocks [(Block tip (map (, def) block))]
    utxos <- utxoSetFromBlockAddrs block
    traverse unspentTxOutFromRef (concat utxos)

  case result of
    Left _        -> Hedgehog.assert False
    Right utxouts -> Hedgehog.assert $ and $ map isJust utxouts

-- | After generating and appending a block in the chain index, verify that
-- querying the chain index with each of the asset classes in the block returns
-- unspent 'TxOutRef's.
eachTxOutRefWithCurrencyShouldBeUnspentSpec :: Property
eachTxOutRefWithCurrencyShouldBeUnspentSpec = property $ do
  ((tip, block), state) <- forAll $ Gen.runTxGenState Gen.genNonEmptyBlock

  let assetClasses =
        fmap (\(c, t, _) -> AssetClass (c, t))
             $ flattenValue
             $ view (traverse . citxOutputs . _ValidTx . traverse . outValue) block

  result <- liftIO $ runEmulatedChainIndex mempty $ do
    -- Append the generated block in the chain index
    appendBlocks [(Block tip (map (, def) block))]

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
    appendBlocks [(Block tip (map (, def) block))]

    let pq = PageQuery 200 Nothing
    UtxosResponse _ utxoRefs <- utxoSetWithCurrency pq (AssetClass ("", ""))
    pure $ pageItems utxoRefs

  case result of
    Left _         -> Hedgehog.assert False
    Right utxoRefs -> Hedgehog.assert $ null utxoRefs

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
