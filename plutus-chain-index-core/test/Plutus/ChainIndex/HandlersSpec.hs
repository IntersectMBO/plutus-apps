{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MonoLocalBinds    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Plutus.ChainIndex.HandlersSpec (tests) where

import Control.Concurrent.MVar (newMVar)
import Control.Lens (view)
import Control.Monad (forM)
import Control.Monad.Freer (Eff)
import Control.Monad.Freer.Extras.Beam (BeamEffect)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Tracer (nullTracer)
import Data.Default (def)
import Data.Set qualified as S
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Sqlite qualified as Sqlite
import Database.Beam.Sqlite.Migrate qualified as Sqlite
import Database.SQLite.Simple qualified as Sqlite
import Generators qualified as Gen
import Hedgehog (MonadTest, Property, assert, failure, forAll, property, (===))
import Ledger (outValue)
import Plutus.ChainIndex (Page (pageItems), PageQuery (PageQuery), RunRequirements (..), appendBlock, citxOutputs,
                          runChainIndexEffects, utxoSetWithCurrency)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.DbSchema (checkedSqliteDb)
import Plutus.ChainIndex.Effects (ChainIndexControlEffect, ChainIndexQueryEffect)
import Plutus.ChainIndex.Tx (_ValidTx)
import Plutus.ChainIndex.Types (ChainSyncBlock (..))
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value (AssetClass (AssetClass), flattenValue)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Util (utxoSetFromBlockAddrs)

tests :: TestTree
tests = do
  testGroup "chain-index handlers"
    [ testGroup "utxoSetAtAddress"
      [ testProperty "each txOutRef should be unspent" eachTxOutRefAtAddressShouldBeUnspentSpec
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

  utxoGroups <- runChainIndexTest $ do
      -- Append the generated block in the chain index
      appendBlock (Block tip (map (, def) block))
      utxoSetFromBlockAddrs block

  S.fromList (concat utxoGroups) === view Gen.txgsUtxoSet state

-- | After generating and appending a block in the chain index, verify that
-- querying the chain index with each of the addresses in the block returns
-- unspent 'TxOutRef's.
eachTxOutRefWithCurrencyShouldBeUnspentSpec :: Property
eachTxOutRefWithCurrencyShouldBeUnspentSpec = property $ do
  ((tip, block), state) <- forAll $ Gen.runTxGenState Gen.genNonEmptyBlock

  let assetClasses =
        fmap (\(c, t, _) -> AssetClass (c, t))
             $ filter (\(c, t, _) -> not $ Ada.adaSymbol == c && Ada.adaToken == t)
             $ flattenValue
             $ view (traverse . citxOutputs . _ValidTx . traverse . outValue) block

  utxoGroups <- runChainIndexTest $ do
      -- Append the generated block in the chain index
      appendBlock (Block tip (map (, def) block))

      forM assetClasses $ \ac -> do
        let pq = PageQuery 200 Nothing
        UtxosResponse _ utxoRefs <- utxoSetWithCurrency pq ac
        pure $ pageItems utxoRefs

  S.fromList (concat utxoGroups) === view Gen.txgsUtxoSet state

-- | Requesting UTXOs containing Ada should not return anything because every
-- transaction output must have a minimum of 1 ADA. So asking for UTXOs with ADA
-- will always return all UTXOs).
cantRequestForTxOutRefsWithAdaSpec :: Property
cantRequestForTxOutRefsWithAdaSpec = property $ do
  (tip, block) <- forAll $ Gen.evalTxGenState Gen.genNonEmptyBlock

  utxoRefs <- runChainIndexTest $ do
      -- Append the generated block in the chain index
      appendBlock (Block tip (map (, def) block))

      let pq = PageQuery 200 Nothing
      UtxosResponse _ utxoRefs <- utxoSetWithCurrency pq (AssetClass (Ada.adaSymbol, Ada.adaToken))
      pure $ pageItems utxoRefs

  Hedgehog.assert $ null utxoRefs

-- | Run a chain index test against an in-memory SQLite database.
runChainIndexTest
  :: (MonadTest m
      , MonadIO m)
  => Eff '[ ChainIndexQueryEffect
          , ChainIndexControlEffect
          , BeamEffect
          ] a
  -> m a
runChainIndexTest action = do
  result <- liftIO $ Sqlite.withConnection ":memory:" $ \conn -> do
    Sqlite.runBeamSqlite conn $ autoMigrate Sqlite.migrationBackend checkedSqliteDb
    stateMVar <- newMVar mempty
    runChainIndexEffects (RunRequirements nullTracer stateMVar conn 10) action

  case result of
    Left _  -> Hedgehog.failure
    Right a -> pure a

