{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ImpredicativeTypes   #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# options_ghc -Wno-missing-signatures #-}

module Control.Monad.Freer.Extras.BeamSpec (tests) where

import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Freer (Eff, Member, interpret, runM)
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Extras.Beam (BeamError)
import Control.Monad.Freer.Extras.Beam.Effects (BeamEffect, handleBeam, selectPage)
import Control.Monad.Freer.Extras.Beam.Sqlite (runBeam)
import Control.Monad.Freer.Extras.Pagination (Page (..), PageQuery (..))
import Control.Monad.Freer.Reader (Reader, runReader)
import Control.Tracer (nullTracer)
import Data.Int (Int16)
import Data.Kind (Constraint)
import Data.List (sort)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set qualified as Set
import Database.Beam (Beamable, Columnar, Database, DatabaseSettings, FromBackendRow, Generic, MonadIO (liftIO), Q,
                      QBaseScope, QExpr, Table (..), TableEntity, all_, dbModification, insertValues, runInsert,
                      withDbModification)
import Database.Beam.Backend.SQL (HasSqlValueSyntax)
import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict (anyConflict, insertOnConflict, onConflictDoNothing))
import Database.Beam.Migrate (CheckedDatabaseSettings, defaultMigratableDbSettings, renameCheckedEntity,
                              unCheckDatabase)
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Query.Internal (QNested)
import Database.Beam.Sqlite (Sqlite)
import Database.Beam.Sqlite qualified as Sqlite
import Database.Beam.Sqlite.Migrate qualified as Sqlite
import Database.Beam.Sqlite.Syntax (SqliteValueSyntax)
import Database.SQLite.Simple qualified as Sqlite

import Data.Maybe (listToMaybe)
import Data.Set (Set)
import Hedgehog (Property, PropertyT, assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Gen
import Test.Tasty
import Test.Tasty.Hedgehog (testProperty)

newtype TestRowT f = IntegerRow
    { _testRowInt :: Columnar f Int16
    } deriving (Generic)
      deriving anyclass (Beamable)

instance Table TestRowT where
    data PrimaryKey TestRowT f = TestRowId (Columnar f Int16) deriving (Generic, Beamable)
    primaryKey = TestRowId . _testRowInt

newtype Db f = Db
    { testRows   :: f (TableEntity TestRowT)
    } deriving (Generic)
      deriving anyclass (Database be)

type AllTables (c :: * -> Constraint) f =
    ( c (f (TableEntity TestRowT))
    )
deriving via (GenericSemigroupMonoid (Db f)) instance AllTables Semigroup f => Semigroup (Db f)
deriving via (GenericSemigroupMonoid (Db f)) instance AllTables Monoid f => Monoid (Db f)

db :: DatabaseSettings Sqlite Db
db = unCheckDatabase checkedSqliteDb

checkedSqliteDb :: CheckedDatabaseSettings Sqlite Db
checkedSqliteDb = defaultMigratableDbSettings
    `withDbModification` dbModification
    { testRows   = renameCheckedEntity (const "test")
    }

tests :: TestTree
tests = do
  testGroup "db store"
    [ testGroup "select page"
        [ testProperty "size of pageItems of all pages should be less or equal than total number of items in database"
                       pageItemsSizeLessOrEqualGenItemsSizeSpec

        , testProperty "last page should have no next page"
                       lastPageShouldHaveNoNextPageQuerySpec
        , testProperty "concat items for all pages should be the same as generated items"
                       pageItemsEqualGenItemsSpec
        , testProperty "page items should be sorted in ascending order"
                       pageItemsSortedAscOrderSpec
        , testProperty "page size equal to total number of items in db should return a single page"
                       pageSizeEqualToTotalItemsSizeShouldReturnOnePage
        ]
    ]

-- | The length of field 'pageItems' should be less or equal than total number
-- of items in database.
pageItemsSizeLessOrEqualGenItemsSizeSpec :: Property
pageItemsSizeLessOrEqualGenItemsSizeSpec = property $ do
  items <- forAll $ Gen.set (Gen.linear 1 10) $ Gen.int (Gen.linear 0 100)
  pageSize <- forAll $ Gen.int (Gen.linear 1 20)
  let q = fmap _testRowInt $ all_ (testRows db)
  runBeamEffectInGenTestDb
    items
    ( selectAllPages (PageQuery (fromIntegral pageSize) Nothing) q
    )
    $ \pages ->
      forM_ pages $ \page ->
        Hedgehog.assert $ length (pageItems page) <= length items

-- | The last 'Page' should have a number of page items less or equal than
-- requested 'PageSize' and it's 'nextPageQuery' field should be Nothing.
lastPageShouldHaveNoNextPageQuerySpec :: Property
lastPageShouldHaveNoNextPageQuerySpec = property $ do
  items <- forAll $ Gen.set (Gen.linear 1 10) $ Gen.int (Gen.linear 0 100)
  pageSize <- forAll $ Gen.int (Gen.linear 1 20)
  let q = fmap _testRowInt $ all_ (testRows db)
  runBeamEffectInGenTestDb
    items
    (selectAllPages (PageQuery (fromIntegral pageSize) Nothing) q)
    $ \pages -> do
      case listToMaybe $ reverse pages of
        Nothing -> Hedgehog.assert False
        Just lastPage -> do
          Hedgehog.assert $ length (pageItems lastPage) <= pageSize
          nextPageQuery lastPage === Nothing

-- | Concatenating all 'pageItems' of all 'Page's should be equal to the
-- generated input list.
pageItemsEqualGenItemsSpec :: Property
pageItemsEqualGenItemsSpec = property $ do
  items <- forAll $ Gen.set (Gen.linear 1 10) $ Gen.int (Gen.linear 0 100)
  let q = fmap _testRowInt $ all_ (testRows db)
  runBeamEffectInGenTestDb
    items
    (selectAllPages (PageQuery 1 Nothing) q)
    $ \pages ->
      Set.fromList (concatMap pageItems pages) === Set.fromList (fromIntegral <$> Set.toList items)

-- | The elemens in all 'pageItems' of all 'Page's should be sorted in
-- ascending order.
pageItemsSortedAscOrderSpec :: Property
pageItemsSortedAscOrderSpec = property $ do
  items <- forAll $ Gen.set (Gen.linear 1 10) $ Gen.int (Gen.linear 0 100)
  let q = fmap _testRowInt $ all_ (testRows db)
  runBeamEffectInGenTestDb
    items
    (selectAllPages (PageQuery 1 Nothing) q)
    $ \pages -> do
      let allPageItems = concatMap pageItems pages
      allPageItems === sort allPageItems

pageSizeEqualToTotalItemsSizeShouldReturnOnePage :: Property
pageSizeEqualToTotalItemsSizeShouldReturnOnePage = property $ do
  items <- forAll $ Gen.set (Gen.linear 1 10) $ Gen.int (Gen.linear 0 100)
  let q = fmap _testRowInt $ all_ (testRows db)
  runBeamEffectInGenTestDb
    items
    (selectAllPages (PageQuery (fromIntegral $ length items) Nothing) q)
    $ \pages -> length pages === 1

selectAllPages
    :: forall a effs db.
    ( FromBackendRow Sqlite a
    , HasSqlValueSyntax SqliteValueSyntax a
    , Member (BeamEffect Sqlite) effs
    )
    => PageQuery a
    -> Q Sqlite db
                (QNested (QNested QBaseScope))
                (QExpr Sqlite (QNested (QNested QBaseScope)) a)
    -> Eff effs [Page a]
selectAllPages pq q = do
  page <- selectPage pq q
  case nextPageQuery page of
    Nothing -> pure [page]
    Just newPageQuery -> do
      nextPages <- selectAllPages newPageQuery q
      pure $ page : nextPages

runBeamEffectInGenTestDb
    :: Set Int
    -> Eff '[BeamEffect Sqlite, Error BeamError, Reader (Pool Sqlite.Connection), IO] a
    -> (a -> PropertyT IO ())
    -> PropertyT IO ()
runBeamEffectInGenTestDb items effect runTest = do
  pool <- liftIO $ Pool.newPool Pool.PoolConfig
    { Pool.createResource = Sqlite.open  ":memory:"
    , Pool.freeResource = Sqlite.close
    , Pool.poolCacheTTL = 1_000_000
    , Pool.poolMaxResources = 1
    }
  result <- liftIO $ do
    Pool.withResource pool $ \conn -> Sqlite.runBeamSqlite conn $ do
      autoMigrate Sqlite.migrationBackend checkedSqliteDb
      runInsert $ insertOnConflict (testRows db) (insertValues $ IntegerRow . fromIntegral <$> Set.toList items) anyConflict onConflictDoNothing
    runBeamEffect pool effect

  case result of
    Left _  -> Hedgehog.assert False
    Right r -> runTest r

runBeamEffect
    :: Pool Sqlite.Connection
    -> Eff '[BeamEffect Sqlite, Error BeamError, Reader (Pool Sqlite.Connection), IO] a
    -> IO (Either BeamError a)
runBeamEffect pool effect = do
  effect
    & interpret (handleBeam runBeam nullTracer)
    & runError
    & runReader pool
    & runM
