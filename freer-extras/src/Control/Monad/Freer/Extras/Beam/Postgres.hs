{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Control.Monad.Freer.Extras.Beam.Postgres where

import Cardano.BM.Data.Tracer (ToObject (..))
import Cardano.BM.Trace (Trace, logDebug)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throw, try)
import Control.Monad (guard)
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Extras.Pagination (Page (..), PageQuery (..), PageSize (..))
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.TH (makeEffect)
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as L
import Data.Maybe (isJust, listToMaybe)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Beam (Beamable, DatabaseEntity, FromBackendRow, Identity, MonadIO (liftIO), Q, QBaseScope, QExpr,
                      SqlDelete, SqlInsert, SqlSelect, SqlUpdate, TableEntity, asc_, filter_, insertValues, limit_,
                      orderBy_, runDelete, runInsert, runSelectReturningList, runSelectReturningOne, runUpdate, select,
                      val_, (>.))
import Database.Beam.Backend.SQL (BeamSqlBackendCanSerialize, HasSqlValueSyntax)
import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict (anyConflict, insertOnConflict, onConflictDoNothing))
import Database.Beam.Postgres (Connection, Pg, Postgres, runBeamPostgresDebug)
import Database.Beam.Postgres.Syntax (PgValueSyntax)
import Database.Beam.Query.Internal (QNested)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Database.PostgreSQL.Simple qualified as Postgres
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), colon, (<+>))

type BeamablePostgres table = (Beamable table, FieldsFulfillConstraint (BeamSqlBackendCanSerialize Postgres) table)
type BeamableSqlite table = BeamablePostgres table

type BeamThreadingArg = QNested (QNested QBaseScope)

newtype BeamError =
  SqlError Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToObject)

instance Exception BeamError

instance Pretty BeamError where
  pretty = \case
    SqlError s -> "SqlError (via Beam)" <> colon <+> pretty s

newtype BeamLog =
  SqlLog String
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, ToObject)

instance Pretty BeamLog where
  pretty = \case
    SqlLog s -> "SqlLog" <> colon <+> pretty s

data BeamEffect r where
  -- Workaround for "too many SQL variables" sqlite error. Provide a
  -- batch size so that we avoid the error. The maximum is 999.
  AddRowsInBatches
    :: BeamablePostgres table
    => Int
    -> DatabaseEntity Postgres db (TableEntity table)
    -> [table Identity]
    -> BeamEffect ()

  AddRows
    :: BeamablePostgres table
    => SqlInsert Postgres table
    -> BeamEffect ()

  UpdateRows
    :: Beamable table
    => SqlUpdate Postgres table
    -> BeamEffect ()

  DeleteRows
    :: Beamable table
    => SqlDelete Postgres table
    -> BeamEffect ()

  SelectList
    :: FromBackendRow Postgres a
    => SqlSelect Postgres a
    -> BeamEffect [a]

  -- | Select using Seek Pagination.
  SelectPage
      :: (FromBackendRow Postgres a, HasSqlValueSyntax PgValueSyntax a)
      => PageQuery a
      -> Q Postgres db BeamThreadingArg (QExpr Postgres BeamThreadingArg a)
      -> BeamEffect (Page a)

  SelectOne
    :: FromBackendRow Postgres a
    => SqlSelect Postgres a
    -> BeamEffect (Maybe a)

  Combined
    :: [BeamEffect ()]
    -> BeamEffect ()

instance Monoid (BeamEffect ()) where
  mempty = Combined []

instance Semigroup (BeamEffect ()) where
  a <> b = Combined [a, b]

handleBeam ::
  forall effs.
  ( LastMember IO effs
  , Member (Reader (Pool Connection)) effs
  )
  => Trace IO BeamLog
  -> BeamEffect
  ~> Eff effs
handleBeam trace eff = runBeam trace $ execute eff
  where
    execute :: BeamEffect ~> Pg
    execute = \case
        AddRowsInBatches _ _ [] -> pure ()
        AddRowsInBatches n table (splitAt n -> (batch, rest)) -> do
            runInsert $ insertOnConflict table (insertValues batch) anyConflict onConflictDoNothing
            execute $ AddRowsInBatches n table rest
        AddRows    q    -> runInsert q
        UpdateRows q    -> runUpdate q
        DeleteRows q    -> runDelete q
        SelectList q    -> runSelectReturningList q
        SelectPage pageQuery@PageQuery { pageQuerySize = PageSize ps, pageQueryLastItem } q -> do
          let ps' = fromIntegral ps

          -- Fetch the first @PageSize + 1@ elements after the last query
          -- element. The @+1@ allows to us to know if there is a next page
          -- or not.
          items <- runSelectReturningList
                    $ select
                    $ limit_ (ps' + 1)
                    $ orderBy_ asc_
                    $ filter_ (\qExpr -> maybe (val_ True)
                                              (\lastItem -> qExpr >. val_ lastItem)
                                              pageQueryLastItem
                              ) q

          let lastItemM = guard (length items > fromIntegral ps)
                       >> L.nonEmpty items
                       >>= listToMaybe . L.tail . L.reverse
          let newPageQuery = fmap (PageQuery (PageSize ps) . Just) lastItemM

          pure $
            Page
                { currentPageQuery = pageQuery
                , nextPageQuery = newPageQuery
                , pageItems = if isJust lastItemM then init items else items
                }
        SelectOne  q    -> runSelectReturningOne  q
        Combined   effs -> traverse_ execute effs

runBeam ::
  forall effs.
  ( LastMember IO effs
  , Member (Reader (Pool Connection)) effs
  )
  => Trace IO BeamLog
  -> Pg
  ~> Eff effs
runBeam trace action = do
  pool <- ask @(Pool Postgres.Connection)
  liftIO $ Pool.withResource pool $ \conn -> loop conn ( 5 :: Int )
  where
    loop conn retries = do
      let traceSql = logDebug trace . SqlLog
      resultEither <- try $ Postgres.withTransaction conn $ runBeamPostgresDebug traceSql conn action
      case resultEither of
          -- 'Database.SQLite.Simple.ErrorError' corresponds to an SQL error or
          -- missing database. When this exception is raised, we suppose it's
          -- because the another transaction was already running.
          Left (Postgres.SqlError _ _ _ _ _) | retries > 0 -> do
              threadDelay 100_000
              loop conn (retries - 1)
          -- We handle and rethrow errors other than
          -- 'Database.SQLite.Simple.ErrorError'.
          Left e -> throw $ SqlError $ Text.pack $ show e
          Right v -> return v

makeEffect ''BeamEffect
