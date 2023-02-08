{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
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

module Control.Monad.Freer.Extras.Beam where

import Cardano.BM.Data.Tracer (ToObject (..))
import Cardano.BM.Trace (Trace, logDebug)
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throw, try)
import Control.Monad (guard, forM_, void)
import Control.Monad.Freer (Eff, LastMember, Member, type (~>))
import Control.Monad.Freer.Extras.Pagination (Page (..), PageQuery (..), PageSize (..))
import Control.Monad.Freer.Reader (Reader, ask)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.Reader qualified as ReaderT
import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.List.NonEmpty qualified as L
import Data.List qualified as List
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy.Char8 qualified as BL
import Data.DList qualified as D
import Data.Maybe (isJust, listToMaybe)
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified as Text
import Database.Beam (Columnar, Beamable, DatabaseEntity, FromBackendRow, HasSqlEqualityCheck, Identity, MonadIO (liftIO), Q, QBaseScope, QExpr,
                      SqlDelete (..), SqlInsert (..), SqlInsertValues (..), SqlSelect (..), SqlUpdate,
                      TableEntity, asc_, delete, filter_, insertValues, limit_, orderBy_, runDelete, runInsert,
                      runSelectReturningList, runSelectReturningOne, runUpdate, select, val_, (>.), (==.))
import Database.Beam.Backend.SQL (deleteCmd, selectCmd, insertCmd, BeamSqlBackendCanSerialize, HasSqlValueSyntax)
import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict (anyConflict, insertOnConflict, onConflictDoNothing))
import Database.Beam.Query.Internal (QNested)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import Database.Beam.Sqlite (Sqlite, SqliteM (..), runBeamSqliteDebug)
import Database.Beam.Sqlite.Syntax (fromSqliteCommand, fromSqliteExpression, withPlaceholders,
                                    SqliteInsertValuesSyntax (..), SqliteSyntax(..), SqliteValueSyntax)
import Database.SQLite.Simple qualified as Sqlite
import Database.SQLite3 qualified as SBase
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), colon, (<+>))

type BeamableSqlite table = (Beamable table, FieldsFulfillConstraint (BeamSqlBackendCanSerialize Sqlite) table)

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
    :: BeamableSqlite table
    => Int
    -> DatabaseEntity Sqlite db (TableEntity table)
    -> [table Identity]
    -> BeamEffect ()

  AddRows
    :: BeamableSqlite table
    => SqlInsert Sqlite table
    -> BeamEffect ()

  AddRowsPreparedStatement
    :: BeamableSqlite table
    => DatabaseEntity Sqlite db (TableEntity table)
    -> [table Identity]
    -> BeamEffect ()
    -- ^ special insertion using prepared statements
    -- to accelerate insertion at DB level

  UpdateRows
    :: Beamable table
    => SqlUpdate Sqlite table
    -> BeamEffect ()

  DeleteRows
    :: Beamable table
    => SqlDelete Sqlite table
    -> BeamEffect ()

  DeleteRowsInClause
   :: ( FromBackendRow Sqlite value,
        HasSqlEqualityCheck Sqlite value,
        BeamSqlBackendCanSerialize Sqlite value,
        BeamableSqlite table
      )
   => DatabaseEntity Sqlite db (TableEntity table)
   -- ^ table to delete from
   -> (forall f. table f -> Columnar f value)
   -- ^ column to be used for the IN clause
   -> SqlSelect Sqlite value
   -- ^ select statement to be used after IN Clause
   -> BeamEffect ()
   -- ^ To properly handle DELETE statements where
   -- an IN CLAUSE on a SELECT statement is required
   -- Database.Beam currently does not support such construct
   -- Indeed, in_ predicate only accepts a list of values and
   -- not an sql statement. E.g.
   -- DELETE FROM table1
   --  WHERE key IN
   --  ( SELECT snd_key FROM table2
   --    WHERE blabla )
   --
  SelectList
    :: FromBackendRow Sqlite a
    => SqlSelect Sqlite a
    -> BeamEffect [a]

  -- | Select using Seek Pagination.
  SelectPage
      :: (FromBackendRow Sqlite a, HasSqlValueSyntax SqliteValueSyntax a)
      => PageQuery a
      -> Q Sqlite db BeamThreadingArg (QExpr Sqlite BeamThreadingArg a)
      -> BeamEffect (Page a)

  SelectOne
    :: FromBackendRow Sqlite a
    => SqlSelect Sqlite a
    -> BeamEffect (Maybe a)

  SqlCommand :: Sqlite.Query -> BeamEffect ()
  -- ^ mainly to execute sql commands in between sql queries
  -- e.g., DROP INDEX AND CREATE INDEX before insertions, delete or updates
  -- This is necessary on large DB

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
  , Member (Reader (Pool Sqlite.Connection)) effs
  )
  => Trace IO BeamLog
  -> BeamEffect
  ~> Eff effs
handleBeam trace eff = runBeam trace $ execute eff
  where
    execute :: BeamEffect ~> (SqliteM)
    execute = \case
        AddRowsInBatches _ _ [] -> pure ()
        AddRowsInBatches n table (splitAt n -> (batch, rest)) -> do
            runInsert $ insertOnConflict table (insertValues batch) anyConflict onConflictDoNothing
            execute $ AddRowsInBatches n table rest
        AddRows q -> runInsert q
        AddRowsPreparedStatement table values ->
          -- using prepared statements to accelerate insertion
          SqliteM $ do
           (logger, conn) <- ReaderT.ask
           let sqlValues =
                 case insertValues @Sqlite values of
                   SqlInsertValuesEmpty -> []
                   SqlInsertValues (SqliteInsertFromSql _) -> [] -- cannot happen if insertValues is called
                   SqlInsertValues (SqliteInsertExpressions es) -> es
           -- creating a select statement with only the necessary placeholders for one row
           case (insertOnConflict table (insertValues (take 1 values)) anyConflict onConflictDoNothing) of
             (SqlInsert _ i) -> do
               let (SqliteSyntax instCmd _) = fromSqliteCommand (insertCmd i)
               let instCmdString = BL.unpack (toLazyByteString (withPlaceholders instCmd))
               let toParams e =
                     let (SqliteSyntax _ vals') = fromSqliteExpression e
                     in D.toList vals'
               let paramRows = map (\row -> (foldMap toParams row)) sqlValues
               liftIO $ do
                 Sqlite.withStatement conn (fromString instCmdString) $ \stmt@(Sqlite.Statement pstmt) -> do
                   forM_ paramRows $ \params -> do
                     logger $ "Prepared Statement: " ++ instCmdString ++ ";\n -- With values: " ++ show params
                     Sqlite.withBind stmt (Sqlite.toRow params) (void . SBase.step $ pstmt )
             _ ->
               -- cannot happen if we use insertOnConflict : do nothing
               pure ()

        UpdateRows q -> runUpdate q
        DeleteRows q -> runDelete q
        DeleteRowsInClause table getKey (SqlSelect s) ->
          SqliteM $ do
           (logger, conn) <- ReaderT.ask
           -- introducing fake equality to be replaced in command afterwards
           let (SqlDelete _ d) = delete table (\row -> getKey row ==. getKey row)
           let (SqliteSyntax delCmd _) = fromSqliteCommand $ deleteCmd d
           let (SqliteSyntax selCmd vals) = fromSqliteCommand $ selectCmd s
           let delCmdString = BL.unpack (toLazyByteString (withPlaceholders delCmd))
           let selCmdString = BL.unpack (toLazyByteString (withPlaceholders selCmd))
           let dropDummyEquality [] = []
               dropDummyEquality str@(c:cs)
                 | "=" `List.isPrefixOf` str = []
                 | otherwise = c : dropDummyEquality cs
           let cmdString = (dropDummyEquality delCmdString) ++ " IN ( " ++ selCmdString ++ " )"
           liftIO $ do
             logger $ cmdString ++ ";\n-- With values: " ++ show (D.toList vals)
             Sqlite.execute conn (fromString cmdString) (D.toList vals)
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
        SelectOne  q  -> runSelectReturningOne q
        SqlCommand q -> runSqlCommand q
        Combined   effs -> traverse_ execute effs

    runSqlCommand :: Sqlite.Query -> SqliteM ()
    runSqlCommand q =
      SqliteM $ do
       (logger, conn) <- ReaderT.ask
       liftIO $ do
         logger $ show q
         Sqlite.execute_ conn q

runBeam ::
  forall effs.
  ( LastMember IO effs
  , Member (Reader (Pool Sqlite.Connection)) effs
  )
  => Trace IO BeamLog
  -> SqliteM
  ~> Eff effs
runBeam trace action = do
  pool <- ask @(Pool Sqlite.Connection)
  liftIO $ Pool.withResource pool $ \conn -> loop conn ( 5 :: Int )
  where
    loop conn retries = do
      let traceSql = logDebug trace . SqlLog
      resultEither <- try $ Sqlite.withTransaction conn $ runBeamSqliteDebug traceSql conn action
      case resultEither of
          -- 'Database.SQLite.Simple.ErrorError' corresponds to an SQL error or
          -- missing database. When this exception is raised, we suppose it's
          -- because the another transaction was already running.
          Left (Sqlite.SQLError Sqlite.ErrorError _ _) | retries > 0 -> do
              threadDelay 100_000
              loop conn (retries - 1)
          -- We handle and rethrow errors other than
          -- 'Database.SQLite.Simple.ErrorError'.
          Left e -> throw $ SqlError $ Text.pack $ show e
          Right v -> return v

makeEffect ''BeamEffect
