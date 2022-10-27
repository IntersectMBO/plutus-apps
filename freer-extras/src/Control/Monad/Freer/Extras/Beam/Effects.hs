{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Control.Monad.Freer.Extras.Beam.Effects where

import Cardano.BM.Trace (Trace)
import Control.Monad (guard)
import Control.Monad.Freer (Eff, Member, send, type (~>))
import Control.Monad.Freer.Extras.Beam.Common (BeamLog (..), BeamThreadingArg, BeamableDb)
import Control.Monad.Freer.Extras.Pagination (Page (..), PageQuery (..), PageSize (..))
import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.List.NonEmpty qualified as L
import Data.Maybe (isJust, listToMaybe)
import Database.Beam (Beamable, DatabaseEntity, FromBackendRow, HasQBuilder, Identity, MonadBeam, Q, QExpr, SqlDelete,
                      SqlInsert, SqlSelect, SqlUpdate, TableEntity, asc_, filter_, insertValues, limit_, orderBy_,
                      runDelete, runInsert, runSelectReturningList, runSelectReturningOne, runUpdate, select, val_,
                      (>.))
import Database.Beam.Backend.SQL (BeamSqlBackend, BeamSqlBackendSyntax, HasSqlValueSyntax,
                                  IsSql92ExpressionSyntax (Sql92ExpressionValueSyntax),
                                  IsSql92SelectSyntax (Sql92SelectSelectTableSyntax),
                                  IsSql92SelectTableSyntax (Sql92SelectTableExpressionSyntax),
                                  IsSql92Syntax (Sql92SelectSyntax))
import Database.Beam.Backend.SQL.BeamExtensions (BeamHasInsertOnConflict (anyConflict, insertOnConflict, onConflictDoNothing))

type Synt dbt = (Sql92ExpressionValueSyntax
                  (Sql92SelectTableExpressionSyntax
                    (Sql92SelectSelectTableSyntax
                        (Sql92SelectSyntax
                          (BeamSqlBackendSyntax dbt)))))

data BeamEffect dbt r where
  -- Workaround for "too many SQL variables" sqlite error. Provide a
  -- batch size so that we avoid the error. The maximum is 999.
  AddRowsInBatches
    :: BeamableDb dbt table
    => Int
    -> DatabaseEntity dbt db (TableEntity table)
    -> [table Identity]
    -> BeamEffect dbt ()

  AddRows
    :: BeamableDb dbt table
    => SqlInsert dbt table
    -> BeamEffect dbt ()

  UpdateRows
    :: Beamable table
    => SqlUpdate dbt table
    -> BeamEffect dbt ()

  DeleteRows
    :: Beamable table
    => SqlDelete dbt table
    -> BeamEffect dbt ()

  SelectList
    :: FromBackendRow dbt a
    => SqlSelect dbt a
    -> BeamEffect dbt [a]

  -- | Select using Seek Pagination.
  SelectPage ::
      ( FromBackendRow dbt a
      , HasSqlValueSyntax (Synt dbt) a
      , HasQBuilder dbt
      )
      => PageQuery a
      -> Q dbt db BeamThreadingArg (QExpr dbt BeamThreadingArg a)
      -> BeamEffect dbt (Page a)

  SelectOne
    :: FromBackendRow dbt a
    => SqlSelect dbt a
    -> BeamEffect dbt(Maybe a)

  Combined
    :: [BeamEffect dbt ()]
    -> BeamEffect dbt ()

instance Monoid (BeamEffect dbt ()) where
  mempty = Combined []

instance Semigroup (BeamEffect dbt ()) where
  a <> b = Combined [a, b]

addRowsInBatches ::
    forall dbt table db effs.
    (BeamableDb dbt table, Member (BeamEffect dbt) effs)
    => Int
    -> DatabaseEntity dbt db (TableEntity table)
    -> [table Identity]
    -> Eff effs ()
addRowsInBatches rows dbent tables
    = send @(BeamEffect dbt) (AddRowsInBatches rows dbent tables)

addRows ::
    forall dbt table effs.
    (BeamableDb dbt table, Member (BeamEffect dbt) effs)
    => SqlInsert dbt table
    -> Eff effs ()
addRows op
    = send @(BeamEffect dbt) (AddRows op)


updateRows ::
    forall dbt table effs.
    (Beamable table, Member (BeamEffect dbt) effs)
    => SqlUpdate dbt table
    -> Eff effs ()
updateRows op
    = send @(BeamEffect dbt) (UpdateRows op)


deleteRows ::
    forall dbt table effs.
    (Beamable table, Member (BeamEffect dbt) effs)
    => SqlDelete dbt table
    -> Eff effs ()
deleteRows op
    = send @(BeamEffect dbt) (DeleteRows op)


selectList ::
    forall dbt a effs.
    (FromBackendRow dbt a, Member (BeamEffect dbt) effs)
    => SqlSelect dbt a
    -> Eff effs [a]
selectList op
    = send @(BeamEffect dbt) (SelectList op)


selectPage ::
    forall dbt a db effs.
    ( FromBackendRow dbt a
    , HasSqlValueSyntax (Synt dbt) a
    , Member (BeamEffect dbt) effs
    , HasQBuilder dbt
    )
    => PageQuery a
    -> Q dbt db BeamThreadingArg (QExpr dbt BeamThreadingArg a)
    -> Eff effs (Page a)
selectPage pq query
    = send @(BeamEffect dbt) (SelectPage pq query)


selectOne ::
    forall dbt a effs.
    (FromBackendRow dbt a, Member (BeamEffect dbt) effs)
    => SqlSelect dbt a
    -> Eff effs (Maybe a)
selectOne op
    = send @(BeamEffect dbt) (SelectOne op)


combined ::
    forall dbt effs.
    Member (BeamEffect dbt) effs
    => [(BeamEffect dbt) ()]
    -> Eff effs ()
combined ops
    = send @(BeamEffect dbt) (Combined ops)


handleBeam ::
  forall dbt (dbM :: Type -> Type) effs.
  ( BeamSqlBackend dbt
  , MonadBeam dbt dbM
  , BeamHasInsertOnConflict dbt
  )
  =>(Trace IO BeamLog -> dbM ~> Eff effs)
  -> Trace IO BeamLog
  -> BeamEffect dbt
  ~> Eff effs
handleBeam run trace eff = run trace $ execute eff
  where
    execute :: BeamEffect dbt ~> dbM
    execute = \case
        AddRowsInBatches _ _ [] -> pure ()
        AddRowsInBatches n table (splitAt n -> (batch, rest)) -> do
            runInsert @dbt @dbM
                $ insertOnConflict table (insertValues batch) anyConflict onConflictDoNothing
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
