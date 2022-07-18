{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

module Control.Monad.Freer.Extras.Beam.Common where

import Cardano.BM.Data.Tracer (ToObject (..))
import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.Beam (Beamable, QBaseScope)
import Database.Beam.Backend (BeamSqlBackendCanSerialize)
import Database.Beam.Query.Internal (QNested)
import Database.Beam.Schema.Tables (FieldsFulfillConstraint)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (..), colon, (<+>))

type BeamableDb db table = (Beamable table, FieldsFulfillConstraint (BeamSqlBackendCanSerialize db) table)


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
