
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}

module Ledger.Contexts.Orphans where
import Plutus.V1.Ledger.Contexts (ScriptPurpose (..))

deriving stock instance Ord ScriptPurpose
