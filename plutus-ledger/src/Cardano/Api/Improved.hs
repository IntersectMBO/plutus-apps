{-# LANGUAGE GADTs #-}

module Cardano.Api.Improved where

import Cardano.Api

data ImprovedBlockInMode mode where
  ImprovedBlockInMode :: IsCardanoEra era => Block era -> EraInMode era mode -> ImprovedBlockInMode mode

improveBlockInMode :: BlockInMode CardanoMode -> ImprovedBlockInMode CardanoMode
improveBlockInMode (BlockInMode bl eim@ByronEraInCardanoMode)   = ImprovedBlockInMode bl eim
improveBlockInMode (BlockInMode bl eim@ShelleyEraInCardanoMode) = ImprovedBlockInMode bl eim
improveBlockInMode (BlockInMode bl eim@AllegraEraInCardanoMode) = ImprovedBlockInMode bl eim
improveBlockInMode (BlockInMode bl eim@MaryEraInCardanoMode)    = ImprovedBlockInMode bl eim
improveBlockInMode (BlockInMode bl eim@AlonzoEraInCardanoMode)  = ImprovedBlockInMode bl eim
