{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Orphans where

import Cardano.Api (BlockHeader, BlockNo (BlockNo), ChainPoint (..), ChainTip (ChainTip, ChainTipAtGenesis), Hash,
                    SlotNo (SlotNo), serialiseToRawBytesHexText)
import Prettyprinter (Pretty (pretty), (<+>))

instance Pretty ChainTip where
  pretty ChainTipAtGenesis   = "ChainTipAtGenesis"
  pretty (ChainTip sn ha bn) = "ChainTip(pretty sn <+> "," <+> pretty ha <+> "," <+> pretty bn <> ")"

instance Pretty ChainPoint where
  pretty ChainPointAtGenesis = "ChainPointAtGenesis"
  pretty (ChainPoint sn ha)  = "ChainPoint(pretty sn <+> "," <+> pretty ha <> ")"

instance Pretty (Hash BlockHeader) where
  pretty hash = "BlockHash" <+> pretty (serialiseToRawBytesHexText hash)

instance Pretty SlotNo where
  pretty (SlotNo n) = "Slot" <+> pretty n

instance Pretty BlockNo where
  pretty (BlockNo bn) = "BlockNo" <+> pretty bn
