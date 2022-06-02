{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marconi.Orphans where

import Cardano.Api (BlockHeader, BlockNo (BlockNo), ChainPoint (..), ChainTip (ChainTip, ChainTipAtGenesis), Hash,
                    SlotNo (SlotNo), serialiseToRawBytesHexText)
import Prettyprinter (Pretty (pretty), (<+>))

instance Pretty ChainTip where
  pretty ChainTipAtGenesis   = "genesis"
  pretty (ChainTip sn ha bn) = "slotNo" <+> pretty sn <+> "hash" <+> pretty ha <+> "blockNo" <+> pretty bn

instance Pretty ChainPoint where
  pretty ChainPointAtGenesis = "genesis"
  pretty (ChainPoint sn ha)  = "slotNo" <+> pretty sn <+> "hash" <+> pretty ha

instance Pretty (Hash BlockHeader) where
  pretty hash = pretty $ serialiseToRawBytesHexText hash

instance Pretty SlotNo where
  pretty (SlotNo wo) = pretty wo

instance Pretty BlockNo where
  pretty (BlockNo bn) = pretty bn
