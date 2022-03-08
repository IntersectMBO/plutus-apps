module Cardano.Api.Lens where

-- import Control.Lens
-- import Control.Lens.Traversal
-- import Control.Lens.Combinators
-- import Cardano.Api

-- makePrisms ''Block
-- makePrisms ''EraInMode

-- _BlockHeader :: Getter (Block era) BlockHeader
-- _BlockHeader f b =
--   let g = \case (Block header _) -> header in
--   contramap g (f $ g b)

-- _Tx :: Traversal' (Block era) (Tx era)
-- _Tx f bl = _wa

-- makePrisms ''Tx
-- makePrisms ''TxBodyContent
