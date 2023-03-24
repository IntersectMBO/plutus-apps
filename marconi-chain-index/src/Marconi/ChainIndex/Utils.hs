module Marconi.ChainIndex.Utils
    ( isBlockRollbackable
    ) where

import Cardano.Api qualified as C
import Data.Word (Word64)

isBlockRollbackable :: Word64 -> C.BlockNo -> C.ChainTip -> Bool
isBlockRollbackable securityParam (C.BlockNo chainSyncBlockNo) localChainTip =
    let chainTipBlockNo =
            case localChainTip of
              C.ChainTipAtGenesis             -> 0
              (C.ChainTip _ _ (C.BlockNo bn)) -> bn
     -- TODO Need to confirm if it's "<" or "<="
     in chainTipBlockNo - chainSyncBlockNo <= securityParam
