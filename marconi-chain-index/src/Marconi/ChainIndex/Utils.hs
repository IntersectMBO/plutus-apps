module Marconi.ChainIndex.Utils
    ( isBlockRollbackable
    , querySecurityParam
    ) where

import Cardano.Api qualified as C
import Cardano.Streaming.Helpers qualified as C
import Control.Monad.IO.Class (liftIO)
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)

isBlockRollbackable :: Int -> C.BlockNo -> C.ChainTip -> Bool
isBlockRollbackable securityParam (C.BlockNo chainSyncBlockNo) localChainTip =
    let chainTipBlockNo =
            case localChainTip of
              C.ChainTipAtGenesis             -> 0
              (C.ChainTip _ _ (C.BlockNo bn)) -> bn
     -- TODO Need to confirm if it's "<" or "<="
     in chainTipBlockNo - chainSyncBlockNo <= toEnum securityParam

-- | Query security param from node
querySecurityParam :: C.NetworkId -> FilePath -> IO Int
querySecurityParam networkId socketPath = do
  result <- liftIO $ C.queryNodeLocalState localNodeConnectInfo Nothing queryInMode
  return $ C.protocolParamSecurity $ either showError (either showError id) result

  where
    localNodeConnectInfo :: C.LocalNodeConnectInfo C.CardanoMode
    localNodeConnectInfo = C.mkLocalNodeConnectInfo networkId socketPath

    eraInMode :: C.EraInMode C.BabbageEra C.CardanoMode
    eraInMode = C.BabbageEraInCardanoMode

    queryInMode :: C.QueryInMode C.CardanoMode (Either EraMismatch C.GenesisParameters)
    queryInMode = C.QueryInEra eraInMode
      $ C.QueryInShelleyBasedEra (C.shelleyBasedEra @C.BabbageEra) C.QueryGenesisParameters

    -- TODO /Really/ handle the error.
    showError :: Show a => a -> b
    showError = error . show
