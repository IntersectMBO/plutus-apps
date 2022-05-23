{-# OPTIONS_GHC -Wno-orphans #-}

import Cardano.Api (Block (Block), BlockHeader (BlockHeader), BlockInMode (BlockInMode), CardanoMode,
                    ChainPoint (ChainPoint), Env,
                    FoldBlocksError (FoldBlocksApplyBlockError, FoldBlocksInitialLedgerStateError),
                    GenesisConfigError (NEAlonzoConfig, NEByronConfig, NECardanoConfig, NEError, NEShelleyConfig),
                    InitialLedgerStateError (ILSEConfigFile, ILSEGenesisFile, ILSELedgerConsensusConfig), LedgerEvent,
                    LedgerState, ValidationMode (FullValidation), foldBlocks)
import Control.Monad.Except (runExceptT)
import Data.Time.LocalTime (getZonedTime)

deriving instance Show InitialLedgerStateError

deriving instance Show FoldBlocksError

deriving instance Show GenesisConfigError

main :: IO ()
main = do
  r <-
    runExceptT $
      foldBlocks
        "/home/andrea/work/cardano-mainnet/config/mainnet-config.json"
        "/home/andrea/work/cardano-mainnet/socket/node.socket"
        FullValidation
        ()
        myfold
  print r

myfold ::
  Env ->
  LedgerState ->
  [LedgerEvent] ->
  BlockInMode CardanoMode ->
  () ->
  IO ()
myfold _env _ls _le (BlockInMode blk _eim) _ = do
  t <- getZonedTime
  putStrLn $ show t <> " " <> show cp
  where
    (Block (BlockHeader sn ha _bn) _) = blk
    cp = ChainPoint sn ha
