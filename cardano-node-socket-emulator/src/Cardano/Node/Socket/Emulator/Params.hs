{-# LANGUAGE NamedFieldPuns #-}
module Cardano.Node.Socket.Emulator.Params where

import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Node.Emulator.Internal.Node.Params
import Cardano.Node.Socket.Emulator.Types
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Default (def)

fromNodeServerConfig :: NodeServerConfig -> IO Params
fromNodeServerConfig NodeServerConfig{nscSlotConfig, nscNetworkId, nscProtocolParametersJsonPath} = do
  protocolParameters <- readProtocolParameters nscProtocolParametersJsonPath
  pure $ paramsWithProtocolsParameters nscSlotConfig protocolParameters nscNetworkId

readProtocolParameters :: Maybe FilePath -> IO ProtocolParameters
readProtocolParameters = maybe (pure def) readPP
  where
    readPP path = do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err -> error $ "Error reading protocol parameters JSON file: "
                         ++ show path ++ " (" ++ err ++ ")"
        Right params -> pure params
