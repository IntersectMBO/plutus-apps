{-# LANGUAGE NamedFieldPuns #-}
module Cardano.Node.Params where

import Cardano.Api.NetworkId.Extra (NetworkIdWrapper (..))
import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Node.Types
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy qualified as BSL
import Data.Default (def)
import Ledger.Params

fromPABServerConfig :: PABServerConfig -> IO Params
fromPABServerConfig PABServerConfig{pscSlotConfig, pscNetworkId, pscProtocolParametersJsonPath} = do
  let NetworkIdWrapper networkId = pscNetworkId
  protocolParameters <- readProtocolParameters pscProtocolParametersJsonPath
  pure $ Params
    { pSlotConfig     = pscSlotConfig
    , pProtocolParams = protocolParameters
    , pNetworkId      = networkId
    }

readProtocolParameters :: Maybe FilePath -> IO ProtocolParameters
readProtocolParameters = maybe (pure def) readPP
  where
    readPP path = do
      bs <- BSL.readFile path
      case eitherDecode bs of
        Left err -> error $ "Error reading protocol parameters JSON file: "
                         ++ show path ++ " (" ++ err ++ ")"
        Right params -> pure params
