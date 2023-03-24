{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-| This module is a duplication of functionality from the `Cardano.Api.LedgerState`
module in the `cardano-api` Haskell project which is not exposed .

TODO: If `cardano-api` ever exposes this module, then we should use that.
-}
module Marconi.ChainIndex.Node.Client.GenesisConfig where

import Cardano.Chain.Genesis qualified as Byron
import Cardano.Chain.Update qualified as Byron
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Crypto.Hashing (decodeAbstractHash)
import Cardano.Crypto.ProtocolMagic (RequiresNetworkMagic)
import Cardano.Ledger.Alonzo.Genesis qualified as Ledger
import Cardano.Ledger.Shelley.API qualified as Ledger
import Control.Exception (IOException, catch)
import Control.Monad (when)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except)
import Control.Monad.Trans.Except.Extra (firstExceptT, handleIOExceptT, hoistEither, left)
import Data.Aeson as Aeson (FromJSON (parseJSON), Object, eitherDecodeStrict', withObject, (.:), (.:?))
import Data.Aeson.Types (Parser)
import Data.ByteString as BS (ByteString, readFile)
import Data.ByteString.Base16 qualified as Base16
import Data.Foldable (asum)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Yaml qualified as Yaml
import Ouroboros.Consensus.Cardano qualified as Consensus
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Block qualified as HFC
import Ouroboros.Consensus.Cardano.Node qualified as Consensus
import Ouroboros.Consensus.Ledger.Extended qualified as Ledger
import Ouroboros.Consensus.Mempool.TxLimits qualified as TxLimits
import Ouroboros.Consensus.Node qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Translate ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Consensus.Shelley.Node.Praos qualified as Consensus
import System.FilePath (takeDirectory, (</>))

-- Usually only one constructor, but may have two when we are preparing for a HFC event.
data GenesisConfig
  = GenesisCardano
      !NodeConfig
      !Byron.Config
      !ShelleyConfig
      !Ledger.AlonzoGenesis

data ShelleyGenesisError
     = ShelleyGenesisReadError !FilePath !Text
     | ShelleyGenesisHashMismatch !GenesisHashShelley !GenesisHashShelley -- actual, expected
     | ShelleyGenesisDecodeError !FilePath !Text
     deriving Show

data AlonzoGenesisError
     = AlonzoGenesisReadError !FilePath !Text
     | AlonzoGenesisHashMismatch !GenesisHashAlonzo !GenesisHashAlonzo -- actual, expected
     | AlonzoGenesisDecodeError !FilePath !Text
     deriving Show

renderAlonzoGenesisError :: AlonzoGenesisError -> Text
renderAlonzoGenesisError sge =
    case sge of
      AlonzoGenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      AlonzoGenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Alonzo genesis file: the actual hash is ", renderHash (unGenesisHashAlonzo actual)
          , ", but the expected Alonzo genesis hash given in the node "
          , "configuration file is ", renderHash (unGenesisHashAlonzo expected), "."
          ]

      AlonzoGenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

renderShelleyGenesisError :: ShelleyGenesisError -> Text
renderShelleyGenesisError sge =
    case sge of
      ShelleyGenesisReadError fp err ->
        mconcat
          [ "There was an error reading the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

      ShelleyGenesisHashMismatch actual expected ->
        mconcat
          [ "Wrong Shelley genesis file: the actual hash is ", renderHash (unGenesisHashShelley actual)
          , ", but the expected Shelley genesis hash given in the node "
          , "configuration file is ", renderHash (unGenesisHashShelley expected), "."
          ]

      ShelleyGenesisDecodeError fp err ->
        mconcat
          [ "There was an error parsing the genesis file: ", Text.pack fp
          , " Error: ", err
          ]

renderHash :: Crypto.Hash Crypto.Blake2b_256 ByteString -> Text
renderHash h = Text.decodeUtf8 $ Base16.encode (Crypto.hashToBytes h)

data ShelleyConfig = ShelleyConfig
  { scConfig      :: !(Ledger.ShelleyGenesis Consensus.StandardShelley)
  , scGenesisHash :: !GenesisHashShelley
  }

newtype GenesisFile = GenesisFile
  { unGenesisFile :: FilePath
  } deriving Show

newtype GenesisHashByron = GenesisHashByron
  { unGenesisHashByron :: Text
  } deriving newtype (Eq, Show)

newtype GenesisHashShelley = GenesisHashShelley
  { unGenesisHashShelley :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype GenesisHashAlonzo = GenesisHashAlonzo
  { unGenesisHashAlonzo :: Crypto.Hash Crypto.Blake2b_256 ByteString
  } deriving newtype (Eq, Show)

newtype LedgerStateDir = LedgerStateDir
  {  unLedgerStateDir :: FilePath
  } deriving Show

newtype NetworkName = NetworkName
  { unNetworkName :: Text
  } deriving Show

newtype NetworkConfigFile = NetworkConfigFile
  { unNetworkConfigFile :: FilePath
  } deriving Show

newtype SocketPath = SocketPath
  { unSocketPath :: FilePath
  } deriving Show

data NodeConfig = NodeConfig
  { ncPBftSignatureThreshold :: !(Maybe Double)
  , ncByronGenesisFile :: !GenesisFile
  , ncByronGenesisHash :: !GenesisHashByron
  , ncShelleyGenesisFile :: !GenesisFile
  , ncShelleyGenesisHash :: !GenesisHashShelley
  , ncAlonzoGenesisFile :: !GenesisFile
  , ncAlonzoGenesisHash :: !GenesisHashAlonzo
  , ncRequiresNetworkMagic :: !RequiresNetworkMagic
  , ncByronSoftwareVersion :: !Byron.SoftwareVersion
  , ncByronProtocolVersion :: !Byron.ProtocolVersion

  -- Per-era parameters for the hardfok transitions:
  , ncByronToShelley   :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Consensus.StandardShelley)
  , ncShelleyToAllegra :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Consensus.StandardAllegra)
  , ncAllegraToMary    :: !(Consensus.ProtocolTransitionParamsShelleyBased
                              Consensus.StandardMary)
  , ncMaryToAlonzo     :: !Consensus.TriggerHardFork
  , ncAlonzoToBabbage  :: !Consensus.TriggerHardFork
  }

instance FromJSON NodeConfig where
  parseJSON =
      Aeson.withObject "NodeConfig" parse
    where
      parse :: Object -> Parser NodeConfig
      parse o =
        NodeConfig
          <$> o .:? "PBftSignatureThreshold"
          <*> fmap GenesisFile (o .: "ByronGenesisFile")
          <*> fmap GenesisHashByron (o .: "ByronGenesisHash")
          <*> fmap GenesisFile (o .: "ShelleyGenesisFile")
          <*> fmap GenesisHashShelley (o .: "ShelleyGenesisHash")
          <*> fmap GenesisFile (o .: "AlonzoGenesisFile")
          <*> fmap GenesisHashAlonzo (o .: "AlonzoGenesisHash")
          <*> o .: "RequiresNetworkMagic"
          <*> parseByronSoftwareVersion o
          <*> parseByronProtocolVersion o
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseShelleyHardForkEpoch o)
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseAllegraHardForkEpoch o)
          <*> (Consensus.ProtocolTransitionParamsShelleyBased ()
                 <$> parseMaryHardForkEpoch o)
          <*> parseAlonzoHardForkEpoch o
          <*> parseBabbageHardForkEpoch o

      parseByronProtocolVersion :: Object -> Parser Byron.ProtocolVersion
      parseByronProtocolVersion o =
        Byron.ProtocolVersion
          <$> o .: "LastKnownBlockVersion-Major"
          <*> o .: "LastKnownBlockVersion-Minor"
          <*> o .: "LastKnownBlockVersion-Alt"

      parseByronSoftwareVersion :: Object -> Parser Byron.SoftwareVersion
      parseByronSoftwareVersion o =
        Byron.SoftwareVersion
          <$> fmap Byron.ApplicationName (o .: "ApplicationName")
          <*> o .: "ApplicationVersion"

      parseShelleyHardForkEpoch :: Object -> Parser Consensus.TriggerHardFork
      parseShelleyHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestShelleyHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 2 -- Mainnet default
          ]

      parseAllegraHardForkEpoch :: Object -> Parser Consensus.TriggerHardFork
      parseAllegraHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestAllegraHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 3 -- Mainnet default
          ]

      parseMaryHardForkEpoch :: Object -> Parser Consensus.TriggerHardFork
      parseMaryHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestMaryHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 4 -- Mainnet default
          ]

      parseAlonzoHardForkEpoch :: Object -> Parser Consensus.TriggerHardFork
      parseAlonzoHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestAlonzoHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 5 -- Mainnet default
          ]
      parseBabbageHardForkEpoch :: Object -> Parser Consensus.TriggerHardFork
      parseBabbageHardForkEpoch o =
        asum
          [ Consensus.TriggerHardForkAtEpoch <$> o .: "TestBabbageHardForkAtEpoch"
          , pure $ Consensus.TriggerHardForkAtVersion 7 -- Mainnet default
          ]

readNetworkConfig :: NetworkConfigFile -> ExceptT Text IO NodeConfig
readNetworkConfig (NetworkConfigFile ncf) = do
    ncfg <- (except . parseNodeConfig) =<< readByteString ncf "node"
    return ncfg
      { ncByronGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncByronGenesisFile ncfg)
      , ncShelleyGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncShelleyGenesisFile ncfg)
      , ncAlonzoGenesisFile = adjustGenesisFilePath (mkAdjustPath ncf) (ncAlonzoGenesisFile ncfg)
      }

adjustGenesisFilePath :: (FilePath -> FilePath) -> GenesisFile -> GenesisFile
adjustGenesisFilePath f (GenesisFile p) = GenesisFile (f p)

mkAdjustPath :: FilePath -> (FilePath -> FilePath)
mkAdjustPath nodeConfigFilePath fp = takeDirectory nodeConfigFilePath </> fp

parseNodeConfig :: ByteString -> Either Text NodeConfig
parseNodeConfig bs =
  case Yaml.decodeEither' bs of
    Left err -> Left $ "Error parsing node config: " <> textShow err
    Right nc -> Right nc

readCardanoGenesisConfig
        :: NodeConfig
        -> ExceptT GenesisConfigError IO GenesisConfig
readCardanoGenesisConfig enc =
  GenesisCardano enc
    <$> readByronGenesisConfig enc
    <*> readShelleyGenesisConfig enc
    <*> readAlonzoGenesisConfig enc

readByronGenesisConfig
        :: NodeConfig
        -> ExceptT GenesisConfigError IO Byron.Config
readByronGenesisConfig enc = do
  let file = unGenesisFile $ ncByronGenesisFile enc
  genHash <- firstExceptT NEError
                . hoistEither
                $ decodeAbstractHash (unGenesisHashByron $ ncByronGenesisHash enc)
  firstExceptT (NEByronConfig file)
                $ Byron.mkConfigFromFile (ncRequiresNetworkMagic enc) file genHash

readShelleyGenesisConfig
    :: NodeConfig
    -> ExceptT GenesisConfigError IO ShelleyConfig
readShelleyGenesisConfig enc = do
  let file = unGenesisFile $ ncShelleyGenesisFile enc
  firstExceptT (NEShelleyConfig file . renderShelleyGenesisError)
    $ readShelleyGenesis (GenesisFile file) (ncShelleyGenesisHash enc)

readAlonzoGenesisConfig
    :: NodeConfig
    -> ExceptT GenesisConfigError IO Ledger.AlonzoGenesis
readAlonzoGenesisConfig enc = do
  let file = unGenesisFile $ ncAlonzoGenesisFile enc
  firstExceptT (NEAlonzoConfig file . renderAlonzoGenesisError)
    $ readAlonzoGenesis (GenesisFile file) (ncAlonzoGenesisHash enc)

readAlonzoGenesis
    :: GenesisFile -> GenesisHashAlonzo
    -> ExceptT AlonzoGenesisError IO Ledger.AlonzoGenesis
readAlonzoGenesis (GenesisFile file) expectedGenesisHash = do
    content <- handleIOExceptT (AlonzoGenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashAlonzo (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    firstExceptT (AlonzoGenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
  where
    checkExpectedGenesisHash :: GenesisHashAlonzo -> ExceptT AlonzoGenesisError IO ()
    checkExpectedGenesisHash actual =
      when (actual /= expectedGenesisHash) $
        left (AlonzoGenesisHashMismatch actual expectedGenesisHash)

readShelleyGenesis
    :: GenesisFile -> GenesisHashShelley
    -> ExceptT ShelleyGenesisError IO ShelleyConfig
readShelleyGenesis (GenesisFile file) expectedGenesisHash = do
    content <- handleIOExceptT (ShelleyGenesisReadError file . textShow) $ BS.readFile file
    let genesisHash = GenesisHashShelley (Crypto.hashWith id content)
    checkExpectedGenesisHash genesisHash
    genesis <- firstExceptT (ShelleyGenesisDecodeError file . Text.pack)
                  . hoistEither
                  $ Aeson.eitherDecodeStrict' content
    pure $ ShelleyConfig genesis genesisHash
  where
    checkExpectedGenesisHash :: GenesisHashShelley -> ExceptT ShelleyGenesisError IO ()
    checkExpectedGenesisHash actual =
      when (actual /= expectedGenesisHash) $
        left (ShelleyGenesisHashMismatch actual expectedGenesisHash)

data GenesisConfigError
  = NEError !Text
  | NEByronConfig !FilePath !Byron.ConfigurationError
  | NEShelleyConfig !FilePath !Text
  | NEAlonzoConfig !FilePath !Text
  | NECardanoConfig !Text

renderGenesisConfigError :: GenesisConfigError -> Text
renderGenesisConfigError ne =
  case ne of
    NEError t -> "Error: " <> t
    NEByronConfig fp ce ->
      mconcat
        [ "Failed reading Byron genesis file ", textShow fp, ": ", textShow ce
        ]
    NEShelleyConfig fp txt ->
      mconcat
        [ "Failed reading Shelley genesis file ", textShow fp, ": ", txt
        ]
    NEAlonzoConfig fp txt ->
      mconcat
        [ "Failed reading Alonzo genesis file ", textShow fp, ": ", txt
        ]
    NECardanoConfig err ->
      mconcat
        [ "With Cardano protocol, Byron/Shelley config mismatch:\n"
        , "   ", err
        ]

initExtLedgerStateVar :: GenesisConfig -> Ledger.ExtLedgerState (HFC.HardForkBlock (Consensus.CardanoEras Consensus.StandardCrypto))
initExtLedgerStateVar genesisConfig = Consensus.pInfoInitLedger protocolInfo
  where
    protocolInfo = mkProtocolInfoCardano genesisConfig

mkProtocolInfoCardano ::
  GenesisConfig ->
  Consensus.ProtocolInfo
    IO
    (HFC.HardForkBlock
            (Consensus.CardanoEras Consensus.StandardCrypto))
mkProtocolInfoCardano (GenesisCardano dnc byronGenesis shelleyGenesis alonzoGenesis)
  = Consensus.protocolInfoCardano
          Consensus.ProtocolParamsByron
            { Consensus.byronGenesis = byronGenesis
            , Consensus.byronPbftSignatureThreshold = Consensus.PBftSignatureThreshold <$> ncPBftSignatureThreshold dnc
            , Consensus.byronProtocolVersion = ncByronProtocolVersion dnc
            , Consensus.byronSoftwareVersion = ncByronSoftwareVersion dnc
            , Consensus.byronLeaderCredentials = Nothing
            , Consensus.byronMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsShelleyBased
            { Consensus.shelleyBasedGenesis = scConfig shelleyGenesis
            , Consensus.shelleyBasedInitialNonce = shelleyPraosNonce shelleyGenesis
            , Consensus.shelleyBasedLeaderCredentials = []
            }
          Consensus.ProtocolParamsShelley
            { Consensus.shelleyProtVer = shelleyProtVer dnc
            , Consensus.shelleyMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsAllegra
            { Consensus.allegraProtVer = shelleyProtVer dnc
            , Consensus.allegraMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsMary
            { Consensus.maryProtVer = shelleyProtVer dnc
            , Consensus.maryMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsAlonzo
            { Consensus.alonzoProtVer = shelleyProtVer dnc
            , Consensus.alonzoMaxTxCapacityOverrides  = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          Consensus.ProtocolParamsBabbage
            { Consensus.babbageProtVer = shelleyProtVer dnc
            , Consensus.babbageMaxTxCapacityOverrides = TxLimits.mkOverrides TxLimits.noOverridesMeasure
            }
          (ncByronToShelley dnc)
          (ncShelleyToAllegra dnc)
          (ncAllegraToMary dnc)
          (Consensus.ProtocolTransitionParamsShelleyBased alonzoGenesis (ncMaryToAlonzo dnc))
          (Consensus.ProtocolTransitionParamsShelleyBased alonzoGenesis (ncAlonzoToBabbage dnc))

shelleyProtVer :: NodeConfig -> Ledger.ProtVer
shelleyProtVer dnc =
  let bver = ncByronProtocolVersion dnc in
  Ledger.ProtVer
    (fromIntegral $ Byron.pvMajor bver)
    (fromIntegral $ Byron.pvMinor bver)

shelleyPraosNonce :: ShelleyConfig -> Ledger.Nonce
shelleyPraosNonce sCfg = Ledger.Nonce (Crypto.castHash . unGenesisHashShelley $ scGenesisHash sCfg)

textShow :: Show a => a -> Text
textShow = Text.pack . show

readByteString :: FilePath -> Text -> ExceptT Text IO ByteString
readByteString fp cfgType = ExceptT $
  catch (Right <$> BS.readFile fp) $ \(_ :: IOException) ->
    return $ Left $ mconcat
      [ "Cannot read the ", cfgType, " configuration file at : ", Text.pack fp ]
