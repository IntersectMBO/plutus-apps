{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Integration where

import Control.Monad (void)
import Data.Aeson qualified as Aeson
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HM
import Data.Monoid (Last (Last))
import Data.Set qualified as Set
import Data.Text qualified as T
import Hedgehog (MonadTest, Property, assert, eval, (===))
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.Concurrent qualified as H
import Hedgehog.Extras.Test.File qualified as H
import Hedgehog.Extras.Test.Process qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Text (Text)
import Test.Base qualified as H
import Test.Base qualified as T
import Test.Process qualified as H
import Testnet.Cardano qualified as H
import Testnet.Cardano qualified as TN
import Testnet.Conf qualified as H
import Testnet.Conf qualified as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)
import Testnet.SubmitApi qualified as TN

-- Copied from plutus-example/test/Test/PlutusExample/SubmitApi/TxInLockingPlutus
data Utxo = Utxo
  { address :: Text
  , value   :: HashMap Text Integer
  } deriving (Eq, Show)

instance Aeson.FromJSON Utxo where
  parseJSON = Aeson.withObject "Utxo" $ \v -> Utxo
    <$> v .: "address"
    <*> v .: "value"

-- * Tmp

p :: (MonadIO m) => String -> m ()
p = liftIO . putStrLn

p2 :: (Show a, MonadIO m) => String -> a -> m ()
p2 str a = liftIO $ putStrLn $ str <> ": " <> show a

pause :: MonadIO m => m ()
pause = liftIO readLn

exit :: String -> m ()
exit = error

exit_ :: m ()
exit_ = exit "MANUAL EXIT"

main :: IO ()
main = defaultMain tests

-- /Tmp

readAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m a
readAs as path = H.leftFailM . liftIO $ C.readFileTextEnvelope as path

tests :: TestTree
tests = testGroup "Integration"
  [ testProperty "prop_script_hashes_in_tx_match" testIndex ]

testIndex :: Property
testIndex = T.integration . HE.runFinallies . HE.workspace "chairman" $ \tempAbsBasePath' -> do

  base <- HE.note =<< HE.noteIO . IO.canonicalizePath =<< HE.getProjectBase

  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@TC.Conf { TC.tempBaseAbsPath, TC.tempAbsPath } <- HE.noteShowM $
    TC.mkConf (TC.ProjectBase base) (TC.YamlFilePath configurationTemplate)
      (tempAbsBasePath' <> "/")
      Nothing

  H.threadDelay 10000
  p "\n\n"
  p2 "tempAbsPath" tempAbsPath -- "/tmp/chairman/$random/"
  p2 "base" base -- current git repo dir

  TN.TestnetRuntime { TN.configurationFile, TN.bftSprockets, TN.testnetMagic } <- TN.testnet TN.defaultTestnetOptions conf
  let networkId = C.Testnet $ C.NetworkMagic $ fromIntegral testnetMagic

  let socketFilePath = IO.sprocketArgumentName (head bftSprockets)
  p2 "socketFilePath" socketFilePath

  env <- H.evalIO IO.getEnvironment

  execConfig <- eval H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("CARDANO_NODE_SOCKET_PATH", socketFilePath)
          ]
          -- The environment must be passed onto child process on Windows in order to
          -- successfully start that process.
          <> env
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  H.note_ base
  work <- H.note tempAbsPath

  assert $ tempAbsPath == (tempAbsBasePath' <> "/")
        && tempAbsPath == (tempBaseAbsPath <> "/")
        && work == (tempAbsBasePath' <> "/")

  utxoVKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- H.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"

  -- Create the Shelley Address from the actual Plutus script.
  plutusScriptFileInUse <- H.note $ base </> "plutus-example/plutus/scripts/always-succeeds-spending.plutus"
  plutusScript <- C.PlutusScript C.PlutusScriptV1
    <$> readAs (C.AsPlutusScript C.AsPlutusScriptV1) plutusScriptFileInUse
  let
    plutusScriptAddr =
      C.makeShelleyAddress
        networkId
        (C.PaymentCredentialByScript $ C.hashScript plutusScript)
        C.NoStakeAddress

  -- Always succeeds Plutus script in use. Any datum and redeemer combination will succeed.
  -- Script at: $plutusscriptinuse

  -- Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
  -- by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
  -- in order to accomodate this.

  plutusScriptAddrBech32 <- H.execCli
    [ "address", "build"
    , "--payment-script-file", plutusScriptFileInUse
    , "--testnet-magic", show @Int testnetMagic
    ]

  -- TODO [X] Create the address using Cardano.Api
  genesisKey :: C.VerificationKey C.GenesisUTxOKey <- -- /cardano-node/cardano-api/src/Cardano/Api/KeysShelley.hs::1031
    readAs (C.AsVerificationKey C.AsGenesisUTxOKey) utxoVKeyFile

  let
    paymentKey = C.castVerificationKey genesisKey :: C.VerificationKey C.PaymentKey
    address :: C.Address C.ShelleyAddr
    address = C.makeShelleyAddress
      networkId
      (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
      C.NoStakeAddress :: C.Address C.ShelleyAddr

  p2 "address" address -- QUESTION: any way to verify this is correct
                       -- (i.e same as the following utxoAddr)?

  utxoAddr <- H.execCli
    [ "address", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--payment-verification-key-file", utxoVKeyFile
    ]
  p2 "utxoAddr genesisKey" genesisKey
  p2 "utxoAddr" utxoAddr

  -- TODO [ ] Query the utxo using the query interface of the node

  -- Boilerplate codecs used for protocol serialisation.  The number
  -- of epochSlots is specific to each blockchain instance. This value
  -- what the cardano main and testnet uses. Only applies to the Byron
  -- era.
  let epochSlots = C.EpochSlots 21600
      localNodeConnectInfo =
          C.LocalNodeConnectInfo
              { C.localConsensusModeParams = C.CardanoModeParams epochSlots
              , C.localNodeNetworkId = networkId
              , C.localNodeSocketPath = work </> socketFilePath
              }

  -- liftIO $ C.connectToLocalNode localNodeConnectInfo undefined

  let
    query :: C.QueryInEra C.AlonzoEra (C.UTxO C.AlonzoEra)
    query = C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo $ C.QueryUTxO $
      C.QueryUTxOByAddress $ Set.singleton $ C.toAddressAny address

  utxo <- liftIO $ C.queryNodeLocalState localNodeConnectInfo Nothing $
    C.QueryInEra C.AlonzoEraInCardanoMode query

  p2 "utxo" utxo

  -- old utxo code
  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]
  H.cat $ work </> "utxo-1.json"
  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  utxo1 <- H.noteShowM $ H.jsonErrorFail $ Aeson.fromJSON @(HashMap Text Utxo) utxo1Json
  txin <- H.noteShow $ head $ HM.keys utxo1
  lovelaceAtTxin <- H.nothingFailM . H.noteShow $ ((utxo1 & HM.lookup txin) >>= HM.lookup "lovelace" . value)
  lovelaceAtTxinDiv3 <- H.noteShow $ lovelaceAtTxin `div` 3
  -- /old utxo code

  -- TODO Query the PP using the IPC interface. See Plutus.PAB.Run.Cli:151 on
  -- how to query some interface from the socket connection.
  -- Would need to use stuff from Cardano.Api.IPC and Cardano.Api.Query.
  void $ H.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  let dummyAddressBech32 = "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4"
      -- targetaddress = "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh"

  -- Convert the string 'dummyAddressBech32' into an actual 'C.Address C.ShelleyAddr'.
  dummyAddress <- H.leftFail $ C.deserialiseFromBech32 (C.AsAddress C.AsShelleyAddr) dummyAddressBech32

  -- This datum hash is the hash of the untyped 42
  let scriptDatumHash = "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"

  -- TODO Create with Cardano.Api.TxBody instead of the CLI
  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", T.unpack dummyAddressBech32
    , "--tx-in", T.unpack txin
    , "--tx-out", plutusScriptAddrBech32 <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--tx-out-datum-hash", scriptDatumHash
    , "--tx-out", utxoAddr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "create-datum-output.body"
    ]

  -- Read the transaction body TextEnvelope that was saved to file in the
  -- previous command and decode it.
  txBody <- readAs (C.AsTxBody C.AsAlonzoEra) (work </> "create-datum-output.body")
  liftIO $ print txBody

  -- Read the signing key TextEnvelope that was saved to file.
  utxoSKey <- readAs (C.AsSigningKey C.AsGenesisUTxOKey) utxoSKeyFile
  liftIO $ print txBody

  let tx = C.signShelleyTransaction txBody [C.WitnessGenesisUTxOKey utxoSKey]

  liftIO $ print socketFilePath
  liftIO $ print $ work </> socketFilePath
  void $ liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx C.AlonzoEraInCardanoMode

  {-
     What to do next:
        - Fix the todos and fully use the Cardano.Api types
        - Submit a 2nd transaction which spends the txout locked by a Plutus script
        - Run the Marconi indexer
        - Query the Marconi indexer and see that it returns a single script (which has the same hash as the one we initially used).
  -}

  datumFile <- H.note $ base </> "plutus-example/plutus/data/42.datum"
  redeemerFile <- H.note $ base </> "plutus-example/plutus/data/42.redeemer"

  -- Just to get a test going...
  "2" === ("2" :: Text)
