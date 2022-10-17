{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeApplications         #-}

module Test.PlutusExample.SubmitApi.TxInLockingPlutus
  ( prop_submit_api_spending_plutus_script
  ) where

import Control.Applicative
import Control.Monad
import Data.Aeson (FromJSON (..), Value, (.:))
import Data.Bool (not)
import Data.Eq
import Data.Function
import Data.Functor ((<&>))
import Data.HashMap.Lazy (HashMap)
import Data.Int
import Data.List ((!!))
import Data.Maybe
import Data.Monoid (Last (..), (<>))
import Data.Text (Text)
import GHC.Num
import GHC.Real
import Hedgehog (Property, (===))
import Prelude (head)
import System.FilePath ((</>))
import Text.Show (Show (..))

import Data.Aeson qualified as J
import Data.HashMap.Lazy qualified as HM
import Data.List qualified as List
import Data.Text qualified as Text
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.Process qualified as H
import System.Directory qualified as IO
import System.Environment qualified as IO
import Test.Base qualified as Test
import Test.Process qualified as H
import Test.Process qualified as Test
import Test.Runtime qualified as H
import Testnet.Cardano qualified as TN
import Testnet.Conf qualified as TN
import Testnet.SubmitApi qualified as TN

{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant return" -}
{- HLINT ignore "Use let" -}

data Utxo = Utxo
  { address :: Text
  , value   :: HashMap Text Integer
  } deriving (Eq, Show)

instance FromJSON Utxo where
  parseJSON = J.withObject "Utxo" $ \v -> Utxo
    <$> v .: "address"
    <*> v .: "value"

prop_submit_api_spending_plutus_script :: Property
prop_submit_api_spending_plutus_script = Test.integration . HE.runFinallies . HE.workspace "chairman" $ \tempAbsBasePath' -> do
  base <- HE.note =<< HE.noteIO . IO.canonicalizePath =<< HE.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@TN.Conf { TN.tempBaseAbsPath, TN.tempAbsPath } <- HE.noteShowM $
    TN.mkConf (TN.ProjectBase base) (TN.YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  tr@TN.TestnetRuntime { TN.configurationFile, TN.testnetMagic } <- TN.testnet TN.defaultTestnetOptions conf

  env <- H.evalIO IO.getEnvironment

  execConfig <- H.noteShow H.ExecConfig
        { H.execConfigEnv = Last $ Just $
          [ ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName $ head $ H.bftSprockets tr)
          ]
          -- The environment must be passed onto child process on Windows in order to
          -- successfully start that process.
          <> env
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  HE.note_ base
  work <- HE.note tempAbsPath
  utxoVKeyFile <- HE.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.vkey"
  utxoSKeyFile <- HE.note $ tempAbsPath </> "shelley/utxo-keys/utxo1.skey"

  plutusScriptFileInUse <- HE.note $ base </> "plutus-example/plutus/scripts/always-succeeds-spending.plutus"

  submitApiConfigFile <- HE.note configurationFile
  submitApiStdoutFile <- HE.note $ tempAbsPath </> "logs/submit-api.stdout"
  submitApiStderrFile <- HE.note $ tempAbsPath </> "logs/submit-api.stderr"

  submitApiPort <- TN.submitApi TN.SubmitApiConfig
    { TN.tempBaseAbsPath
    , TN.base
    , TN.configFile = submitApiConfigFile
    , TN.sprocket = head $ H.bftSprockets tr
    , TN.testnetMagic
    , TN.stdoutFile = submitApiStdoutFile
    , TN.stderrFile = submitApiStderrFile
    }

  -- This datum hash is the hash of the untyped 42
  let scriptDatumHash = "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
  let plutusRequiredSpace = id @Integer 70000000
  let plutusRequiredTime = id @Integer 70000000

  datumFile <- HE.note $ base </> "plutus-example/plutus/data/42.datum"
  redeemerFile <- HE.note $ base </> "plutus-example/plutus/data/42.redeemer"

  -- Always succeeds Plutus script in use. Any datum and redeemer combination will succeed.
  -- Script at: $plutusscriptinuse

  -- Step 1: Create a tx ouput with a datum hash at the script address. In order for a tx ouput to be locked
  -- by a plutus script, it must have a datahash. We also need collateral tx inputs so we split the utxo
  -- in order to accomodate this.

  plutusScriptAddr <- Test.execCli
    [ "address", "build"
    , "--payment-script-file", plutusScriptFileInUse
    , "--testnet-magic", show @Int testnetMagic
    ]

  utxoAddr <- Test.execCli
    [ "address", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--payment-verification-key-file", utxoVKeyFile
    ]

  void $ Test.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]

  HE.cat $ work </> "utxo-1.json"

  utxo1Json <- HE.leftFailM . HE.readJsonFile $ work </> "utxo-1.json"
  utxo1 <- HE.noteShowM $ HE.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) utxo1Json
  txin <- HE.noteShow $ head $ HM.keys utxo1
  lovelaceAtTxin <- HE.nothingFailM . HE.noteShow $ utxo1 & HM.lookup txin <&> value >>= HM.lookup "lovelace"
  lovelaceAtTxinDiv3 <- HE.noteShow $ lovelaceAtTxin `div` 3

  let dummyaddress = "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4"
      targetaddress = "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh"

  void $ Test.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", dummyaddress
    , "--tx-in", Text.unpack txin
    , "--tx-out", plutusScriptAddr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--tx-out-datum-hash", scriptDatumHash
    , "--tx-out", utxoAddr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "create-datum-output.body"
    ]

  void $ Test.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "create-datum-output.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "create-datum-output.tx"
    ]

  void $ Test.execCli' execConfig
    [ "transaction", "submit"
    , "--tx-file", work </> "create-datum-output.tx"
    , "--testnet-magic", show @Int testnetMagic
    ]

  HE.threadDelay 5000000

  -- With the tx ouput at the script address we can now attempt to spend it.

  void $ Test.execCli' execConfig
    [ "query", "utxo"
    , "--address", plutusScriptAddr
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "plutusutxo.json"
    ]

  HE.cat $ work </> "plutusutxo.json"

  plutusUtxoJson <- HE.leftFailM . HE.readJsonFile $ work </> "plutusutxo.json"
  plutusUtxo <- HE.noteShowM $ HE.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) plutusUtxoJson
  plutusUtxoTxIn <- HE.noteShow $ head $ HM.keys plutusUtxo

  void $ Test.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-2.json"
    ]

  HE.cat $ work </> "utxo-2.json"

  utxo2Json :: Value <- HE.leftFailM $ HE.readJsonFile $ work </> "utxo-2.json"
  utxo2 <- HE.noteShowM $ HE.jsonErrorFail $ J.fromJSON @(HashMap Text Utxo) utxo2Json
  txinCollateral <- HE.noteShow $ head $ HM.keys utxo2


  lovelaceAtplutusScriptAddr <- HE.nothingFailM . HE.noteShow $ plutusUtxo & HM.lookup plutusUtxoTxIn <&> value >>= HM.lookup "lovelace"

  spendable <- HE.noteShow $ lovelaceAtplutusScriptAddr - plutusRequiredTime - plutusRequiredSpace

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", dummyaddress
    , "--tx-in", Text.unpack plutusUtxoTxIn
    , "--tx-in-collateral", Text.unpack txinCollateral
    , "--tx-out", targetaddress <> "+" <> show @Integer spendable
    , "--tx-in-script-file", plutusScriptFileInUse
    , "--tx-in-datum-file", datumFile
    , "--protocol-params-file", work </> "pparams.json"
    , "--tx-in-redeemer-file", redeemerFile
    , "--out-file", work </> "test-alonzo.body"
    ]

  void $ Test.execCli
    [ "transaction", "sign"
    , "--tx-body-file", work </> "test-alonzo.body"
    , "--testnet-magic", show @Int testnetMagic
    , "--signing-key-file", utxoSKeyFile
    , "--out-file", work </> "alonzo.tx"
    ]

  void $ TN.submitApiSubmitTransaction submitApiPort $ work </> "alonzo.tx"

  HE.threadDelay 5000000

  -- Querying UTxO at $targetaddress. If there is ADA at the address the Plutus script successfully executed!

  result <- H.evalM $ Text.pack <$> Test.execCli' execConfig
    [ "query", "utxo"
    , "--address", targetaddress
    , "--testnet-magic", show @Int testnetMagic
    ]

  HE.note_ $ Text.unpack result

  List.filter (not . Text.null) (Text.splitOn " " (Text.lines result !! 2)) !! 2 === "299860000000"
