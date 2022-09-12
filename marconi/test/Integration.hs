{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Integration where

import Control.Concurrent qualified as IO
import Control.Exception (catch)
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
import Prettyprinter.Render.Text (renderStrict)
import System.Directory qualified as IO
import System.Environment qualified as IO
import System.FilePath ((</>))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

import Cardano.Api (CardanoEra (AlonzoEra))
import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson ((.:))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map qualified as Map
import Data.Text (Text)
import Gen.Cardano.Api.Typed qualified as CGen
import Ouroboros.Network.Protocol.LocalTxSubmission.Type
import Test.Base qualified as H
import Test.Base qualified as T
import Test.Process qualified as H
import Testnet.Cardano qualified as H
import Testnet.Cardano qualified as TN
import Testnet.Conf qualified as H
import Testnet.Conf qualified as TC (Conf (..), ProjectBase (ProjectBase), YamlFilePath (YamlFilePath), mkConf)
import Testnet.SubmitApi qualified as TN

import Cardano.BM.Setup (withTrace)
import Cardano.BM.Trace (logError)
import Cardano.BM.Tracing (defaultConfigStdout)
import Gen qualified
import Marconi.Indexers qualified
import Marconi.Logging qualified
import Plutus.Streaming (ChainSyncEventException (NoIntersectionFound), withChainSyncEventStream)
import Prettyprinter (defaultLayoutOptions, layoutPretty, pretty, (<+>))

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

exit2 :: Show a => String -> a -> m ()
exit2 label v = exit $ label <> ": " <> show v

exit_ :: m ()
exit_ = exit "MANUAL EXIT"

main :: IO ()
main = defaultMain tests

-- /Tmp

readAs :: (C.HasTextEnvelope a, MonadIO m, MonadTest m) => C.AsType a -> FilePath -> m a
readAs as path = H.leftFailM . liftIO $ C.readFileTextEnvelope as path

findUTxOByAddress
  :: (MonadIO m, MonadTest m)
  => C.LocalNodeConnectInfo C.CardanoMode -> C.AddressAny -> m (C.UTxO C.AlonzoEra)
findUTxOByAddress localNodeConnectInfo address = let
  query = C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo $ C.QueryUTxO $
    C.QueryUTxOByAddress $ Set.singleton address
  in
  H.leftFailM . H.leftFailM . liftIO $ C.queryNodeLocalState localNodeConnectInfo Nothing $
    C.QueryInEra C.AlonzoEraInCardanoMode query

failOnTxSubmitFail :: (Show a, Applicative m) => SubmitResult a -> m ()
failOnTxSubmitFail submitResult =
  case submitResult of
    SubmitFail reason -> exit2 "reason" reason
    SubmitSuccess     -> pure ()


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
      socketPathAbs = tempAbsPath </> socketFilePath
  p2 "socketFilePath" socketFilePath

  H.threadDelay 1_000_000

  let sqlitedb = tempAbsPath </> "script-tx.db"
  liftIO $ IO.forkIO $ do
    let chainPoint = C.ChainPointAtGenesis :: C.ChainPoint
    c <- defaultConfigStdout
    p2 "getCurrentDirectory" =<< IO.getCurrentDirectory
    withTrace c "marconi" $ \trace ->
      withChainSyncEventStream
        socketPathAbs
        networkId
        chainPoint
        (Marconi.Indexers.combinedIndexer Nothing Nothing (Just sqlitedb) . Marconi.Logging.logging trace)
        `catch` \NoIntersectionFound ->
          logError trace $
            renderStrict $
              layoutPretty defaultLayoutOptions $
                "No intersection found when looking for the chain point" <+> pretty chainPoint <> "."
                  <+> "Please check the slot number and the block hash do belong to the chain"

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
    plutusScriptAddr :: C.Address C.ShelleyAddr
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
  genesisVKey :: C.VerificationKey C.GenesisUTxOKey <- -- /cardano-node/cardano-api/src/Cardano/Api/KeysShelley.hs::1031
    readAs (C.AsVerificationKey C.AsGenesisUTxOKey) utxoVKeyFile

  genesisSKey :: C.SigningKey C.GenesisUTxOKey <- readAs (C.AsSigningKey C.AsGenesisUTxOKey) utxoSKeyFile

  let
    paymentKey = C.castVerificationKey genesisVKey :: C.VerificationKey C.PaymentKey
    address :: C.Address C.ShelleyAddr
    address = C.makeShelleyAddress
      networkId
      (C.PaymentCredentialByKey (C.verificationKeyHash paymentKey :: C.Hash C.PaymentKey))
      C.NoStakeAddress :: C.Address C.ShelleyAddr

  p2 "address" address -- QUESTION: any way to verify this is correct
                       -- (i.e same as the following utxoAddr)?

  utxoAddrStr <- H.execCli
    [ "address", "build"
    , "--testnet-magic", show @Int testnetMagic
    , "--payment-verification-key-file", utxoVKeyFile
    ]
  p2 "utxoAddr genesisKey" genesisVKey
  p2 "utxoAddr" utxoAddrStr

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

  utxo <- findUTxOByAddress localNodeConnectInfo $ C.toAddressAny address
--  exit2 "utxo" utxo
  let utxoMap = C.unUTxO utxo
  txIn <- H.noteShow $ head $ Map.keys utxoMap
--  exit2 "txIn" txIn

  (C.Lovelace lovelaceAtTxin) <- H.nothingFailM . H.noteShow $ (\(C.TxOut _ v _) -> C.txOutValueToLovelace v) <$> (utxoMap & Map.lookup txIn)
  lovelaceAtTxinDiv3 <- H.noteShow $ lovelaceAtTxin `div` 3

  p2 "utxo" utxo
  p2 "lovelaceAtTxin" lovelaceAtTxin -- 900 ADA
  p2 "lovelaceAtTxinDiv3" lovelaceAtTxinDiv3

  -- old utxo code
  void $ H.execCli' execConfig
    [ "query", "utxo"
    , "--address", utxoAddrStr
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "utxo-1.json"
    ]
  H.cat $ work </> "utxo-1.json"
  utxo1Json <- H.leftFailM . H.readJsonFile $ work </> "utxo-1.json"
  utxo1 <- H.noteShowM $ H.jsonErrorFail $ Aeson.fromJSON @(HashMap Text Utxo) utxo1Json
  txinStr <- H.noteShow $ head $ HM.keys utxo1
  lovelaceAtTxin <- H.nothingFailM . H.noteShow $ ((utxo1 & HM.lookup txinStr) >>= HM.lookup "lovelace" . value)
  lovelaceAtTxinDiv3 <- H.noteShow $ lovelaceAtTxin `div` 3
  -- /old utxo code

  -- TODO [X] Query the PP using the IPC interface. See Plutus.PAB.Run.Cli:151 on
  -- how to query some interface from the socket connection.
  -- Would need to use stuff from Cardano.Api.IPC and Cardano.Api.Query.
  void $ H.execCli' execConfig
    [ "query", "protocol-parameters"
    , "--testnet-magic", show @Int testnetMagic
    , "--out-file", work </> "pparams.json"
    ]
  pparams <-
      H.leftFailM . H.leftFailM . liftIO
        $ C.queryNodeLocalState localNodeConnectInfo Nothing
        $ C.QueryInEra C.AlonzoEraInCardanoMode
        $ C.QueryInShelleyBasedEra C.ShelleyBasedEraAlonzo C.QueryProtocolParameters
  -- p2 "pparams" pparams

  let dummyAddressBech32 = "addr_test1vpqgspvmh6m2m5pwangvdg499srfzre2dd96qq57nlnw6yctpasy4"
      -- targetaddress = "addr_test1qpmxr8d8jcl25kyz2tz9a9sxv7jxglhddyf475045y8j3zxjcg9vquzkljyfn3rasfwwlkwu7hhm59gzxmsyxf3w9dps8832xh"

  -- Convert the string 'dummyAddressBech32' into an actual 'C.Address C.ShelleyAddr'.
  dummyAddress <- H.leftFail $ C.deserialiseFromBech32 (C.AsAddress C.AsShelleyAddr) dummyAddressBech32

  -- This datum hash is the hash of the untyped 42
  let scriptDatumHashStr = "9e1199a988ba72ffd6e9c269cadb3b53b5f360ff99f112d9b2ee30c4d74ad88b"
      scriptDatum = C.ScriptDataNumber 42 :: C.ScriptData
      scriptDatumHash = C.hashScriptData scriptDatum
  -- scriptDatumHash <- H.nothingFail $ C.deserialiseFromRawBytes (C.AsHash C.AsScriptData) scriptDatumHashStr
  p2 "scriptDatumHash" scriptDatumHash

  -- TODO [X] Create with Cardano.Api.TxBody instead of the CLI
  let
      txOut1 :: C.TxOut ctx C.AlonzoEra
      txOut1 =
          C.TxOut
            (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) plutusScriptAddr)
            (C.TxOutValue C.MultiAssetInAlonzoEra $ C.lovelaceToValue $ C.Lovelace lovelaceAtTxinDiv3)
            (C.TxOutDatumHash C.ScriptDataInAlonzoEra scriptDatumHash)
      txOut2 :: C.TxOut ctx C.AlonzoEra
      txOut2 =
          C.TxOut
            (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) address)
            (C.TxOutValue C.MultiAssetInAlonzoEra $ C.lovelaceToValue $ C.Lovelace $ 2 * lovelaceAtTxinDiv3 - 271)
            C.TxOutDatumNone
      txBodyContent :: C.TxBodyContent C.BuildTx C.AlonzoEra
      txBodyContent =
        C.TxBodyContent {
          C.txIns              = [(txIn, C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending)],
          C.txInsCollateral    = C.TxInsCollateralNone,
          C.txOuts             = [txOut1, txOut2],
          C.txFee              = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra $ C.Lovelace 271,
          C.txValidityRange    = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra),
          C.txMetadata         = C.TxMetadataNone,
          C.txAuxScripts       = C.TxAuxScriptsNone,
          C.txExtraKeyWits     = C.TxExtraKeyWitnessesNone,
          C.txProtocolParams   = C.BuildTxWith $ Just pparams,
          C.txWithdrawals      = C.TxWithdrawalsNone,
          C.txCertificates     = C.TxCertificatesNone,
          C.txUpdateProposal   = C.TxUpdateProposalNone,
          C.txMintValue        = C.TxMintNone,
          C.txScriptValidity   = C.TxScriptValidityNone
        }

  tx1body :: C.TxBody C.AlonzoEra <- H.leftFail $ C.makeTransactionBody txBodyContent
  p2 "tx1body" tx1body
  let
    kw :: C.KeyWitness C.AlonzoEra
    kw = C.makeShelleyKeyWitness tx1body (C.WitnessPaymentKey $ C.castSigningKey genesisSKey)
    tx1 = C.makeSignedTransaction [kw] tx1body

  submitResult :: SubmitResult (C.TxValidationErrorInMode C.CardanoMode) <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx1 C.AlonzoEraInCardanoMode
  failOnTxSubmitFail submitResult

  void $ H.execCli' execConfig
    [ "transaction", "build"
    , "--alonzo-era"
    , "--cardano-mode"
    , "--testnet-magic", show @Int testnetMagic
    , "--change-address", T.unpack dummyAddressBech32
    , "--tx-in", T.unpack txinStr
    , "--tx-out", plutusScriptAddrBech32 <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--tx-out-datum-hash", scriptDatumHashStr
    , "--tx-out", utxoAddrStr <> "+" <> show @Integer lovelaceAtTxinDiv3
    , "--protocol-params-file", work </> "pparams.json"
    , "--out-file", work </> "create-datum-output.body"
    ]

  {-
     What to do next:
        - Fix the todos and fully use the Cardano.Api types
        - Submit a 2nd transaction which spends the txout locked by a Plutus script
        - Run the Marconi indexer
        - Query the Marconi indexer and see that it returns a single script (which has the same hash as the one we initially used).
  -}

  -- Second transaction: spend the UTXO specified in the first transaction

  HE.threadDelay 5000000 -- wait for the first transaction to be accepted

  txIn_ <- head . Map.keys . C.unUTxO <$> findUTxOByAddress localNodeConnectInfo (C.toAddressAny address)

  p2 "plutusScriptAddr" plutusScriptAddr
  scriptUtxo <- findUTxOByAddress localNodeConnectInfo $ C.toAddressAny plutusScriptAddr
  scriptTxIn <- H.noteShow $ head $ Map.keys $ C.unUTxO scriptUtxo

  redeemer <- H.forAll CGen.genScriptData
--  executionUnits <- H.forAll Gen.genExecutionUnits -- QUESTION: this is maybe not a good idea?
  let
      executionUnits = C.ExecutionUnits {C.executionSteps = 500000,C.executionMemory = 500000 }
      fee = 100343

      C.PlutusScript lang plutusScript_ = plutusScript
      scriptWitness :: C.Witness C.WitCtxTxIn C.AlonzoEra
      scriptWitness = C.ScriptWitness C.ScriptWitnessForSpending $
        C.PlutusScriptWitness C.PlutusScriptV1InAlonzo C.PlutusScriptV1 plutusScript_
        (C.ScriptDatumForTxIn scriptDatum) redeemer executionUnits

      collateral = C.TxInsCollateral C.CollateralInAlonzoEra [txIn_]

      tx2out :: C.TxOut ctx C.AlonzoEra
      tx2out =
          C.TxOut
            (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraAlonzo) address)
             -- send ADA back to the original genesis address               ^
            (C.TxOutValue C.MultiAssetInAlonzoEra $ C.lovelaceToValue $ C.Lovelace $ lovelaceAtTxinDiv3 - fee)
            C.TxOutDatumNone

      tx2bodyContent :: C.TxBodyContent C.BuildTx C.AlonzoEra
      tx2bodyContent =
        C.TxBodyContent {
          C.txIns              = [(scriptTxIn, C.BuildTxWith scriptWitness)],
          C.txInsCollateral    = collateral,
          C.txOuts             = [tx2out],
          C.txFee              = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra $ C.Lovelace fee,
          C.txValidityRange    = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra),
          C.txMetadata         = C.TxMetadataNone,
          C.txAuxScripts       = C.TxAuxScriptsNone,
          C.txExtraKeyWits     = C.TxExtraKeyWitnessesNone,
          C.txProtocolParams   = C.BuildTxWith $ Just pparams,
          C.txWithdrawals      = C.TxWithdrawalsNone,
          C.txCertificates     = C.TxCertificatesNone,
          C.txUpdateProposal   = C.TxUpdateProposalNone,
          C.txMintValue        = C.TxMintNone,
          C.txScriptValidity   = C.TxScriptValidityNone
        }

  tx2body :: C.TxBody C.AlonzoEra <- H.leftFail $ C.makeTransactionBody tx2bodyContent
  let tx2 = C.signShelleyTransaction tx2body [C.WitnessGenesisUTxOKey genesisSKey]

  tx2submitResult <- liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx2 C.AlonzoEraInCardanoMode
  failOnTxSubmitFail tx2submitResult

  datumFile <- H.note $ base </> "plutus-example/plutus/data/42.datum"
  redeemerFile <- H.note $ base </> "plutus-example/plutus/data/42.redeemer"

  exit_
  -- Just to get a test going...
  "2" === ("2" :: Text)
