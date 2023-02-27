{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -Wno-unused-local-binds -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

-- | Copy of Cardano.Babbage from cardano-testnet (1.35.4 branch) with some tweaks to make it
--   work correctly in Alonzo and Babbage eras. Will use cardano-testnet in future when it has
--   been refined to work correctly in multi-era.
module CardanoTestnet
  ( TestnetOptions(..)
  , defaultTestnetOptions
  , TestnetNodeOptions(..)
  , defaultTestnetNodeOptions

  , Era(..)
  , TestnetRuntime (..)
  , TestnetNode (..)
  , PaymentKeyPair(..)

  , testnet
  ) where

import Cardano.Api qualified as C
import Control.Concurrent (threadDelay)
import Control.Monad (forM, forM_, unless, when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (encode, object, toJSON, (.=))
import Hedgehog.Extras.Stock.IO.Network.Sprocket (Sprocket (..))
import Hedgehog.Extras.Stock.Time (showUTCTimeSeconds)
import System.FilePath.Posix ((</>))
import Test.Runtime (Delegator (..), NodeLoggingFormat (..), PaymentKeyPair (..), PoolNode (PoolNode),
                     PoolNodeKeys (..), StakingKeyPair (..), TestnetNode (..), TestnetRuntime (..))

import Data.Functor (void, ($>), (<&>))
import Data.HashMap.Lazy qualified as HM
import Data.List qualified as L
import Data.Time.Clock qualified as DTC
import Hedgehog (MonadTest)
import Hedgehog qualified as H
import Hedgehog.Extras.Stock.Aeson qualified as J
import Hedgehog.Extras.Stock.IO.Network.Socket qualified as H
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Stock.OS qualified as OS
import Hedgehog.Extras.Stock.String qualified as S
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H
import Hedgehog.Extras.Test.Process qualified as H
import Network.Socket qualified as IO
import System.IO qualified as IO
import System.Info qualified as OS
import System.Process qualified as IO
import Test.Assert qualified as H
import Test.Process qualified as H
import Testnet.Conf qualified as H
import UnliftIO.Exception qualified as IO

{- HLINT ignore "Reduce duplication" -}
{- HLINT ignore "Redundant <&>" -}
{- HLINT ignore "Redundant flip" -}
{- HLINT ignore "Redundant id" -}
{- HLINT ignore "Use let" -}

data Era = Alonzo | Babbage deriving (Eq, Show)

data TestnetOptions = TestnetOptions
  { era               :: C.AnyCardanoEra
  , protocolVersion   :: Int
  , slotDuration      :: Int
  , slotLength        :: Double
  , activeSlotsCoeff  :: Double
  , securityParam     :: Int
  , totalBalance      :: Int
  , nodeLoggingFormat :: NodeLoggingFormat
  } deriving (Eq, Show)

defaultTestnetOptions :: TestnetOptions
defaultTestnetOptions = TestnetOptions
  { era = C.AnyCardanoEra C.BabbageEra
  , protocolVersion = 8
  , slotDuration = 1000
  , slotLength = 0.2
  , activeSlotsCoeff = 0.1 -- higher value (e.g. 0.9) prevents long waits for slot leader but could be the cause of more rollbacks/forks
  , securityParam = 10
  , totalBalance = 10020000000
  , nodeLoggingFormat = NodeLoggingFormatAsJson
  }

data TestnetNodeOptions = TestnetNodeOptions deriving (Eq, Show)

defaultTestnetNodeOptions :: TestnetNodeOptions
defaultTestnetNodeOptions = TestnetNodeOptions

-- | For an unknown reason, CLI commands are a lot slower on Windows than on Linux and
-- MacOS.  We need to allow a lot more time to set up a testnet.
startTimeOffsetSeconds :: DTC.NominalDiffTime
startTimeOffsetSeconds = if OS.isWin32 then 90 else 15

-- | Check if a TCP port is open
isPortOpen :: Int -> IO Bool
isPortOpen port = do
  socketAddressInfos <- IO.getAddrInfo Nothing (Just "127.0.0.1") (Just (show port))
  case socketAddressInfos of
    socketAddressInfo:_ -> canConnect (IO.addrAddress socketAddressInfo) $> True
    []                  -> return False

-- | Check if it is possible to connect to a socket address
-- TODO: upstream fix to Hedgehog Extras
canConnect :: IO.SockAddr -> IO Bool
canConnect sockAddr = IO.bracket (IO.socket IO.AF_INET IO.Stream 6) IO.close' $ \sock -> do
  res <- IO.try $ IO.connect sock sockAddr
  case res of
    Left (_ :: IO.IOException) -> return False
    Right _                    -> return True

-- | Get random list of open ports. Timeout after 60seconds if unsuccessful.
getOpenPorts :: (MonadTest m, Control.Monad.IO.Class.MonadIO m) => Int -> Int -> m [Int]
getOpenPorts n numberOfPorts = do
  when (n == 0) $ do
   error "getOpenPorts timeout"
  ports <- liftIO $ H.allocateRandomPorts numberOfPorts
  allOpen <- liftIO $ mapM isPortOpen ports
  unless (and allOpen) $ do
    H.annotate "Some ports are not open, trying again..."
    liftIO $ threadDelay 1_000_000 -- wait 1 sec
    void $ getOpenPorts (pred n) numberOfPorts
  pure ports

testnet :: TestnetOptions -> H.Conf -> H.Integration TestnetRuntime
testnet testnetOptions H.Conf {..} = do
  H.createDirectoryIfMissing (tempAbsPath </> "logs")

  H.lbsWriteFile (tempAbsPath </> "byron.genesis.spec.json") . encode $ object
    [ "heavyDelThd"       .= ("300000000000" :: String)
    , "maxBlockSize"      .= ("2000000" :: String)
    , "maxTxSize"         .= ("4096" :: String)
    , "maxHeaderSize"     .= ("2000000" :: String)
    , "maxProposalSize"   .= ("700" :: String)
    , "mpcThd"            .= ("20000000000000" :: String)
    , "scriptVersion"     .= (0 :: Int)
    , "slotDuration"      .= show @Int (slotDuration testnetOptions)
    , "unlockStakeEpoch"  .= ("18446744073709551615" :: String)
    , "updateImplicit"    .= ("10000" :: String)
    , "updateProposalThd" .= ("100000000000000" :: String)
    , "updateVoteThd"     .= ("1000000000000" :: String)
    , "softforkRule" .= object
      [ "initThd" .= ("900000000000000" :: String)
      , "minThd" .= ("600000000000000" :: String)
      , "thdDecrement" .= ("50000000000000" :: String)
      ]
    , "txFeePolicy" .= object
      [ "multiplier" .= ("43946000000" :: String)
      , "summand" .= ("155381000000000" :: String)
      ]
    ]

  void $ H.note OS.os
  currentTime <- H.noteShowIO DTC.getCurrentTime
  startTime <- H.noteShow $ DTC.addUTCTime startTimeOffsetSeconds currentTime

  let numPoolNodes = 3 :: Int

  void . H.execCli $
    [ "byron", "genesis", "genesis"
    , "--protocol-magic", show @Int testnetMagic
    , "--start-time", showUTCTimeSeconds startTime
    , "--k", show @Int (securityParam testnetOptions)
    , "--n-poor-addresses", "0"
    , "--n-delegate-addresses", show numPoolNodes
    , "--total-balance", show @Int (totalBalance testnetOptions)
    , "--delegate-share", "1"
    , "--avvm-entry-count", "0"
    , "--avvm-entry-balance", "0"
    , "--protocol-parameters-file", tempAbsPath </> "byron.genesis.spec.json"
    , "--genesis-output-dir", tempAbsPath </> "byron-gen-command"
    ]

  -- Because in Babbage the overlay schedule and decentralization parameter
  -- are deprecated, we must use the "create-staked" cli command to create
  -- SPOs in the ShelleyGenesis

  alonzoBabbageTestGenesisJsonSourceFile <- H.noteShow $ base </> "scripts/babbage/alonzo-babbage-test-genesis.json"
  alonzoBabbageTestGenesisJsonTargetFile <- H.noteShow $ tempAbsPath </> "genesis.alonzo.spec.json"

  H.copyFile alonzoBabbageTestGenesisJsonSourceFile alonzoBabbageTestGenesisJsonTargetFile

  configurationFile <- H.noteShow $ tempAbsPath </> "configuration.yaml"

  H.readFile configurationTemplate >>= H.writeFile configurationFile

  H.rewriteYamlFile (tempAbsPath </> "configuration.yaml") . J.rewriteObject
    $ HM.delete "GenesisFile"
    . HM.insert "Protocol" (toJSON @String "Cardano")
    . HM.insert "PBftSignatureThreshold" (toJSON @Double 0.6)
    . HM.insert "minSeverity" (toJSON @String "Debug")
    . HM.insert "ByronGenesisFile" (toJSON @String "genesis/byron/genesis.json")
    . HM.insert "ShelleyGenesisFile" (toJSON @String "genesis/shelley/genesis.json")
    . HM.insert "AlonzoGenesisFile" (toJSON @String "genesis/shelley/genesis.alonzo.json")
    . HM.insert "RequiresNetworkMagic" (toJSON @String "RequiresMagic")
    . HM.insert "LastKnownBlockVersion-Major" (toJSON @Int 6)
    . HM.insert "LastKnownBlockVersion-Minor" (toJSON @Int 0)
    . HM.insert "TestShelleyHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestAllegraHardForkAtEpoch" (toJSON @Int 0)
    . HM.insert "TestMaryHardForkAtEpoch" (toJSON @Int 0)
    . case era testnetOptions of
        C.AnyCardanoEra C.AlonzoEra  ->
            HM.insert "TestAlonzoHardForkAtEpoch" (toJSON @Int 0)
        C.AnyCardanoEra C.BabbageEra ->
            HM.insert "TestAlonzoHardForkAtEpoch" (toJSON @Int 0)
          . HM.insert "TestBabbageHardForkAtEpoch" (toJSON @Int 0)
        otherwise -> HM.insert "" "" -- meh
    . HM.insert "TestEnableDevelopmentHardForkEras" (toJSON True)
    . flip HM.alter "setupScribes"
        ( fmap
          . J.rewriteArrayElements
            . J.rewriteObject
              . HM.insert "scFormat"
                $ case nodeLoggingFormat testnetOptions of
                    NodeLoggingFormatAsJson -> "ScJson"
                    NodeLoggingFormatAsText -> "ScText")

  void . H.execCli $
    [ "genesis", "create-staked"
    , "--genesis-dir", tempAbsPath
    , "--testnet-magic", show @Int testnetMagic
    , "--gen-pools", show numPoolNodes
    , "--supply", "1000000000000"
    , "--supply-delegated", "1000000000000"
    , "--gen-stake-delegs", "3"
    , "--gen-utxo-keys", "3"
    ]

  poolKeys <- H.noteShow $ flip fmap [1..numPoolNodes] $ \n ->
    PoolNodeKeys
      { poolNodeKeysColdVkey = tempAbsPath </> "pools" </> "cold" <> show n <> ".vkey"
      , poolNodeKeysColdSkey = tempAbsPath </> "pools" </> "cold" <> show n <> ".skey"
      , poolNodeKeysVrfVkey = tempAbsPath </> "node-spo" <> show n </> "vrf.vkey"
      , poolNodeKeysVrfSkey = tempAbsPath </> "node-spo" <> show n </> "vrf.skey"
      , poolNodeKeysStakingVkey = tempAbsPath </> "pools" </> "staking-reward" <> show n <> ".vkey"
      , poolNodeKeysStakingSkey = tempAbsPath </> "pools" </> "staking-reward" <> show n <> ".skey"
      }

  wallets <- forM [1..3] $ \idx -> do
    pure $ PaymentKeyPair
      { paymentSKey = tempAbsPath </> "utxo-keys/utxo" <> show @Int idx <> ".skey"
      , paymentVKey = tempAbsPath </> "utxo-keys/utxo" <> show @Int idx <> ".vkey"
      }

  delegators <- forM [1..3] $ \idx -> do
    pure $ Delegator
      { paymentKeyPair = PaymentKeyPair
        { paymentSKey = tempAbsPath </> "stake-delegator-keys/payment" <> show @Int idx <> ".skey"
        , paymentVKey = tempAbsPath </> "stake-delegator-keys/payment" <> show @Int idx <> ".vkey"
        }
      , stakingKeyPair = StakingKeyPair
        { stakingSKey = tempAbsPath </> "stake-delegator-keys/staking" <> show @Int idx <> ".skey"
        , stakingVKey = tempAbsPath </> "stake-delegator-keys/staking" <> show @Int idx <> ".vkey"
        }
      }

  let spoNodes :: [String] = ("node-spo" <>) . show <$> [1..3 :: Int] -- 3 spo nodes

  -- Create the node directories

  forM_ spoNodes $ \node -> do
    H.createDirectoryIfMissing (tempAbsPath </> node)

  -- Here we move all of the keys etc generated by create-staked
  -- for the nodes to use

  -- Move all genesis related files

  H.createDirectoryIfMissing $ tempAbsPath </> "genesis/byron"
  H.createDirectoryIfMissing $ tempAbsPath </> "genesis/shelley"

  files <- H.listDirectory tempAbsPath
  forM_ files $ \file -> do
    H.note file

  H.renameFile (tempAbsPath </> "byron-gen-command/genesis.json") (tempAbsPath </> "genesis/byron/genesis.json")
  H.renameFile (tempAbsPath </> "genesis.alonzo.json") (tempAbsPath </> "genesis/shelley/genesis.alonzo.json")
  H.renameFile (tempAbsPath </> "genesis.json") (tempAbsPath </> "genesis/shelley/genesis.json")

  H.rewriteJsonFile (tempAbsPath </> "genesis/byron/genesis.json") $ J.rewriteObject
    $ flip HM.adjust "protocolConsts"
      ( J.rewriteObject ( HM.insert "protocolMagic" (toJSON @Int testnetMagic)))

  H.rewriteJsonFile (tempAbsPath </> "genesis/shelley/genesis.json") $ J.rewriteObject
    ( HM.insert "slotLength"             (toJSON @Double (slotLength testnetOptions))
    . HM.insert "activeSlotsCoeff"       (toJSON @Double (activeSlotsCoeff testnetOptions))
    . HM.insert "securityParam"          (toJSON @Int 10)
    . HM.insert "epochLength"            (toJSON @Int 10_000) -- increased so that txs can have higher upper bound validity range
    . HM.insert "maxLovelaceSupply"      (toJSON @Int 1000000000000)
    . flip HM.adjust "protocolParams"
      ( J.rewriteObject
        ( flip HM.adjust "protocolVersion"
          ( J.rewriteObject ( HM.insert "major" (toJSON @Int (protocolVersion testnetOptions))))
        )
        . J.rewriteObject (HM.insert "rho"                    (toJSON @Double 0.1))
        . J.rewriteObject (HM.insert "tau"                    (toJSON @Double 0.1))
        . J.rewriteObject (HM.insert "minFeeA"                (toJSON @Int 44))
        . J.rewriteObject (HM.insert "minFeeB"                (toJSON @Int 155381))
        . J.rewriteObject (HM.insert "minUTxOValue"           (toJSON @Int 1000000))
        . J.rewriteObject (HM.insert "decentralisationParam"  (toJSON @Double 0.7))
      )
    . HM.insert "updateQuorum"           (toJSON @Int 1)
    )

  H.renameFile (tempAbsPath </> "pools/vrf1.skey") (tempAbsPath </> "node-spo1/vrf.skey")
  H.renameFile (tempAbsPath </> "pools/vrf2.skey") (tempAbsPath </> "node-spo2/vrf.skey")
  H.renameFile (tempAbsPath </> "pools/vrf3.skey") (tempAbsPath </> "node-spo3/vrf.skey")

  H.renameFile (tempAbsPath </> "pools/opcert1.cert") (tempAbsPath </> "node-spo1/opcert.cert")
  H.renameFile (tempAbsPath </> "pools/opcert2.cert") (tempAbsPath </> "node-spo2/opcert.cert")
  H.renameFile (tempAbsPath </> "pools/opcert3.cert") (tempAbsPath </> "node-spo3/opcert.cert")

  H.renameFile (tempAbsPath </> "pools/kes1.skey") (tempAbsPath </> "node-spo1/kes.skey")
  H.renameFile (tempAbsPath </> "pools/kes2.skey") (tempAbsPath </> "node-spo2/kes.skey")
  H.renameFile (tempAbsPath </> "pools/kes3.skey") (tempAbsPath </> "node-spo3/kes.skey")

  -- Byron related

  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.000.key") (tempAbsPath </> "node-spo1/byron-delegate.key")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.001.key") (tempAbsPath </> "node-spo2/byron-delegate.key")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegate-keys.002.key") (tempAbsPath </> "node-spo3/byron-delegate.key")

  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.000.json") (tempAbsPath </> "node-spo1/byron-delegation.cert")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.001.json") (tempAbsPath </> "node-spo2/byron-delegation.cert")
  H.renameFile (tempAbsPath </> "byron-gen-command/delegation-cert.002.json") (tempAbsPath </> "node-spo3/byron-delegation.cert")

  [port1, port2, port3] <- getOpenPorts 60 numPoolNodes -- 60s timeout to find open ports

  H.writeFile (tempAbsPath </> "node-spo1/port") (show port1)
  H.writeFile (tempAbsPath </> "node-spo2/port") (show port2)
  H.writeFile (tempAbsPath </> "node-spo3/port") (show port3)

  -- Make topology files
  -- TODO generalise this over the N BFT nodes and pool nodes

  H.lbsWriteFile (tempAbsPath </> "node-spo1/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port2
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port3
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  H.lbsWriteFile (tempAbsPath </> "node-spo2/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port1
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port3
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  H.lbsWriteFile (tempAbsPath </> "node-spo3/topology.json") $ encode $
    object
    [ "Producers" .= toJSON
      [ object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port1
        , "valency" .= toJSON @Int 1
        ]
      , object
        [ "addr"    .= toJSON @String "127.0.0.1"
        , "port"    .= toJSON @Int port2
        , "valency" .= toJSON @Int 1
        ]
      ]
    ]

  (poolSprockets, poolStdins, poolStdouts, poolStderrs, poolProcessHandles) <- fmap L.unzip5 . forM spoNodes $ \node -> do
    dbDir <- H.noteShow $ tempAbsPath </> "db/" <> node
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    nodeStderrFile <- H.noteTempFile logDir $ node <> ".stderr.log"
    sprocket <- H.noteShow $ Hedgehog.Extras.Stock.IO.Network.Sprocket.Sprocket tempBaseAbsPath (socketDir </> node)

    H.createDirectoryIfMissing dbDir
    H.createDirectoryIfMissing $ tempBaseAbsPath </> socketDir

    hNodeStdout <- H.openFile nodeStdoutFile IO.WriteMode
    hNodeStderr <- H.openFile nodeStderrFile IO.WriteMode

    H.diff (L.length (IO.sprocketArgumentName sprocket)) (<=) IO.maxSprocketArgumentNameLength

    portString <- fmap S.strip . H.readFile $ tempAbsPath </> node </> "port"

    (Just stdIn, _, _, hProcess, _) <- H.createProcess =<<
      ( H.procNode
        [ "run"
        , "--config", tempAbsPath </> "configuration.yaml"
        , "--topology", tempAbsPath </> node </> "topology.json"
        , "--database-path", tempAbsPath </> node </> "db"
        , "--socket-path", IO.sprocketArgumentName sprocket
        , "--shelley-kes-key", tempAbsPath </> node </> "kes.skey"
        , "--shelley-vrf-key", tempAbsPath </> node </> "vrf.skey"
        , "--byron-delegation-certificate", tempAbsPath </> node </> "byron-delegation.cert"
        , "--byron-signing-key", tempAbsPath </> node </> "byron-delegate.key"
        , "--shelley-operational-certificate", tempAbsPath </> node </> "opcert.cert"
        , "--port",  portString
        ] <&>
        ( \cp -> cp
          { IO.std_in = IO.CreatePipe
          , IO.std_out = IO.UseHandle hNodeStdout
          , IO.std_err = IO.UseHandle hNodeStderr
          , IO.cwd = Just tempBaseAbsPath
          }
        )
      )

    when (OS.os `L.elem` ["darwin", "linux"]) $ do
      H.onFailure . H.noteIO_ $ IO.readProcess "lsof" ["-iTCP:" <> portString, "-sTCP:LISTEN", "-n", "-P"] ""

    return (sprocket, stdIn, nodeStdoutFile, nodeStderrFile, hProcess)

  when (OS.os == "darwin") $ do
    liftIO $ threadDelay 60_000_000 -- wait 1 min for network to stabilise before proceeding

  now <- H.noteShowIO DTC.getCurrentTime
  deadline <- H.noteShow $ DTC.addUTCTime 300 now -- increased from 90s

  forM_ spoNodes $ \node -> do
    nodeStdoutFile <- H.noteTempFile logDir $ node <> ".stdout.log"
    H.assertChainExtended deadline (nodeLoggingFormat testnetOptions) nodeStdoutFile

  H.noteShowIO_ DTC.getCurrentTime

  forM_ wallets $ \wallet -> do
    H.cat $ paymentSKey wallet
    H.cat $ paymentVKey wallet

  return TestnetRuntime
    { configurationFile
    , shelleyGenesisFile = tempAbsPath </> "genesis/shelley/genesis.json"
    , testnetMagic
    , poolNodes = L.zipWith7 PoolNode
        spoNodes
        poolSprockets
        poolStdins
        poolStdouts
        poolStderrs
        poolProcessHandles
        poolKeys
    , wallets = wallets
    , bftNodes = []
    , delegators = delegators
    }
