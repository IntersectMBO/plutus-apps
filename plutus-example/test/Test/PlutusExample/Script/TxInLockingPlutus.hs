{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}

module Test.PlutusExample.Script.TxInLockingPlutus
  ( hprop_plutus
  ) where

import Control.Monad
import Data.Bool (not)
import Data.Function
import Data.Functor ((<$>))
import Data.Int
import Data.List ((!!))
import Data.Maybe
import Data.Monoid
import Hedgehog (Property, (===))
import Prelude (head)
import System.FilePath ((</>))
import Text.Show (Show (..))

import Data.List qualified as L
import Data.Text qualified as T
import Hedgehog.Extras.Stock.IO.Network.Sprocket qualified as IO
import Hedgehog.Extras.Test.Base qualified as H
import Hedgehog.Extras.Test.File qualified as H
import Hedgehog.Extras.Test.Process qualified as H
import Hedgehog.Internal.Property qualified as H
import System.Directory qualified as IO
import System.Environment qualified as IO
import Test.Base qualified as H
import Test.Process qualified as H
import Test.Runtime qualified as H
import Testnet.Cardano qualified as H
import Testnet.Conf qualified as H

hprop_plutus :: Property
hprop_plutus = H.integration . H.runFinallies . H.workspace "chairman" $ \tempAbsBasePath' -> do
  base <- H.note =<< H.evalIO . IO.canonicalizePath =<< H.getProjectBase
  configurationTemplate <- H.noteShow $ base </> "configuration/defaults/byron-mainnet/configuration.yaml"
  conf@H.Conf { H.tempBaseAbsPath, H.tempAbsPath } <- H.noteShowM $
    H.mkConf (H.ProjectBase base) (H.YamlFilePath configurationTemplate) tempAbsBasePath' Nothing

  resultFile <- H.noteTempFile tempAbsPath "result.out"

  tr@H.TestnetRuntime { H.testnetMagic } <- H.testnet H.defaultTestnetOptions conf

  cardanoCli <- H.binFlex "cardano-cli" "CARDANO_CLI"

  path <- H.evalIO $ fromMaybe "" <$> IO.lookupEnv "PATH"

  let execConfig = H.ExecConfig
        { H.execConfigEnv = Last $ Just
          [ ("CARDANO_CLI", cardanoCli)
          , ("BASE", base)
          , ("WORK", tempAbsPath)
          , ("UTXO_VKEY", tempAbsPath </> "shelley/utxo-keys/utxo1.vkey")
          , ("UTXO_SKEY", tempAbsPath </> "shelley/utxo-keys/utxo1.skey")
          , ("CARDANO_NODE_SOCKET_PATH", IO.sprocketArgumentName $ head $ H.bftSprockets tr)
          , ("TESTNET_MAGIC", show @Int testnetMagic)
          , ("PATH", path)
          , ("RESULT_FILE", resultFile)
          ]
        , H.execConfigCwd = Last $ Just tempBaseAbsPath
        }

  scriptPath <- H.eval $ base </> "plutus-example/plutus/example-txin-locking-plutus-script.sh"

  H.exec_ execConfig H.bashPath
    [ "-x"
    , scriptPath
    ]

  result <- T.pack <$> H.readFile resultFile

  L.filter (not . T.null) (T.splitOn " " (T.lines result !! 2)) !! 2 === "10000000"
