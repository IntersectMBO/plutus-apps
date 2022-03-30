module Test.PlutusExample.Conf
  ( mkPlutusConf
  ) where

import System.FilePath ((</>))
import Testnet.Conf qualified as H

mkPlutusConf :: FilePath -> Maybe Int -> H.Integration Conf
mkPlutusConf tempAbsPath maybeMagic = do
  conf@H.Conf { base } <- H.mkConf
  return conf
    { H.configurationTemplate = base </> "plutus-example/configuration/configuration.yaml"
    }
