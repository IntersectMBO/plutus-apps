module Main
    ( main
    ) where

import Cardano.Wallet.LocalClient.ExportTxSpec qualified
import Cardano.Wallet.RemoteClientSpec qualified
import Cardano.Wallet.ServerSpec qualified
import Control.Concurrent.STM.ExtrasSpec qualified
import Plutus.PAB.ArbitrarySpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ Cardano.Wallet.LocalClient.ExportTxSpec.tests
        , Cardano.Wallet.RemoteClientSpec.tests
        , Cardano.Wallet.ServerSpec.tests
        , Control.Concurrent.STM.ExtrasSpec.tests
        , Plutus.PAB.ArbitrarySpec.tests
        ]
