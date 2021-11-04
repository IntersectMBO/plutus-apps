module Main
    ( main
    ) where

import Cardano.Api.NetworkId.ExtraSpec qualified
import Cardano.Wallet.ServerSpec qualified
import Control.Concurrent.STM.ExtrasSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ Cardano.Api.NetworkId.ExtraSpec.tests
        , Cardano.Wallet.ServerSpec.tests
        , Control.Concurrent.STM.ExtrasSpec.tests
        ]
