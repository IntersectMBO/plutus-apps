module Main
    ( main
    ) where

import Plutus.PAB.CliSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [
        -- TODO: commented because MockNode supports only Emulator tx:
        -- "Cardano.Node.Client: Expecting a mock tx, not an Alonzo tx when publishing it".
        -- Plutus.PAB.CliSpec.tests
        ]
