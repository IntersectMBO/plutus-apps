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
        [ Plutus.PAB.CliSpec.tests
        ]
