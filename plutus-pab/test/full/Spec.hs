module Main
    ( main
    ) where

import Plutus.PAB.CoreSpec qualified
import Plutus.PAB.Effects.Contract.BuiltinSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ Plutus.PAB.CoreSpec.tests
        , Plutus.PAB.Effects.Contract.BuiltinSpec.tests
        ]
