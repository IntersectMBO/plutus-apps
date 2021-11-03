module Main
    ( main
    ) where

import Auth.TypesSpec qualified
import Playground.THSpec qualified
import Playground.TypesSpec qualified
import SchemaSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ Auth.TypesSpec.tests
        , SchemaSpec.tests
        , Playground.THSpec.tests
        , Playground.TypesSpec.tests
        ]
