module Main
    ( main
    ) where

import GistSpec qualified
import Playground.InterpreterSpec qualified
import Playground.UsecasesSpec qualified
import Test.Tasty (defaultMain, testGroup)

main :: IO ()
main =
    defaultMain $
    testGroup
        "all tests"
        [ GistSpec.tests
        , Playground.UsecasesSpec.tests
        , Playground.InterpreterSpec.tests
        ]
