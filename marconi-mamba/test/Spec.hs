module Main (main) where

import Spec.Marconi.Mamba.Api.Query.Indexers.Utxo qualified as Api.Query.Indexers.Utxo
import Spec.Marconi.Mamba.CLI qualified as CLI
import Test.Tasty (TestTree, defaultMain, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit))

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = localOption (HedgehogTestLimit $ Just 200) $
    testGroup "marconi-mamba"
        [ Api.Query.Indexers.Utxo.tests
        , CLI.tests
        ]
