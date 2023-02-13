module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.Marconi.Mamba.Api.Query.Indexers.Utxo qualified as Api.Query.Indexers.Utxo
import Spec.Marconi.Mamba.CLI qualified as CLI

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "marconi-mamba"
  [ Api.Query.Indexers.Utxo.tests
  , CLI.tests
  ]
