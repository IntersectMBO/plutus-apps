module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.Marconi.Mamba.Api.Query.UtxoIndexer qualified as Spec.Query.UtxoIndexer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "marconi-mamba"
  [Spec.Query.UtxoIndexer.tests]
