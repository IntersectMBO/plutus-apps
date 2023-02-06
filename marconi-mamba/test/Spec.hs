module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.Marconi.Mamba.Api.UtxoIndexersQuery qualified as Spec.UtxoIndexersQuery

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "marconi-mamba"
  [Spec.UtxoIndexersQuery.tests]
