module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Spec.UtxoIndexersQuery qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "marconi-mamba"
  [Spec.UtxoIndexersQuery.tests]
