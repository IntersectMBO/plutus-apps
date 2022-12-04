{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where


import Hedgehog (Gen, Property, assert, forAll, property)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as Shelley
import Gen.Cardano.Api.Typed qualified as CGen

import Spec.Cli qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "marconi-mamba"
  [
  -- , Spec.UtxoIndexersQuery.tests
   Spec.Cli.tests
  ]
