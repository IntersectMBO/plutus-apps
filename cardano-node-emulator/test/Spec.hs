{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main(main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Cardano.Node.Emulator.GeneratorsSpec qualified as GeneratorsSpec
import Cardano.Node.Emulator.MTLSpec qualified as MTLSpec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all tests"
  [ GeneratorsSpec.tests
  , MTLSpec.tests
  ]
