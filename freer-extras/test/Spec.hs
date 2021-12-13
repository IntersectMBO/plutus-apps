{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Main(main) where

import Control.Monad.Freer.Extras.BeamSpec qualified as BeamSpec
import Control.Monad.Freer.Extras.PaginationSpec qualified as PaginationSpec

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "tests"
    [ BeamSpec.tests
    , PaginationSpec.tests
    ]

