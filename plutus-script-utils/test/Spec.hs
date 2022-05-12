{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "all tests" [
        -- TODO: Before writing tests for typed Plutus scripts, we need to define
        -- the ideal testing strategy (Mockchain?, cardano-testnet?).
        testGroup "typed plutus script" [
        ]
    ]
