{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module Main(main, runSplitDataEmulatorTrace) where

import Control.Monad (void)
import Control.Monad.Freer qualified as Eff
import Control.Monad.Freer.Extras.Log (LogLevel (Info))
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Lazy.Char8 (pack)
import Data.Default (def)
import Plutus.Contract.Test (mockWalletAddress, w1, w2)
import Plutus.Trace qualified as Trace
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Golden (goldenVsString)

import BasicApps (LockArgs (LockArgs), splitPlutusApp)

main :: IO ()
main = defaultMain tests

-- BLOCK1

runSplitDataEmulatorTrace :: IO ()
runSplitDataEmulatorTrace = do
    Trace.runEmulatorTraceIO mkSplitDataEmulatorTrace

mkSplitDataEmulatorTrace :: Trace.EmulatorTrace ()
mkSplitDataEmulatorTrace = do
    -- w1, w2, w3, ... are predefined mock wallets used for testing
    let w1Addr = mockWalletAddress w1
    let w2Addr = mockWalletAddress w2

    h <- Trace.activateContractWallet w1 splitPlutusApp
    Trace.callEndpoint @"lock" h $ LockArgs w1Addr w2Addr 10_000_000
    void Trace.nextSlot
    Trace.callEndpoint @"unlock" h $ LockArgs w1Addr w2Addr 10_000_000

-- BLOCK2

tests :: TestTree
tests = testGroup "BasicApps"
    [ testGroup "trace output"
        [ goldenVsString
              "captures a trace"
              "plutus/tutorials/basic-apps-trace.txt"
              (pure captureBasicAppTrace)
        ]
    ]

captureBasicAppTrace :: BSL.ByteString
captureBasicAppTrace = pack $ unlines output
  where
    output = capturePrintEffect $ Trace.runEmulatorTraceEff def def mkSplitDataEmulatorTrace

capturePrintEffect
         :: Eff.Eff '[Trace.PrintEffect] r
         -> [String]
capturePrintEffect effs = snd $ Eff.run (runWriter (Eff.reinterpret f effs))
  where
    f :: Trace.PrintEffect r -> Eff.Eff '[Writer [String]] r
    f = \case
      Trace.PrintLn s -> tell [s]
