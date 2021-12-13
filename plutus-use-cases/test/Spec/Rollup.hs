{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}

module Spec.Rollup where

import Control.Foldl qualified as L
import Control.Monad.Freer (run)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Default (Default (..))
import Data.Text.Encoding (encodeUtf8)

import Plutus.Contract.Trace

import Plutus.Contracts.Crowdfunding
import Spec.GameStateMachine qualified
import Spec.Vesting qualified

import Plutus.Trace.Emulator (EmulatorTrace, runEmulatorStream)
import Streaming.Prelude qualified as S
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (assertFailure)
import Wallet.Emulator.Stream (foldEmulatorStreamM, takeUntilSlot)
import Wallet.Rollup.Render (showBlockchainFold)

tests :: TestTree
tests = testGroup "showBlockchain"
     [ goldenVsString
          "renders a crowdfunding scenario sensibly"
          "test/Spec/renderCrowdfunding.txt"
          (render successfulCampaign)
     , goldenVsString
          "renders a game guess scenario sensibly"
          "test/Spec/renderGuess.txt"
          (render Spec.GameStateMachine.successTrace)
     , goldenVsString
          "renders a vesting scenario sensibly"
          "test/Spec/renderVesting.txt"
          (render Spec.Vesting.retrieveFundsTrace)
     ]

render :: forall a. EmulatorTrace a -> IO ByteString
render trace = do
    let result =
               S.fst'
               $ run
               $ foldEmulatorStreamM (L.generalize (showBlockchainFold knownWallets'))
               $ takeUntilSlot 21
               $ runEmulatorStream def trace
        knownWallets' = fmap (\w -> (mockWalletPaymentPubKeyHash w, w)) knownWallets
    case result of
        Left err       -> assertFailure $ show err
        Right rendered -> pure $ LBS.fromStrict $ encodeUtf8 rendered
