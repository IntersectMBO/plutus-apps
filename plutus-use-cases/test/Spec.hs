{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Spec.Auction qualified
import Spec.Crowdfunding qualified
import Spec.Currency qualified
import Spec.ErrorHandling qualified
import Spec.Escrow qualified
import Spec.Future qualified
import Spec.Game.Alonzo qualified
import Spec.Game.Babbage qualified
import Spec.GameStateMachine qualified
import Spec.Governance qualified
import Spec.MultiSig qualified
import Spec.MultiSigStateMachine qualified
import Spec.PingPong qualified
import Spec.Prism qualified
import Spec.PubKey qualified
import Spec.Rollup qualified
import Spec.SealedBidAuction qualified
import Spec.SimpleEscrow qualified
-- import Spec.Stablecoin qualified
import Spec.TokenAccount qualified
import Spec.Uniswap qualified
import Spec.Vesting qualified

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup "use cases" [
    Spec.Crowdfunding.tests,
    Spec.Vesting.tests,
    Spec.ErrorHandling.tests,
    Spec.Future.tests,
    Spec.MultiSig.tests,
    Spec.MultiSigStateMachine.tests,
    Spec.Currency.tests,
    Spec.PubKey.tests,
    Spec.Escrow.tests,
    Spec.SimpleEscrow.tests,
    Spec.Game.Alonzo.tests,
    Spec.Game.Babbage.tests,
    Spec.GameStateMachine.tests,
    Spec.Rollup.tests,
    Spec.TokenAccount.tests,
    Spec.PingPong.tests,
    Spec.Prism.tests,
    -- TODO: should be uncommented after fix of Oracle
    -- Spec.Stablecoin.tests,
    Spec.Auction.tests,
    Spec.SealedBidAuction.tests,
    Spec.Governance.tests,
    Spec.Uniswap.tests
    ]
