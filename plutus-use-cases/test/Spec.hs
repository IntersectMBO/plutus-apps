{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Spec.Auction qualified
import Spec.Crowdfunding qualified
import Spec.Currency qualified
import Spec.ErrorHandling qualified
import Spec.Escrow qualified
import Spec.Future qualified
import Spec.Game qualified
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
import Test.Tasty.Hedgehog (HedgehogTestLimit (..))
import Test.Tasty.QuickCheck (QuickCheckTests (QuickCheckTests))

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each property test.
-- You can override this number for a specific property test by using
-- 'Test.Tasty.Quickcheck.withMaxSuccess'.
limit :: Int
limit = 50

tests :: TestTree
tests =
    localOption (HedgehogTestLimit (Just $ fromIntegral limit))
  $ localOption (QuickCheckTests limit)
  $ testGroup "use cases" [
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
    Spec.Game.tests,
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
