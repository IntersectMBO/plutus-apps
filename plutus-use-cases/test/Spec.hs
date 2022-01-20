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
import Spec.SealedBidAuction qualified
import Spec.SimpleEscrow qualified
-- import qualified Spec.MultiSig
import Spec.MultiSigStateMachine qualified
import Spec.PingPong qualified
import Spec.Prism qualified
import Spec.PubKey qualified
import Spec.Rollup qualified
import Spec.Stablecoin qualified
import Spec.TokenAccount qualified
import Spec.Uniswap qualified
import Spec.Vesting qualified

import Test.Tasty
import Test.Tasty.Hedgehog (HedgehogTestLimit (..))

main :: IO ()
main = defaultMain tests

-- | Number of successful tests for each hedgehog property.
--   The default is 100 but we use a smaller number here in order to speed up
--   the test suite.
--
limit :: HedgehogTestLimit
limit = HedgehogTestLimit (Just 5)

tests :: TestTree
tests = localOption limit $ testGroup "use cases" [
    Spec.Crowdfunding.tests,
    Spec.Vesting.tests,
    Spec.ErrorHandling.tests,
    Spec.Future.tests,
    -- disable temporarily, because we need to adopt the signing API
    -- Spec.MultiSig.tests,
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
    Spec.Stablecoin.tests,
    Spec.Auction.tests,
    Spec.SealedBidAuction.tests,
    Spec.Governance.tests,
    Spec.Uniswap.tests
    ]
