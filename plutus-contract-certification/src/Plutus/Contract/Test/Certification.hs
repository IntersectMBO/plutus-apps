{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Plutus.Contract.Test.Certification where

import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import PlutusTx.Coverage
import Test.Tasty as Tasty

data Instance c m where
  Instance :: c m => Instance c m

-- | A certification object specifies what tests should be run by the
--   'Plutus.Contract.Test.Certification.Run.certify' function.
data Certification m = Certification {
    certCoverageIndex      :: CoverageIndex,                      -- ^ Coverage locations for on-chain test coverage.
    certNoLockedFunds      :: Maybe (NoLockedFundsProof m),
    certNoLockedFundsLight :: Maybe (NoLockedFundsProofLight m),
    certCrashTolerance     :: Maybe (Instance CrashTolerance m),  -- ^ Contract model for testing robustness against off-chain code crashes.
    certWhitelist          :: Maybe Whitelist,                    -- ^ List of allowed exceptions from on-chain code. Usually `Just 'defaultWhiteList'`.
    certUnitTests          :: Maybe (CoverageRef -> TestTree),    -- ^ Unit tests using "Test.Tasty". See e.g. 'Plutus.Contract.Test.checkPredicateCoverage'.
    certDLTests            :: [(String, DL m ())]                 -- ^ Unit tests using 'Plutus.Contract.Test.ContractModel.DL'.
  }

defaultCertification :: Certification m
defaultCertification = Certification
  { certCoverageIndex      = mempty
  , certNoLockedFunds      = Nothing
  , certNoLockedFundsLight = Nothing
  , certUnitTests          = Nothing
  , certCrashTolerance     = Nothing
  , certWhitelist          = Just defaultWhitelist
  , certDLTests            = []
  }
