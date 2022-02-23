{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs           #-}
module Plutus.Contract.Test.Certification where

import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import Plutus.Contract.Test.Coverage
import PlutusTx.Coverage
import Test.Tasty as Tasty

data Instance c m where
  Instance :: c m => Instance c m

data Certification m = Certification {
    certCoverageIndex      :: CoverageIndex,
    certNoLockedFunds      :: Maybe (NoLockedFundsProof m),
    certNoLockedFundsLight :: Maybe (NoLockedFundsProofLight m),
    certUnitTests          :: Maybe (CoverageRef -> TestTree),
    certCrashTolerance     :: Maybe (Instance CrashTolerance m),
    certWhitelist          :: Maybe Whitelist,
    certDLTests            :: [(String, DL m ())]
  }

defaultCertification :: Certification m
defaultCertification = Certification { certCoverageIndex = mempty
                                     , certNoLockedFunds = Nothing
                                     , certNoLockedFundsLight = Nothing
                                     , certUnitTests = Nothing
                                     , certCrashTolerance = Nothing
                                     , certWhitelist = Just defaultWhitelist
                                     , certDLTests = [] }
