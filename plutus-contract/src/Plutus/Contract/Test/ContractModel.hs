-- | This module provides a framework for testing Plutus contracts built on "Test.QuickCheck". The
--   testing is model based, so to test a contract you define a type modelling the state of the
--   contract (or set of contracts) and provide an instance of the `ContractModel` class. This
--   instance specifies what operations (`Action`s) the contract supports, how they interact with
--   the model state, and how to execute them in the blockchain emulator ("Plutus.Trace.Emulator").
--   Tests are evaluated by running sequences of actions (random or user-specified) in the emulator
--   and comparing the state of the blockchain to the model state at the end.
--
--   Test cases are written in the `DL` monad, which supports mixing fixed sequences of actions with
--   random actions, making it easy to write properties like
--   /it is always possible to get all funds out of the contract/.

{-# LANGUAGE PatternSynonyms #-}
module Plutus.Contract.Test.ContractModel
    ( -- * Contract models
      --
      -- $contractModel
      ContractModel(..)
    , HasActions(..)
      -- ** Model state
    , ModelState
    , contractState
    , currentSlot
    , balanceChanges
    , balanceChange
    , minted
    , lockedValue
    , symIsZero
    , GetModelState(..)
    , getContractState
    , askModelState
    , askContractState
    , viewModelState
    , viewContractState
    , SymToken
    , symAssetClassValue
    -- ** The Spec monad
    --
    -- $specMonad
    , Spec
    , wait
    , waitUntil
    , mint
    , burn
    , deposit
    , withdraw
    , transfer
    , modifyContractState
    , createToken
    , assertSpec
    , ($=)
    , ($~)
    -- * Helper functions for writing perform functions
    , SpecificationEmulatorTrace
    , registerToken
    , delay
    -- * Test scenarios
    --
    -- $dynamicLogic
    , DL
    , action
    , waitUntilDL
    , anyAction
    , anyActions
    , anyActions_

    -- ** Failures
    --
    -- $dynamicLogic_errors
    , DL.assert
    , assertModel
    , stopping
    , weight
    , monitor

    -- ** Random generation
    --
    -- $quantify
    , DL.forAllQ
    , Quantification(isaQ)
    , isEmptyQ, generateQ, shrinkQ
    , arbitraryQ, exactlyQ, elementsQ, oneofQ, frequencyQ, mapQ, whereQ, chooseQ
    , validQuantification
    , Quantifiable(..)

    -- * Properties
    --
    -- $runningProperties
    , Actions(..)
    , Act(..)
    , pattern Actions
    , actionsFromList
    -- ** Wallet contract handles
    --
    -- $walletHandles
    , SchemaConstraints
    , ContractInstanceSpec(..)
    , SomeContractInstanceKey(..)
    , StartContract(..)
    , HandleFun
    -- ** Model properties
    , propSanityCheckModel
    , propSanityCheckAssertions
    , propSanityCheckReactive
    -- ** Coverage cheking options
    , CoverageOptions
    , defaultCoverageOptions
    , CoverageRef
    , endpointCoverageReq
    , checkCoverage
    , coverageIndex
    , quickCheckWithCoverage
    , quickCheckWithCoverageAndResult
    -- ** Emulator properties
    , propRunActions_
    , propRunActions
    , propRunActionsWithOptions
    , defaultCheckOptionsContractModel
    -- ** DL properties
    , forAllDL
    -- ** Test cases
    --
    -- $testCases
    , DLTest(..)
    , TestStep(..)
    , FailedStep(..)
    , withDLTest

    -- ** Standard properties
    --
    -- $noLockedFunds
    , NoLockedFundsProof(..)
    , defaultNLFP
    , checkNoLockedFundsProof
    , checkNoLockedFundsProofFast
    , NoLockedFundsProofLight(..)
    , checkNoLockedFundsProofLight
    , checkNoLockedFundsProofWithOptions
    , checkNoLockedFundsProofFastWithOptions
    -- $checkNoPartiality
    , Whitelist
    , whitelistOk
    , mkWhitelist
    , errorPrefixes
    , defaultWhitelist
    , checkErrorWhitelist
    , checkErrorWhitelistWithOptions
    -- * Double satisfaction
    , checkDoubleSatisfaction
    , checkDoubleSatisfactionWithOptions
    ) where

import Plutus.Contract.Test.ContractModel.DoubleSatisfaction
import Plutus.Contract.Test.ContractModel.Internal
import Plutus.Contract.Test.Coverage
import Test.QuickCheck.DynamicLogic.Monad qualified as DL
import Test.QuickCheck.DynamicLogic.Quantify
