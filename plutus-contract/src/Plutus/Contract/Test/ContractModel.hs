{-# LANGUAGE PatternSynonyms #-}
module Plutus.Contract.Test.ContractModel
    ( -- * Contract models
      --
      -- $contractModel
      ContractModel(..)
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
    , HandleFun
    -- ** Model properties
    , propSanityCheckModel
    -- ** Coverage cheking options
    , CoverageOptions
    , defaultCoverageOptions
    , endpointCoverageReq
    , checkCoverage
    , coverageIndex
    , quickCheckWithCoverage
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
    , checkNoLockedFundsProof
    , checkNoLockedFundsProofFast
    , checkNoLockedFundsProofWithWiggleRoom
    , checkNoLockedFundsProofWithWiggleRoomFast
    -- $checkNoPartiality
    , Whitelist
    , whitelistOk
    , mkWhitelist
    , errorPrefixes
    , defaultWhitelist
    , checkErrorWhitelist
    , checkErrorWhitelistWithOptions
    ) where

import Plutus.Contract.Test.ContractModel.Internal
import Test.QuickCheck.DynamicLogic.Monad qualified as DL
import Test.QuickCheck.DynamicLogic.Quantify
