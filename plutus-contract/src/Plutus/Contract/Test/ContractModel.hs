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
    , GetModelState(..)
    , getContractState
    , askModelState
    , askContractState
    , viewModelState
    , viewContractState
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
    , ($=)
    , ($~)
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
    -- ** Wallet contract handles
    --
    -- $walletHandles
    , SchemaConstraints
    , ContractInstanceSpec(..)
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
