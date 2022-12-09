{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fno-warn-name-shadowing -Wno-orphans #-}


module Plutus.Contract.Test.ContractModel.Interface
    ( -- * Contract models
      --
      -- $contractModel
      ContractModel(..)
    , Actions
    , SomeContractInstanceKey(..)
    , QCCM.HasSymTokens(..)
      -- ** Model state
    , QCCM.ModelState
    , QCCM.contractState
    , currentSlot
    , QCCM.balanceChanges
    , balanceChange
    , QCCM.minted
    , QCCM.lockedValue
    , QCCM.symIsZero
    , QCCM.GetModelState
    , QCCM.viewModelState
    , viewContractState
    , QCCM.SymToken
    , QCCM.SymValueLike(..)
    , QCCM.TokenLike(..)
    , invSymValue
    , QCCM.toValue

    -- ** The Spec monad
    --
    -- $specMonad
    , QCCM.Spec(..)
    , wait
    , waitUntil
    , QCCM.mint
    , QCCM.burn
    , deposit
    , withdraw
    , transfer
    , QCCM.createToken
    , QCCM.assertSpec
    , SpecificationEmulatorTrace
    , registerToken
    , delay

    -- * Test scenarios
    --
    -- $dynamicLogic
    , DL
    , action
    , waitUntilDL
    , QCCM.anyAction
    , QCCM.anyActions
    , QCCM.anyActions_
    , QCD.forAllQ
    , QCD.elementsQ
    , QCD.chooseQ

    -- ** Failures
    --
    -- $dynamicLogic_errors
    , QCD.assert
    , assertModel
    , QCCM.stopping
    , QCCM.weight
    , QCCM.getSize
    , QCCM.monitor

    -- * Properties
    -- ** Wallet contract handles
    --
    -- $walletHandles
    , CMI.SchemaConstraints
    , StartContract(..)
    , HandleFun
    -- ** Model properties
    , propSanityCheckModel
    , propSanityCheckAssertions
    , propSanityCheckReactive
    -- ** Coverage checking options
    , module Coverage
    , CMI.CoverageOptions
    , CMI.defaultCoverageOptions
    , CMI.endpointCoverageReq
    , CMI.checkCoverage
    , CMI.coverageIndex
    , CMI.quickCheckWithCoverage
    , CMI.quickCheckWithCoverageAndResult
    -- ** Emulator properties
    , propRunActions_
    , propRunActions
    , propRunActionsWithOptions
    , CMI.defaultCheckOptionsContractModel
    -- ** DL properties
    , QCCM.forAllDL
    , QCCM.forAllDL_

    -- ** Standard properties
    --
    -- $noLockedFunds
    , NoLockedFundsProof
    , pattern CMI.NoLockedFundsProof
    , CMI.nlfpMainStrategy
    , CMI.nlfpWalletStrategy
    , CMI.nlfpOverhead
    , CMI.nlfpErrorMargin
    , CMI.defaultNLFP
    , CMI.checkNoLockedFundsProof
    , CMI.checkNoLockedFundsProofFast
    , NoLockedFundsProofLight
    , pattern CMI.NoLockedFundsProofLight
    , CMI.nlfplMainStrategy
    , CMI.checkNoLockedFundsProofLight
    , CMI.checkNoLockedFundsProofWithOptions
    , CMI.checkNoLockedFundsProofFastWithOptions
    -- $checkNoPartiality
    , CMI.Whitelist
    , CMI.whitelistOk
    , CMI.mkWhitelist
    , CMI.errorPrefixes
    , CMI.defaultWhitelist
    , checkErrorWhitelist
    , checkErrorWhitelistWithOptions
    -- To make GHC understand coercibility
    , CMI.WithInstances(..)
    , WrappedState(..)
    , checkDoubleSatisfaction
    , checkDoubleSatisfactionWithOptions
    , Generic
    ) where

import Control.Monad.Freer.Reader (ask)
import Control.Monad.Freer.Writer (Writer, runWriter, tell)
import Data.Row (Row)
import Plutus.Trace.Effects.RunContract (walletInstanceTag)
import Plutus.Trace.Emulator.Types (ContractHandle (..), ContractInstanceTag)
import PlutusTx.AssocMap qualified as AssocMap

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Freer (Eff, raise)
import Data.Coerce
import Data.Data
import GHC.Generics hiding (to)

import Cardano.Api qualified as CardanoAPI
import Ledger.Slot
import Plutus.Contract (Contract)
import Plutus.Contract.Test hiding (not)
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value
import Plutus.Trace.Emulator as Trace (BaseEmulatorEffects, waitNSlots)
import PlutusTx.Builtins qualified as Builtins


import Test.QuickCheck hiding (ShrinkState, checkCoverage, getSize, (.&&.), (.||.))
import Test.QuickCheck qualified as QC

import Plutus.Contract.Test.ContractModel.Internal qualified as CMI
import Plutus.Contract.Test.Coverage as Coverage
import Test.QuickCheck.ContractModel qualified as QCCM
import Test.QuickCheck.DynamicLogic qualified as QCD

-- | A function returning the `ContractHandle` corresponding to a `ContractInstanceKey`. A
--   `HandleFun` is provided to the `perform` function to enable calling contract endpoints with
--   `Plutus.Trace.Emulator.callEndpoint`.
type HandleFun state = forall w schema err params. (Typeable w, Typeable schema, Typeable err, Typeable params)
                        => ContractInstanceKey state w schema err params -> ContractHandle w schema err

data StartContract state where
  StartContract :: (CMI.SchemaConstraints w s e, Typeable p) => ContractInstanceKey state w s e p -> p -> StartContract state

type SpecificationEmulatorTrace = Eff (Writer [(String, AssetClass)] ': BaseEmulatorEffects)

-- | A `ContractModel` instance captures everything that is needed to generate and run tests of a
--   contract or set of contracts. It specifies among other things
--
--  * what operations are supported by the contract (`Action`),
--  * when they are valid (`precondition`),
--  * how to generate random actions (`arbitraryAction`),
--  * how the operations affect the state (`nextState`), and
--  * how to run the operations in the emulator (`perform`)

class ( Typeable state
      , Show state
      , Show (Action state)
      , Eq (Action state)
      , QCCM.HasSymTokens (Action state)
      , (forall w s e p. Eq (ContractInstanceKey state w s e p))
      , (forall w s e p. Show (ContractInstanceKey state w s e p))
      ) => ContractModel state where

    -- | The type of actions that are supported by the contract. An action usually represents a single
    --   `Plutus.Trace.Emulator.callEndpoint` or a transfer of tokens, but it can be anything
    --   that can be interpreted in the `EmulatorTrace` monad.
    data Action state

    -- | To be able to call a contract endpoint from a wallet a `ContractHandle` is required. These
    --   are managed by the test framework and all the user needs to do is provide this contract
    --   instance key type representing the different contract instances that a test needs to work
    --   with, and when creating a property (see `propRunActions_`) provide a list of contract
    --   instance keys together with their wallets and contracts (a `ContractInstanceSpec`).
    --   Contract instance keys are indexed by the observable state, schema, and error type of the
    --   contract and should be defined as a GADT. For example, a handle type for a contract with
    --   one seller and multiple buyers could look like this.
    --
    --   >  data ContractInstanceKey MyModel w s e where
    --   >      Buyer  :: Wallet -> ContractInstanceKey MyModel MyObsState MySchema MyError MyParams
    --   >      Seller :: ContractInstanceKey MyModel MyObsState MySchema MyError MyParams
    data ContractInstanceKey state :: * -> Row * -> * -> * -> *

    -- | Get the wallet that the contract running at a specific `ContractInstanceKey` should run
    -- in
    instanceWallet :: ContractInstanceKey state w s e p -> Wallet

    -- | The 'ContractInstanceTag' of an instance key for a wallet. Defaults to 'walletInstanceTag'.
    --   You must override this if you have multiple instances per wallet.
    instanceTag :: forall w s e p. CMI.SchemaConstraints w s e => ContractInstanceKey state w s e p -> ContractInstanceTag
    instanceTag = walletInstanceTag . instanceWallet

    -- | Given the current model state, provide a QuickCheck generator for a random next action.
    --   This is used in the `Arbitrary` instance for `Actions`s as well as by `anyAction` and
    --   `anyActions`.
    arbitraryAction :: QCCM.ModelState state -> Gen (Action state)

    -- | The name of an Action, used to report statistics.
    actionName :: Action state -> String
    actionName = head . words . show

    -- | The probability that we will generate a `WaitUntil` in a given state
    waitProbability :: QCCM.ModelState state -> Double
    waitProbability _ = 0.1

    -- | Control the distribution of how long `WaitUntil` waits
    arbitraryWaitInterval :: QCCM.ModelState state -> Gen Slot
    arbitraryWaitInterval s = Slot <$> choose (1,max 10 (head [ 5*(k-1) | k <- [0..], 2^k > n]))
      where
        CardanoAPI.SlotNo n = s ^. QCCM.currentSlot

    -- | The initial state, before any actions have been performed.
    initialState :: state

    -- | The initial handles
    initialInstances :: [StartContract state]
    initialInstances = []

    -- | The `precondition` function decides if a given action is valid in a given state. Typically
    --   actions generated by `arbitraryAction` will satisfy the precondition, but if they don't
    --   they will be discarded and another action will be generated. More importantly, the
    --   preconditions are used when shrinking (see `shrinkAction`) to ensure that shrunk test cases
    --   still make sense.
    --
    --   If an explicit `action` in a `DL` scenario violates the precondition an error is raised.
    precondition :: QCCM.ModelState state -> Action state -> Bool
    precondition _ _ = True

    -- | `nextReactiveState` is run every time the model `wait`s for a slot to be reached. This
    --   can be used to model reactive components of off-chain code.
    nextReactiveState :: Slot -> QCCM.Spec state ()
    nextReactiveState _ = return ()

    -- | This is where the model logic is defined. Given an action, `nextState` specifies the
    --   effects running that action has on the model state. It runs in the `QCCM.Spec` monad, which is a
    --   state monad over the `QCCM.ModelState`.
    nextState :: Action state -> QCCM.Spec state ()

    -- | Start new contract instances
    startInstances :: QCCM.ModelState state
                   -> Action state
                   -> [StartContract state]
    startInstances _ _ = []

    -- | Map a `ContractInstanceKey` `k` to the `Contract` that is started when we start
    -- `k` in a given `QCCM.ModelState` with a given semantics of `SymToken`s
    instanceContract :: (QCCM.SymToken -> AssetClass)
                     -> ContractInstanceKey state w s e p
                     -> p
                     -> Contract w s e ()

    -- | While `nextState` models the behaviour of the actions, `perform` contains the code for
    --   running the actions in the emulator (see "Plutus.Trace.Emulator"). It gets access to the
    --   wallet contract handles, the current model state, and the action to be performed.
    perform :: HandleFun state  -- ^ Function from `ContractInstanceKey` to `ContractHandle`
            -> (QCCM.SymToken -> AssetClass) -- ^ Map from symbolic tokens (that may appear in actions or the state)
                                             -- to assset class of actual blockchain token
            -> QCCM.ModelState state -- ^ The model state before peforming the action
            -> Action state     -- ^ The action to perform
            -> SpecificationEmulatorTrace ()

    -- | When a test involving random sequences of actions fails, the framework tries to find a
    --   minimal failing test case by shrinking the original failure. Action sequences are shrunk by
    --   removing individual actions, or by replacing an action by one of the (simpler) actions
    --   returned by `shrinkAction`.
    --
    --   See `Test.QuickCheck.shrink` for more information on shrinking.
    shrinkAction :: QCCM.ModelState state -> Action state -> [Action state]
    shrinkAction _ _ = []

    -- | The `monitoring` function allows you to collect statistics of your testing using QuickCheck
    --   functions like `Test.QuickCheck.label`, `Test.QuickCheck.collect`,
    --   `Test.QuickCheck.classify`, and `Test.QuickCheck.tabulate`. This function is called by
    --   `propRunActions` (and friends) for any actions in the given `Actions`.
    --
    --   Statistics on which actions are executed are always collected.
    monitoring :: (QCCM.ModelState state, QCCM.ModelState state)  -- ^ Model state before and after the action
               -> Action state                          -- ^ The action that was performed
               -> Property -> Property
    monitoring _ _ = id

    -- | In some scenarios it's useful to have actions that are never generated randomly, but only
    --   used explicitly in `DL` scenario `action`s. To avoid these actions matching an `anyAction`
    --   when shrinking, they can be marked `restricted`.
    restricted :: Action state -> Bool
    restricted _ = False

newtype WrappedState state = WrapState { unwrapState :: state }
  deriving (Ord, Eq)
  deriving newtype (Show)

deriving instance Eq (ContractInstanceKey state w s e p) => Eq (CMI.ContractInstanceKey (WrappedState state) w s e p)
deriving newtype instance Show (ContractInstanceKey state w s e p) => Show (CMI.ContractInstanceKey (WrappedState state) w s e p)

-- TODO: This instance and the duplicated StartContract above is one of many good indications that going
-- via this indirection is fugly and smelly. Instead of running in `WithInstances (WrappedState state)` we
-- should perhaps ditch this particular indirection and just run in `WithInstances state`. Now, you
-- can't just naively implement that - you would need to remove the `ContractInstanceModel` class and
-- move all the derived machinery from internal to here to make it work. Painful but doable...
instance ContractModel state => CMI.ContractInstanceModel (WrappedState state) where
  newtype ContractInstanceKey (WrappedState state) w s e p =
            WrapContractInstanceKey { unwrapContractInstanceKey :: ContractInstanceKey state w s e p }
  instanceWallet = instanceWallet . unwrapContractInstanceKey
  instanceTag = instanceTag . unwrapContractInstanceKey
  initialInstances = map wrapStartContract $ initialInstances @state
  startInstances m a = map wrapStartContract $ startInstances (fmap unwrapState m) (unwrapAction a)
  instanceContract e k p = instanceContract (fromAssetId . e) (unwrapContractInstanceKey k) p

wrapStartContract :: StartContract state -> CMI.StartContract (WrappedState state)
wrapStartContract (StartContract k p) = CMI.StartContract (WrapContractInstanceKey k) p

deriving instance Eq (Action state) => Eq (QCCM.Action (WrappedState state))
deriving newtype instance Show (Action state) => Show (QCCM.Action (WrappedState state))
deriving instance Generic (QCCM.Action (WrappedState state))

instance ContractModel state => QCCM.ContractModel (WrappedState state) where
  newtype Action (WrappedState state) = WrapAction{ unwrapAction :: Action state }
  arbitraryAction = fmap WrapAction . arbitraryAction . fmap unwrapState
  actionName = actionName . unwrapAction
  waitProbability = waitProbability . fmap unwrapState
  arbitraryWaitInterval = fmap toSlotNo . arbitraryWaitInterval . fmap unwrapState
  initialState = WrapState initialState
  precondition s a = precondition (fmap unwrapState s) (unwrapAction a)
  nextReactiveState = QCCM.coerceSpec . nextReactiveState @state . fromSlotNo
  nextState = QCCM.coerceSpec . nextState . unwrapAction
  shrinkAction s a = WrapAction <$> shrinkAction (fmap unwrapState s) (unwrapAction a)
  restricted = restricted . unwrapAction

instance ContractModel state => QCCM.RunModel (WrappedState state) (CMI.SpecificationEmulatorTrace (WrappedState state)) where
  perform s a e = do
    handles <- lift $ ask @(CMI.Handles (WrappedState state))
    let lookup :: HandleFun state
        lookup key =
          case CMI.imLookup (WrapContractInstanceKey key) handles of
            Just (CMI.WalletContractHandle _ h) -> h
            Nothing                             -> error $ "contractHandle: No handle for " ++ show key
    (_, ts) <- lift $ raise $ runWriter $ perform lookup (fromAssetId . e) (fmap unwrapState s) (unwrapAction a)
    sequence_ [ QCCM.registerToken s (toAssetId ac) | (s, ac) <- ts ]
  monitoring (s, s') a _ _ = monitoring (fmap unwrapState s, fmap unwrapState s')
                                        (unwrapAction a)

toAssetId :: AssetClass -> CardanoAPI.AssetId
toAssetId (AssetClass (sym, tok))
  | sym == Ada.adaSymbol, tok == Ada.adaToken = CardanoAPI.AdaAssetId
  | otherwise                                 = CardanoAPI.AssetId (toPolicyId sym) (toAssetName tok)

toPolicyId :: CurrencySymbol -> CardanoAPI.PolicyId
toPolicyId sym@(CurrencySymbol bs)
  | Just hash <- CardanoAPI.deserialiseFromRawBytes CardanoAPI.AsScriptHash
                                                    (Builtins.fromBuiltin bs) = CardanoAPI.PolicyId hash
  | otherwise = error $ "Bad policy id: " ++ show sym

toAssetName :: TokenName -> CardanoAPI.AssetName
toAssetName (TokenName bs) = CardanoAPI.AssetName $ Builtins.fromBuiltin bs

fromAssetId :: CardanoAPI.AssetId -> AssetClass
fromAssetId CardanoAPI.AdaAssetId            = AssetClass (Ada.adaSymbol, Ada.adaToken)
fromAssetId (CardanoAPI.AssetId policy name) = AssetClass (fromPolicyId policy, fromAssetName name)

fromPolicyId :: CardanoAPI.PolicyId -> CurrencySymbol
fromPolicyId (CardanoAPI.PolicyId hash) = CurrencySymbol . Builtins.toBuiltin $ CardanoAPI.serialiseToRawBytes hash

fromAssetName :: CardanoAPI.AssetName -> TokenName
fromAssetName (CardanoAPI.AssetName bs) = TokenName $ Builtins.toBuiltin bs

fromSlotNo :: CardanoAPI.SlotNo -> Slot
fromSlotNo (CardanoAPI.SlotNo n) = Slot $ fromIntegral n

toSlotNo :: Slot -> CardanoAPI.SlotNo
toSlotNo (Slot n) = CardanoAPI.SlotNo $ fromIntegral n

type RunsIn state = CMI.WithInstances (WrappedState state)

wait :: forall state. ContractModel state => Integer -> QCCM.Spec state ()
wait = QCCM.coerceSpec . QCCM.wait @(RunsIn state)

waitUntil :: forall state. ContractModel state => Slot -> QCCM.Spec state ()
waitUntil = QCCM.coerceSpec . QCCM.waitUntil @(RunsIn state) . toSlotNo

-- | Add tokens to the `balanceChange` of a wallet. The added tokens are subtracted from the
--   `lockedValue` of tokens held by contracts.
deposit :: QCCM.SymValueLike v => Wallet -> v -> QCCM.Spec state ()
deposit = QCCM.deposit . CMI.walletAddress

-- | Withdraw tokens from a wallet. The withdrawn tokens are added to the `lockedValue` of tokens
--   held by contracts.
withdraw :: QCCM.SymValueLike v => Wallet -> v -> QCCM.Spec state ()
withdraw = QCCM.withdraw . CMI.walletAddress

-- | Transfer tokens between wallets, updating their `balances`.
transfer :: QCCM.SymValueLike v
         => Wallet  -- ^ Transfer from this wallet
         -> Wallet  -- ^ to this wallet
         -> v   -- ^ this many tokens
         -> QCCM.Spec state ()
transfer w1 w2 = QCCM.transfer (CMI.walletAddress w1) (CMI.walletAddress w2)

-- | Register the real token corresponding to a symbolic token created
-- in `createToken`.
registerToken :: String -> AssetClass -> SpecificationEmulatorTrace ()
registerToken s ac = tell [(s, ac)]

-- | `delay n` delays emulator execution by `n` slots
delay :: Integer -> SpecificationEmulatorTrace ()
delay = void . Trace.waitNSlots . fromInteger

-- TODO: more evidence that we should torch WithInstances and fuse it with WrappedState
type DL state = QCCM.DL (CMI.WithInstances (WrappedState state))

action :: ContractModel state => Action state -> DL state ()
action = QCCM.action . CMI.UnderlyingAction . WrapAction

waitUntilDL :: ContractModel state => Slot -> DL state ()
waitUntilDL = QCCM.waitUntilDL . toSlotNo

assertModel :: String -> (QCCM.ModelState state -> Bool) -> DL state ()
assertModel s p = QCCM.assertModel s (p . fmap coerce)

type Actions state = QCCM.Actions (CMI.WithInstances (WrappedState state))

-- | Run a `Actions` in the emulator and check that the model and the emulator agree on the final
--   wallet balance changes. Equivalent to
--
-- @
-- propRunActions_ hs actions = `propRunActions` hs (`const` `$` `pure` `True`) actions
-- @
propRunActions_ ::
    ContractModel state
    => Actions state                 -- ^ The actions to run
    -> Property
propRunActions_ actions =
    propRunActions (\ _ -> pure True) actions

-- | Run a `Actions` in the emulator and check that the model and the emulator agree on the final
--   wallet balance changes, and that the given `TracePredicate` holds at the end. Equivalent to:
--
-- @
-- propRunActions = `propRunActionsWithOptions` `defaultCheckOptionsContractModel` `defaultCoverageOptions`
-- @
propRunActions ::
    ContractModel state
    => (QCCM.ModelState state -> TracePredicate) -- ^ Predicate to check at the end
    -> Actions state                         -- ^ The actions to run
    -> Property
propRunActions = propRunActionsWithOptions CMI.defaultCheckOptionsContractModel CMI.defaultCoverageOptions

-- | Run a `Actions` in the emulator and check that the model and the emulator agree on the final
--   wallet balance changes, that no off-chain contract instance crashed, and that the given
--   `TracePredicate` holds at the end. The predicate has access to the final model state.
--
--   The `Actions` argument can be generated by a `forAllDL` from a `DL` scenario, or using the
--   `Arbitrary` instance for actions which generates random actions using `arbitraryAction`:
--
-- >>> quickCheck $ propRunActions_ handles
-- +++ OK, passed 100 tests
-- >>> quickCheck $ forAllDL dl $ propRunActions_ handles
-- +++ OK, passed 100 tests
--
--   The options argument can be used to configure the emulator--setting initial wallet balances,
--   the maximum number of slots to run for, and the log level for the emulator trace printed on
--   failing tests:
--
-- @
-- options :: `Map` `Wallet` `Value` -> `Slot` -> `Control.Monad.Freer.Extras.Log.LogLevel` -> `CheckOptions`
-- options dist slot logLevel =
--     `defaultCheckOptions` `&` `emulatorConfig` . `Plutus.Trace.Emulator.initialChainState` `.~` `Left` dist
--                           `&` `minLogLevel`                        `.~` logLevel
-- @
--
propRunActionsWithOptions ::
    ContractModel state
    => CheckOptions                               -- ^ Emulator options
    -> CMI.CoverageOptions                        -- ^ Coverage options
    -> (QCCM.ModelState state -> TracePredicate)  -- ^ Predicate to check at the end
    -> Actions state                              -- ^ The actions to run
    -> Property
propRunActionsWithOptions opts copts predicate actions =
  CMI.propRunActionsWithOptions opts copts (predicate . fmap coerce) actions

-- | Sanity check a `ContractModel`. Ensures that wallet balances are not always unchanged.
propSanityCheckModel :: forall state. ContractModel state => Property
propSanityCheckModel =
  QC.expectFailure (noBalanceChanges . QCCM.stateAfter @(CMI.WithInstances (WrappedState state)))
  where
    noBalanceChanges s = all QCCM.symIsZero (s ^. QCCM.balanceChanges)

-- | Sanity check a `ContractModel`. Ensures that all assertions in
-- the property generation succeed.
propSanityCheckAssertions :: forall state. ContractModel state => Actions state -> Property
propSanityCheckAssertions as = QCCM.asserts $ QCCM.stateAfter as

-- | Sanity check a `ContractModel`. Ensures that `nextReactiveState` is idempotent.
propSanityCheckReactive :: forall state. (ContractModel state, Eq state) => Actions state -> Positive Integer -> Positive Integer -> Property
propSanityCheckReactive as (Positive sl) (Positive sl') =
    let s0 = QCCM.stateAfter as
        sl1 = s0 ^. QCCM.currentSlot + CardanoAPI.SlotNo (fromIntegral sl)
        sl2 = sl1 + CardanoAPI.SlotNo (fromIntegral sl')
        s1 = QCCM.runSpec (QCCM.coerceSpec $ nextReactiveState @state (fromSlotNo sl1) >> nextReactiveState (fromSlotNo sl2)) (error "unreachable") s0
        s2 = QCCM.runSpec (QCCM.coerceSpec $ nextReactiveState @state (fromSlotNo sl2)) (error "unreachable") s0
    in counterexample "Balance changes not idempotent"        (s1 ^. QCCM.balanceChanges === s2 ^. QCCM.balanceChanges) QC..&&.
       counterexample "Contract state changes not idempotent" (s1 ^. QCCM.contractState  === s2 ^. QCCM.contractState)

fromValue :: Value -> CardanoAPI.Value
fromValue (Value m) = CardanoAPI.valueFromList [ (toAssetId (AssetClass (cls, sym)), fromIntegral n)
                                               | (cls, toks) <- AssocMap.toList m
                                               , (sym, n)    <- AssocMap.toList toks
                                               ]
instance QCCM.SymValueLike Value where
  toSymValue = QCCM.toSymValue . fromValue

instance QCCM.TokenLike AssetClass where
  symAssetIdValueOf v = QCCM.symAssetIdValueOf v . toAssetId
  symAssetIdValue = QCCM.symAssetIdValue . toAssetId

instance QCCM.SymValueLike Ada.Ada where
  toSymValue = QCCM.toSymValue . Ada.toValue

type NoLockedFundsProof state = CMI.NoLockedFundsProof (WrappedState state)
type NoLockedFundsProofLight state = CMI.NoLockedFundsProofLight (WrappedState state)

-- | Get a component of the model state.
askModelState :: (Coercible (QCCM.StateType m) state, QCCM.GetModelState m) => (QCCM.ModelState state -> a) -> m a
askModelState f = (f . fmap coerce) <$> QCCM.getModelState

-- | Get a component of the contract state using a lens.
viewContractState :: (Coercible (QCCM.StateType m) state, QCCM.GetModelState m) => Getting a state a -> m a
viewContractState l = askModelState (view $ QCCM.contractState . l)

currentSlot :: Getter (QCCM.ModelState state) Slot
currentSlot = QCCM.currentSlot . to fromSlotNo

balanceChange :: Wallet -> Getter (QCCM.ModelState state) QCCM.SymValue
balanceChange = QCCM.balanceChange . CMI.walletAddress

-- | Check that running a contract model does not result in validation
-- failures that are not accepted by the whitelist.
checkErrorWhitelist :: ContractModel m
                    => CMI.Whitelist
                    -> Actions m
                    -> Property
checkErrorWhitelist = checkErrorWhitelistWithOptions CMI.defaultCheckOptionsContractModel CMI.defaultCoverageOptions

-- | Check that running a contract model does not result in validation
-- failures that are not accepted by the whitelist.
checkErrorWhitelistWithOptions :: forall m. ContractModel m
                               => CheckOptions
                               -> CMI.CoverageOptions
                               -> CMI.Whitelist
                               -> Actions m
                               -> Property
checkErrorWhitelistWithOptions = CMI.checkErrorWhitelistWithOptions

-- | Perform a light-weight check to find egregious double satisfaction
-- vulnerabilities in contracts.
--
-- A counterexample to this property consists of three transactions.
-- * The first transaction is a valid transaction from the trace generated by the contract model.
-- * The second transaction, generated by redirecting
--   a non-datum pubkey output from a non-signer to a signer in the first transaction,
--   fails to validate. This demonstrates that funds can't simply be stolen.
-- * The third transaction goes through and manages to steal funds by altering the first transaction.
--   It is generated by adding another script input (with the same value as the non-signer non-stealable
--   pubkey output) and adding a datum to the non-signer non-stealable pubkey output, and giving the
--   extra value from the new script input to a signer.
checkDoubleSatisfaction :: forall m. ContractModel m
                        => Actions m
                        -> Property
checkDoubleSatisfaction = checkDoubleSatisfactionWithOptions CMI.defaultCheckOptionsContractModel
                                                             CMI.defaultCoverageOptions

-- | Perform a light-weight check to find egregious double satisfaction
-- vulnerabilities in contracts, with options.
checkDoubleSatisfactionWithOptions :: forall m. ContractModel m
                                   => CheckOptions
                                   -> CMI.CoverageOptions
                                   -> Actions m
                                   -> Property
checkDoubleSatisfactionWithOptions _opts _covopts _acts = error "TODO"

instance QCCM.HasSymTokens Wallet where
  getAllSymTokens _ = mempty
instance QCCM.HasSymTokens Value where
  getAllSymTokens _ = mempty

data SomeContractInstanceKey state where
  Key :: (CMI.SchemaConstraints w s e, Typeable p) => ContractInstanceKey state w s e p -> SomeContractInstanceKey state

instance ContractModel state => Eq (SomeContractInstanceKey state) where
  Key k == Key k' = Just k == cast k'

instance ContractModel state => Show (SomeContractInstanceKey state) where
  showsPrec d (Key k) = showsPrec d k

invSymValue :: QCCM.SymValue -> QCCM.SymValue
invSymValue = QCCM.inv
