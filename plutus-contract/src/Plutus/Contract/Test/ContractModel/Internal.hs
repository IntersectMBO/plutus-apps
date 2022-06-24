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
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fno-warn-name-shadowing #-}

module Plutus.Contract.Test.ContractModel.Internal
    ( -- * Contract models
      --
      -- $contractModel
      ContractModel(..)
    , HasActions(..)
      -- ** Model state
    , ModelState(..)
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
    , Spec(..)
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
    , getSize
    , monitor

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
    -- ** Coverage checking options
    , CoverageOptions
    , defaultCoverageOptions
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
    , forAllDL_
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
    -- * Internals
    , IMap(..)
    , AssetMap
    , ContractMonadState(..)
    , getEnvContract
    , envContractInstanceTag
    , finalChecks
    , finalPredicate
    , initiateWallets
    , toStateModelActions
    , runEmulatorAction
    , bucket
    ) where


import Control.DeepSeq
import Control.Monad.Freer.Error (Error)
import Plutus.Trace.Effects.Assert (Assert)
import Plutus.Trace.Effects.EmulatedWalletAPI (EmulatedWalletAPI)
import Plutus.Trace.Effects.EmulatorControl (EmulatorControl)
import Plutus.Trace.Effects.RunContract (RunContract)
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator (initialChainState, waitUntilSlot)
import Plutus.Trace.Emulator.Types (ContractHandle (..), ContractInstanceMsg (..), ContractInstanceTag,
                                    EmulatorRuntimeError (..), UserThreadMsg (..), cilMessage)

import PlutusTx.Prelude qualified as P

import Control.Foldl qualified as L
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Freer (Eff, raise, run)
import Control.Monad.Freer.Extras.Log (LogMessage, LogMsg, logMessageContent)
import Control.Monad.Reader
import Control.Monad.State (MonadState, State)
import Control.Monad.State qualified as State
import Control.Monad.Writer qualified as Writer
import Data.Aeson qualified as JSON
import Data.Data
import Data.Foldable
import Data.IORef
import Data.List as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Row (Row)
import Data.Row.Records (labels')
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Plutus.V1.Ledger.Ada qualified as Ada

import Ledger.Index as Index
import Ledger.Scripts
import Ledger.Slot
import Ledger.Value (AssetClass)
import Plutus.Contract (Contract, ContractError, ContractInstanceId, Endpoint, endpoint)
import Plutus.Contract.Schema (Input)
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel.Symbolics
import Plutus.Contract.Test.Coverage
import Plutus.Trace.Effects.EmulatorControl (discardWallets)
import Plutus.Trace.Emulator as Trace (EmulatorTrace, activateContract, callEndpoint, freezeContractInstance,
                                       runEmulatorStream, waitNSlots, walletInstanceTag)
import Plutus.Trace.Emulator.Types (unContractInstanceTag)
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Coverage hiding (_coverageIndex)
import PlutusTx.ErrorCodes
import Streaming qualified as S
import Test.QuickCheck.DynamicLogic.Monad qualified as DL
import Test.QuickCheck.StateModel hiding (Action, Actions (..), actionName, arbitraryAction, initialState, monitoring,
                                   nextState, pattern Actions, perform, precondition, shrinkAction, stateAfter)
import Test.QuickCheck.StateModel qualified as StateModel

import Plutus.Contract.Test.MissingLovelace (calculateDelta)
import Test.QuickCheck hiding (ShrinkState, checkCoverage, getSize, (.&&.), (.||.))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.Monadic (PropertyM, monadic)
import Test.QuickCheck.Monadic qualified as QC

import Wallet.Emulator.Chain hiding (_currentSlot, currentSlot)
import Wallet.Emulator.MultiAgent (EmulatorEvent, eteEvent)
import Wallet.Emulator.Stream (EmulatorErr)

import Wallet.Emulator.Folds (postMapM)
import Wallet.Emulator.Folds qualified as Folds

import Control.Monad.Freer.Reader qualified as Freer
import Control.Monad.Freer.Writer (Writer (..), runWriter, tell)
import Data.Void
import Plutus.Contract.Types (IsContract (..))
import Prettyprinter

import Data.Generics.Uniplate.Data (universeBi)

bucket :: (Num a, Ord a, Show a, Integral a) => a -> a -> [String]
bucket _ 0 = ["0"]
bucket size n | n < size = [ "<" ++ show size ]
              | size <= n, n < size*10 = [bucketIn size n]
              | otherwise = bucket (size*10) n
  where bucketIn size n = let b = n `div` size in show (b*size) ++ "-" ++ show (b*size+(size - 1))

-- | Key-value map where keys and values have three indices that can vary between different elements
--   of the map. Used to store `ContractHandle`s, which are indexed over observable state, schema,
--   and error type.
data IMap (key :: i -> j -> k -> l -> *) (val :: i -> j -> k -> *) where
    IMNil  :: IMap key val
    IMCons :: (Typeable i, Typeable j, Typeable k, Typeable l) => key i j k l -> val i j k -> IMap key val -> IMap key val

-- TODO: Should this make sure we don't duplicate keys?
imAppend :: IMap key val -> IMap key val -> IMap key val
imAppend IMNil m           = m
imAppend (IMCons k v m) m' = IMCons k v (imAppend m m')

-- | Look up a value in an indexed map. First checks that the indices agree, using `cast`. Once the
--   type checker is convinced that the indices match we can check the key for equality.
imLookup :: (Typeable i, Typeable j, Typeable k, Typeable l, Typeable key, Typeable val, Eq (key i j k l)) => key i j k l -> IMap key val -> Maybe (val i j k)
imLookup _ IMNil = Nothing
imLookup k (IMCons key val m) =
    case cast (key, val) of
        Just (key', val') | key' == k -> Just val'
        _                             -> imLookup k m

-- $walletHandles
--
-- In order to call contract endpoints using `Plutus.Trace.Emulator.callEndpoint`, a `ContractHandle`
-- is required. Contract handles are managed behind the scenes by the `propRunActions` functions,
-- based on a given a list of `ContractInstanceSpec`s, associating `ContractInstanceKey`s with `Wallet`s and
-- `Contract`s. Before testing starts, `activateContractWallet` is called for all entries in the
-- list and the mapping from `ContractInstanceKey` to `ContractHandle` is provided in the `HandleFun` argument
-- to `perform`.

-- | The constraints required on contract schemas and error types to enable calling contract
--   endpoints (`Plutus.Trace.Emulator.callEndpoint`).
type SchemaConstraints w schema err =
        ( Typeable w
        , Monoid w
        , JSON.ToJSON w
        , Typeable schema
        , ContractConstraints schema
        , Show err
        , Typeable err
        , JSON.ToJSON err
        , JSON.FromJSON err
        , JSON.ToJSON w
        , JSON.FromJSON w
        )

-- | A `ContractInstanceSpec` associates a `ContractInstanceKey` with a concrete `Wallet` and
--   `Contract`. The contract type parameters are hidden from the outside.
data ContractInstanceSpec state where
    ContractInstanceSpec :: (SchemaConstraints w schema err, Typeable params)
                  => ContractInstanceKey state w schema err params -- ^ The key used when looking up contract instance handles in `perform`
                  -> Wallet                                        -- ^ The wallet who owns the contract instance
                  -> Contract w schema err ()                      -- ^ The contract that is running in the instance
                  -> ContractInstanceSpec state

-- TODO: Here be ugly hacks to make the CrashTolerance stuff less ugly. The crash tolerance stuff can be done without this
-- but then I have to write crap myself and I'm not paid enough to suffer that much!
instance (forall w s e p. Show (ContractInstanceKey state w s e p)) => Show (ContractInstanceSpec state) where
  showsPrec p (ContractInstanceSpec key w _) = showParen (p >= 11) $ showString "ConstractInstanceSpec "
                                                                   . showsPrec 11 key
                                                                   . showString " "
                                                                   . showsPrec 11 w
                                                                   . showString " <Contract>"

instance (Typeable state, forall w s e p. Eq (ContractInstanceKey state w s e p)) => Eq (ContractInstanceSpec state) where
  ContractInstanceSpec key w _ == ContractInstanceSpec key' w' _ = w == w' && cast key == Just key'

data WalletContractHandle w s e = WalletContractHandle Wallet (ContractHandle w s e)

type Handles state = IMap (ContractInstanceKey state) WalletContractHandle

handlesAppend :: Handles state -> Handles state -> Handles state
handlesAppend = imAppend

-- | Used to freeze other wallets when checking a `NoLockedFundsProof`.
instancesForOtherWallets :: Wallet -> Handles state -> [ContractInstanceId]
instancesForOtherWallets _ IMNil = []
instancesForOtherWallets w (IMCons _ (WalletContractHandle w' h) m)
  | w /= w'   = chInstanceId h : instancesForOtherWallets w m
  | otherwise = instancesForOtherWallets w m

activateWallets :: forall state. ContractModel state => (SymToken -> AssetClass) -> [StartContract state] -> EmulatorTrace (Handles state)
activateWallets _ [] = return IMNil
activateWallets sa (StartContract key params : starts) = do
    let wallet = instanceWallet key
    h <- activateContract wallet (instanceContract sa key params) (instanceTag key)
    m <- activateWallets sa starts
    return $ IMCons key (WalletContractHandle wallet h) m

-- | A function returning the `ContractHandle` corresponding to a `ContractInstanceKey`. A
--   `HandleFun` is provided to the `perform` function to enable calling contract endpoints with
--   `Plutus.Trace.Emulator.callEndpoint`.
type HandleFun state = forall w schema err params. (Typeable w, Typeable schema, Typeable err, Typeable params) => ContractInstanceKey state w schema err params -> ContractHandle w schema err

-- | The `ModelState` models the state of the blockchain. It contains,
--
--   * the contract-specific state (`contractState`)
--   * the current slot (`currentSlot`)
--   * the wallet balances (`balances`)
--   * the amount that has been minted (`minted`)
data ModelState state = ModelState
        { _currentSlot    :: Slot
        , _balanceChanges :: Map Wallet SymValue
        , _minted         :: SymValue
        , _symTokens      :: Set SymToken
        , _assertions     :: [(String, Bool)]
        , _assertionsOk   :: Bool
        , _contractState  :: state
        }
  deriving (Show)

instance Functor ModelState where
  fmap f m = m { _contractState = f (_contractState m) }

dummyModelState :: state -> ModelState state
dummyModelState s = ModelState 1 Map.empty mempty mempty mempty True s

-- | The `Spec` monad is a state monad over the `ModelState` with reader and writer components to keep track
--   of newly created symbolic tokens. It is used exclusively by the `nextState` function to model the effects
--   of an action on the blockchain.
newtype Spec state a = Spec { unSpec :: Writer.WriterT [SymToken] (ReaderT (Var AssetKey) (State (ModelState state))) a }
    deriving (Functor, Applicative, Monad)

instance MonadState state (Spec state) where
    state f = Spec $ State.state $ \s -> case f (_contractState s) of
        (a, cs) -> (a, s { _contractState = cs })
    {-# INLINE state #-}

    get = Spec $ fmap _contractState State.get
    {-# INLINE get #-}

    put cs = Spec $ State.modify' $ \s -> s { _contractState = cs }
    {-# INLINE put #-}

data SomeContractInstanceKey state where
  Key :: (SchemaConstraints w s e, Typeable p) => ContractInstanceKey state w s e p -> SomeContractInstanceKey state

data StartContract state where
  StartContract :: (SchemaConstraints w s e, Typeable p) => ContractInstanceKey state w s e p -> p -> StartContract state

instance ContractModel state => Eq (SomeContractInstanceKey state) where
  Key k == Key k' = Just k == cast k'

instance ContractModel state => Show (SomeContractInstanceKey state) where
  showsPrec d (Key k) = showsPrec d k

type SpecificationEmulatorTrace a =
        Eff '[ Writer [(String, AssetClass)]
             , RunContract
             , Assert
             , Waiting
             , EmulatorControl
             , EmulatedWalletAPI
             , LogMsg String
             , Error EmulatorRuntimeError
             ] a

-- $contractModel
--
-- A contract model is a type @state@ with a `ContractModel` instance. The state type should
-- capture an abstraction of the state of the blockchain relevant to the contract (or contracts)
-- under test. During test generation and execution, the contract-specific @state@ is wrapped in the
-- `ModelState` type, which in addition to @state@ tracks common features of the blockchain, like
-- wallet balances and the current slot.

class (Eq (Action state), Show (Action state)) => HasActions state where
  getAllSymtokens :: Action state -> Set SymToken

instance {-# OVERLAPPABLE #-} (Eq (Action state), Show (Action state), Data (Action state)) => HasActions state where
  getAllSymtokens = Set.fromList . universeBi

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
      , HasActions state
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
    instanceTag :: forall w s e p. SchemaConstraints w s e => ContractInstanceKey state w s e p -> ContractInstanceTag
    instanceTag = walletInstanceTag . instanceWallet

    -- | Given the current model state, provide a QuickCheck generator for a random next action.
    --   This is used in the `Arbitrary` instance for `Actions`s as well as by `anyAction` and
    --   `anyActions`.
    arbitraryAction :: ModelState state -> Gen (Action state)

    -- | The name of an Action, used to report statistics.
    actionName :: Action state -> String
    actionName = head . words . show

    -- | The probability that we will generate a `WaitUntil` in a given state
    waitProbability :: ModelState state -> Double
    waitProbability _ = 0.1

    -- | Control the distribution of how long `WaitUntil` waits
    arbitraryWaitInterval :: ModelState state -> Gen Slot
    arbitraryWaitInterval s = Slot <$> choose (1,max 10 (head [ 5*(k-1) | k <- [0..], 2^k > n]))
      where
        Slot n = _currentSlot s

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
    precondition :: ModelState state -> Action state -> Bool
    precondition _ _ = True

    -- | `nextReactiveState` is run every time the model `wait`s for a slot to be reached. This
    --   can be used to model reactive components of off-chain code.
    nextReactiveState :: Slot -> Spec state ()
    nextReactiveState _ = return ()

    -- | This is where the model logic is defined. Given an action, `nextState` specifies the
    --   effects running that action has on the model state. It runs in the `Spec` monad, which is a
    --   state monad over the `ModelState`.
    nextState :: Action state -> Spec state ()

    -- | Start new contract instances
    startInstances :: ModelState state
                   -> Action state
                   -> [StartContract state]
    startInstances _ _ = []

    -- | Map a `ContractInstanceKey` `k` to the `Contract` that is started when we start
    -- `k` in a given `ModelState` with a given semantics of `SymToken`s
    instanceContract :: (SymToken -> AssetClass)
                     -> ContractInstanceKey state w s e p
                     -> p
                     -> Contract w s e ()

    -- | While `nextState` models the behaviour of the actions, `perform` contains the code for
    --   running the actions in the emulator (see "Plutus.Trace.Emulator"). It gets access to the
    --   wallet contract handles, the current model state, and the action to be performed.
    perform :: HandleFun state  -- ^ Function from `ContractInstanceKey` to `ContractHandle`
            -> (SymToken -> AssetClass) -- ^ Map from symbolic tokens (that may appear in actions or the state)
                                        -- to assset class of actual blockchain token
            -> ModelState state -- ^ The model state before peforming the action
            -> Action state     -- ^ The action to perform
            -> SpecificationEmulatorTrace ()

    -- | When a test involving random sequences of actions fails, the framework tries to find a
    --   minimal failing test case by shrinking the original failure. Action sequences are shrunk by
    --   removing individual actions, or by replacing an action by one of the (simpler) actions
    --   returned by `shrinkAction`.
    --
    --   See `Test.QuickCheck.shrink` for more information on shrinking.
    shrinkAction :: ModelState state -> Action state -> [Action state]
    shrinkAction _ _ = []

    -- | The `monitoring` function allows you to collect statistics of your testing using QuickCheck
    --   functions like `Test.QuickCheck.label`, `Test.QuickCheck.collect`,
    --   `Test.QuickCheck.classify`, and `Test.QuickCheck.tabulate`. This function is called by
    --   `propRunActions` (and friends) for any actions in the given `Actions`.
    --
    --   Statistics on which actions are executed are always collected.
    monitoring :: (ModelState state, ModelState state)  -- ^ Model state before and after the action
               -> Action state                          -- ^ The action that was performed
               -> Property -> Property
    monitoring _ _ = id

    -- | In some scenarios it's useful to have actions that are never generated randomly, but only
    --   used explicitly in `DL` scenario `action`s. To avoid these actions matching an `anyAction`
    --   when shrinking, they can be marked `restricted`.
    restricted :: Action state -> Bool
    restricted _ = False

-- | Lens for the contract-specific part of the model state.
--
--   `Spec` monad update functions: `$=` and `$~`.
makeLensesFor [("_contractState",  "contractState")]   'ModelState
makeLensesFor [("_currentSlot",    "currentSlotL")]    'ModelState
makeLensesFor [("_lastSlot",       "lastSlotL")]       'ModelState
makeLensesFor [("_balanceChanges", "balanceChangesL")] 'ModelState
makeLensesFor [("_minted",         "mintedL")]         'ModelState
makeLensesFor [("_tokenNameIndex", "tokenNameIndex")]  'ModelState
makeLensesFor [("_assertions", "assertions")]          'ModelState
makeLensesFor [("_assertionsOk", "assertionsOk")]      'ModelState
makeLensesFor [("_symTokens", "symTokens")]            'ModelState

-- | Get the current slot.
--
--   `Spec` monad update functions: `wait` and `waitUntil`.
currentSlot :: Getter (ModelState state) Slot
currentSlot = currentSlotL

-- | Get the current wallet balance changes. These are delta balances, so they start out at zero and
--   can be negative. The absolute balances used by the emulator can be set in the `CheckOptions`
--   argument to `propRunActionsWithOptions`.
--
--   `Spec` monad update functions: `withdraw`, `deposit`, `transfer`.
balanceChanges :: Getter (ModelState state) (Map Wallet SymValue)
balanceChanges = balanceChangesL

-- | Get the current balance change for a wallet. This is the delta balance, so it starts out at zero and
--   can be negative. The absolute balance used by the emulator can be set in the `CheckOptions`
--   argument to `propRunActionsWithOptions`.
--
--   `Spec` monad update functions: `withdraw`, `deposit`, `transfer`.
balanceChange :: Wallet -> Getter (ModelState state) SymValue
balanceChange w = balanceChangesL . at w . non mempty

-- | Get the amount of tokens minted so far. This is used to compute `lockedValue`.
--
--   `Spec` monad update functions: `mint` and `burn`.
minted :: Getter (ModelState state) SymValue
minted = mintedL

-- | How much value is currently locked by contracts. This computed by subtracting the wallet
--   `balances` from the `minted` value.
lockedValue :: ModelState s -> SymValue
lockedValue s = s ^. minted <> inv (fold $ s ^. balanceChanges)

-- | Monads with read access to the model state: the `Spec` monad used in `nextState`, and the `DL`
--   monad used to construct test scenarios.
class Monad m => GetModelState m where
    -- | The contract state type of the monad. For both `Spec` and `DL` this is simply the @state@
    --   parameter of the respective monad.
    type StateType m :: *

    -- | Get the current model state.
    getModelState :: m (ModelState (StateType m))

-- | Get the contract state part of the model state.
getContractState :: GetModelState m => m (StateType m)
getContractState = _contractState <$> getModelState

-- | Get a component of the model state.
askModelState :: GetModelState m => (ModelState (StateType m) -> a) -> m a
askModelState f = f <$> getModelState

-- | Get a component of the contract state.
askContractState :: GetModelState m => (StateType m -> a) -> m a
askContractState f = askModelState (f . _contractState)

-- | Get a component of the model state using a lens.
viewModelState :: GetModelState m => Getting a (ModelState (StateType m)) a -> m a
viewModelState l = askModelState (^. l)

-- | Get a component of the contract state using a lens.
viewContractState :: GetModelState m => Getting a (StateType m) a -> m a
viewContractState l = viewModelState (contractState . l)

-- $specMonad
--
-- The `Spec` monad is used in the `nextState` function to specify how the model state is affected
-- by each action.
--
-- Note that the model state does not track the absolute `balances` of each wallet, only how the
-- balance changes over the execution of a contract. Thus, token transfers (using `transfer`,
-- `deposit` or `withdraw`) always succeed in the model, but might fail when running the
-- contract in the emulator, causing test failures. The simplest way to deal with this is
-- to make sure that each wallet has enough starting funds to cover any scenario encountered during
-- testing. The starting funds can be provided in the `CheckOptions` argument to
-- `propRunActionsWithOptions`.
-- Another option is to model the starting funds of each contract in the contract state and check
-- that enough funds are available before performing a transfer.

runSpec :: Spec state ()
        -> Var AssetKey
        -> ModelState state
        -> ModelState state
runSpec (Spec spec) v s = flip State.execState s $ do
  w <- runReaderT (snd <$> Writer.runWriterT spec) v
  symTokens %= (Set.fromList w <>)

-- | Check if a given action creates new symbolic tokens in a given `ModelState`
createsTokens :: ContractModel state
              => ModelState state
              -> Action state
              -> Bool
createsTokens s a = ([] /=) $ State.evalState (runReaderT (snd <$> Writer.runWriterT (unSpec (nextState a))) (Var 0)) s

-- | Modify a field in the `ModelState`
modState :: forall state a. Setter' (ModelState state) a -> (a -> a) -> Spec state ()
modState l f = Spec $ State.modify $ over l f

-- | Wait the given number of slots. Updates the `currentSlot` of the model state.
wait :: ContractModel state => Integer -> Spec state ()
wait 0 = return ()
wait n = do
  Slot now <- viewModelState currentSlot
  nextReactiveState (Slot $ now + n)
  modState currentSlotL (const (Slot $ now + n))

-- | Wait until the given slot. Has no effect if `currentSlot` is greater than the given slot.
waitUntil :: ContractModel state => Slot -> Spec state ()
waitUntil (Slot n) = do
  Slot now <- viewModelState currentSlot
  when (now < n) $ do
    wait (n - now)

-- | Mint tokens. Minted tokens start out as `lockedValue` (i.e. owned by the contract) and can be
--   transferred to wallets using `deposit`.
mint :: SymValueLike v => v -> Spec state ()
mint v = modState mintedL (<> toSymValue v)

-- | Burn tokens. Equivalent to @`mint` . `inv`@.
burn :: SymValueLike v => v -> Spec state ()
burn = mint . inv . toSymValue

-- | Add tokens to the `balanceChange` of a wallet. The added tokens are subtracted from the
--   `lockedValue` of tokens held by contracts.
deposit :: SymValueLike v => Wallet -> v -> Spec state ()
deposit w val = modState (balanceChangesL . at w) (Just . maybe (toSymValue val) (<> toSymValue val))

-- | Withdraw tokens from a wallet. The withdrawn tokens are added to the `lockedValue` of tokens
--   held by contracts.
withdraw :: SymValueLike v => Wallet -> v -> Spec state ()
withdraw w val = deposit w (inv . toSymValue $ val)

-- | Transfer tokens between wallets, updating their `balances`.
transfer :: SymValueLike v
         => Wallet  -- ^ Transfer from this wallet
         -> Wallet  -- ^ to this wallet
         -> v   -- ^ this many tokens
         -> Spec state ()
transfer fromW toW val = withdraw fromW val >> deposit toW val

-- | Assert that a particular predicate holds at a point in the specification
assertSpec :: String -> Bool -> Spec state ()
assertSpec s b = do
  modState assertions ((s, b):)
  modState assertionsOk (&&b)

-- | Modify the contract state.
modifyContractState :: (state -> state) -> Spec state ()
modifyContractState f = modState contractState f

-- | Set a specific field of the contract state.
($=) :: Setter' state a -> a -> Spec state ()
($=) = (.=)

-- | Modify a specific field of the contract state.
($~) :: Setter' state a -> (a -> a) -> Spec state ()
($~) = (%=)

-- | Create a new symbolic token in `nextState` - must have a
-- corresponding `registerToken` call in `perform`
createToken :: String -> Spec state SymToken
createToken key = Spec $ do
  var <- ask
  Writer.tell [SymToken var key]
  pure $ SymToken var key

-- | Register the real token corresponding to a symbolic token created
-- in `createToken`.
registerToken :: String -> AssetClass -> SpecificationEmulatorTrace ()
registerToken s ac = tell [(s, ac)]

-- | `delay n` delays emulator execution by `n` slots
delay :: Integer -> SpecificationEmulatorTrace ()
delay = void . Trace.waitNSlots . fromInteger

instance GetModelState (Spec state) where
    type StateType (Spec state) = state
    getModelState = Spec State.get

handle :: ContractModel s => Handles s -> HandleFun s
handle handles key =
    case imLookup key handles of
        Just (WalletContractHandle _ h) -> h
        Nothing                         -> error $ "handle: No handle for " ++ show key

type AssetMap = Map AssetKey (Map String AssetClass)

-- | The `EmulatorTrace` monad does not let you get the result of a computation out, but the way
--   "Test.QuickCheck.Monadic" is set up requires you to provide a function @m Property -> Property@.
--   This means that we can't use `EmulatorTrace` as the action monad in the `StateModel`. Instead
--   we use a state monad that builds up an `EmulatorTrace` computation to be executed at the end
--   (by `finalChecks`). We also need access to the contract handles, so what we are building is a
--   function from the handles to an emulator trace computation returning potentially updated
--   handles.

-- TODO: Refactor this(!)
type EmulatorMonad = State.StateT AssetMap EmulatorTrace
newtype EmulatorAction state = EmulatorAction { runEmulatorAction :: Handles state -> EmulatorMonad (Handles state) }

instance Semigroup (EmulatorAction state) where
    EmulatorAction f <> EmulatorAction g = EmulatorAction (f >=> g)

instance Monoid (EmulatorAction state) where
    mempty  = EmulatorAction pure
    mappend = (<>)

type ContractMonad state = State.State (ContractMonadState state)

data ContractMonadState state = ContractMonadState { _cmsEmulatorAction    :: (EmulatorAction state)
                                                   , _cmsContractInstances :: [SomeContractInstanceKey state]
                                                   , _cmsNextVarIdx        :: AssetKey }

makeLenses ''ContractMonadState

instance Semigroup (ContractMonadState state) where
    ContractMonadState f xs idx <> ContractMonadState g ys idx' = ContractMonadState (f <> g) (xs <> ys) (max idx idx')

instance Monoid (ContractMonadState state) where
    mempty = ContractMonadState mempty mempty 0

runEmulator_ :: (Handles state -> EmulatorMonad ()) -> ContractMonad state ()
runEmulator_ a = cmsEmulatorAction %= (<> EmulatorAction (\ h -> h <$ a h))

runEmulator :: (Handles state -> EmulatorMonad (Handles state)) -> ContractMonad state ()
runEmulator a = cmsEmulatorAction %= (<> EmulatorAction (\ h -> a h))

addInstances :: [StartContract state] -> ContractMonad state ()
addInstances starts = cmsContractInstances <>= [Key key | StartContract key _ <- starts]

setHandles :: EmulatorMonad (Handles state) -> ContractMonad state ()
setHandles a = cmsEmulatorAction %= (<> EmulatorAction (const a))

makeVariable :: ContractMonad state AssetKey
makeVariable = do
  i <- use cmsNextVarIdx
  cmsNextVarIdx += 1
  pure i

contractAction :: ContractModel state => ModelState state -> Action state -> StateModel.Action (ModelState state) AssetKey
contractAction s a = ContractAction (createsTokens s a) a

instance ContractModel state => Show (StateModel.Action (ModelState state) a) where
    showsPrec p (ContractAction _ a) = showsPrec p a
    showsPrec p (WaitUntil n)        = showParen (p >= 11) $ showString "WaitUntil " . showsPrec 11 n
    showsPrec p (Unilateral w)       = showParen (p >= 11) $ showString "Unilateral " . showsPrec 11 w

deriving instance ContractModel state => Eq (StateModel.Action (ModelState state) a)

instance ContractModel state => StateModel (ModelState state) where

    data Action (ModelState state) a where
        ContractAction :: Bool -> Action state -> StateModel.Action (ModelState state) AssetKey
        Unilateral :: Wallet -> StateModel.Action (ModelState state) ()
          -- ^ This action disables all wallets other than the given wallet, by freezing their
          --   contract instances and removing their private keys from the emulator state. This can
          --   be used to check that a wallet can *unilaterally* achieve a desired outcome, without
          --   the help of other wallets.
        WaitUntil :: Slot -> StateModel.Action (ModelState state) ()

    type ActionMonad (ModelState state) = ContractMonad state

    actionName (ContractAction _ act) = actionName act
    actionName (Unilateral _)         = "Unilateral"
    actionName (WaitUntil _)          = "WaitUntil"

    arbitraryAction s =
        -- TODO: do we need some way to control the distribution
        -- between actions and waits here?
        frequency [(floor $ 100.0*(1.0-waitProbability s), do a <- arbitraryAction s
                                                              return (Some (ContractAction (createsTokens s a) a)))
                  ,(floor $ 100.0*waitProbability s, Some . WaitUntil . step <$> arbitraryWaitInterval s)]
        where
            slot = s ^. currentSlot
            step n = slot + n

    shrinkAction s (ContractAction _ a) =
      [ Some (WaitUntil (Slot n')) | let Slot n = runSpec (nextState a) (Var 0) s ^. currentSlot
                                   , n' <- n : shrink n
                                   , Slot n' > s ^. currentSlot ] ++
      [ Some (contractAction s a') | a' <- shrinkAction s a ]
    shrinkAction s (WaitUntil (Slot n))        =
      [ Some (WaitUntil (Slot n')) | n' <- shrink n, Slot n' > s ^. currentSlot ]
    shrinkAction _ _                    = []

    initialState = ModelState { _currentSlot      = 1
                              , _balanceChanges   = Map.fromList [(w,mempty) | w <- knownWallets]
                              , _minted           = mempty
                              , _assertions       = mempty
                              , _assertionsOk     = True
                              , _symTokens        = mempty
                              , _contractState    = initialState
                              }

    nextState s (ContractAction _ cmd) v = runSpec (nextState cmd) v s
    nextState s (WaitUntil n) _          = runSpec (() <$ waitUntil n) (error "unreachable") s
    nextState s Unilateral{} _           = s

    -- Note that the order of the preconditions in this case matter - we want to run
    -- `getAllSymtokens` last because its likely to be stricter than the user precondition
    -- and so if the user relies on the lazyness of the Gen monad by using the precondition
    -- to avoid duplicate checks in the precondition and generator we don't screw that up.
    precondition s (ContractAction _ cmd) = s ^. assertionsOk
                                          && precondition s cmd
                                          && getAllSymtokens cmd `Set.isSubsetOf` (s ^. symTokens)
    precondition s (WaitUntil n)          = n > s ^. currentSlot
    precondition _ _                      = True

    perform s (ContractAction _ cmd) envOuter = do
      let newKeys = startInstances s cmd
      addInstances newKeys
      v <- makeVariable
      runEmulator $ \ h -> do
        envInner <- State.get
        let lookup (SymToken outerVar idx) = case Map.lookup idx $ fold (Map.lookup (envOuter outerVar) envInner) of
              Just tok -> tok
              Nothing  -> error $ "Missing registerToken call for token: " ++ show idx
        newHandles <- lift $ activateWallets lookup newKeys
        let h' = handlesAppend newHandles h
        (_, result) <- lift . raise . runWriter $ perform (handle h') lookup s cmd
        -- Ensure that each call to `createToken` in the spec corresponds to a call to
        -- `registerToken` during execution
        when (Set.size (Set.fromList . map fst $ result) /= length result) $ do
          error $ "Non-unique registered token in call to perform with tokens: " ++ show result
        State.modify (Map.insert v (Map.fromList result))
        return h'
      return v
    perform _ (Unilateral w) _env = runEmulator_ $ \ h -> lift $ do
      let insts = instancesForOtherWallets w h
      mapM_ freezeContractInstance insts
      discardWallets (w /=)
    perform _ (WaitUntil slot) _ = runEmulator_ $ \ _ -> lift . void $ waitUntilSlot slot

    postcondition _s _cmd _env _res = True

    monitoring (s0, s1) (ContractAction _ cmd) _env _res = monitoring (s0, s1) cmd
    monitoring (s0, _) (WaitUntil n@(Slot _n)) _ _                 =
      tabulate "Wait interval" (bucket 10 diff) .
      tabulate "Wait until" (bucket 10 _n)
      where Slot diff = n - s0 ^. currentSlot
    monitoring _ _ _ _ = id

-- We present a simplified view of test sequences, and DL test cases, so
-- that users do not need to see the variables bound to results.

-- $testCases
--
-- Failing `DL` tests can be rechecked using `withDLTest`. The easiest way to do this is to copy and
-- paste the `DLTest` printed on failure into a source file. For instance, suppose
-- @prop_Finish@ from the `forAllDL` example fails with @`BadPrecondition` ...@. You could copy this
-- into your source file and define the property
--
-- @
-- failedTest :: `DLTest` AuctionState
-- failedTest = `BadPrecondition` ...
--
-- checkFailed :: Property
-- checkFailed = `withMaxSuccess` 1 $ `withDLTest` finishAuction prop_Auction failedTest
-- @
--
-- Now the failing test can be rerun to check if changes code or model has fixed the problem.

-- | A `Actions` is a list of intelligent and sophisticated `Action`s.

-- We include a list of rejected action names.
data Actions s = Actions_ [String] (Smart [Act s])

{-# COMPLETE Actions #-}
pattern Actions :: [Act s] -> Actions s
pattern Actions as <- Actions_ _ (Smart _ as) where
  Actions as = Actions_ [] (Smart 0 as)

data Act s = Bind {varOf :: Var AssetKey, actionOf :: Action s }
           | NoBind {varOf :: Var AssetKey, actionOf :: Action s}
           | ActWaitUntil (Var ()) Slot

deriving instance ContractModel s => Eq (Act s)

isBind :: Act s -> Bool
isBind Bind{} = True
isBind _      = False

actionsFromList :: [Action s] -> Actions s
actionsFromList = Actions . zipWith NoBind (Var <$> [0..])

varNumOf :: Act s -> Int
varNumOf (ActWaitUntil (Var i) _) = i
varNumOf act | Var i <- varOf act = i

instance ContractModel state => Show (Act state) where
  showsPrec d (Bind (Var i) a)   = showParen (d >= 11) $ showString ("tok" ++ show i ++ " := ") . showsPrec 0 a
  showsPrec d (ActWaitUntil _ n) = showParen (d >= 11) $ showString ("WaitUntil ") . showsPrec 11 n
  showsPrec d (NoBind _ a)       = showsPrec d a

instance ContractModel state => Show (Actions state) where
  showsPrec d (Actions as)
    | d>10      = ("("++).showsPrec 0 (Actions as).(")"++)
    | null as   = ("Actions []"++)
    | otherwise = ("Actions \n [" ++) .
                  foldr (.) (showsPrec 0 (last as) . ("]"++))
                    [showsPrec 0 a . (",\n  "++) | a <- init as]

instance ContractModel s => Arbitrary (Actions s) where
  arbitrary = fromStateModelActions <$> arbitrary
  shrink = map fromStateModelActions . shrink . toStateModelActions

toStateModelActions :: ContractModel state =>
                        Actions state -> StateModel.Actions (ModelState state)
toStateModelActions (Actions_ rs (Smart k s)) =
  StateModel.Actions_ rs (Smart k $ map mkStep s)
    where mkStep (ActWaitUntil v n) = v := WaitUntil n
          mkStep act                = varOf act := ContractAction (isBind act) (actionOf act)

fromStateModelActions :: StateModel.Actions (ModelState s) -> Actions s
fromStateModelActions (StateModel.Actions_ rs (Smart k s)) =
  Actions_ rs (Smart k (catMaybes $ map mkAct s))
  where
    mkAct :: Step (ModelState s) -> Maybe (Act s)
    mkAct (Var i := ContractAction b act) = Just $ if b then Bind (Var i) act else NoBind (Var i) act
    mkAct (v     := WaitUntil n)          = Just $ ActWaitUntil v n
    mkAct (_     := Unilateral{})         = Nothing

-- | An instance of a `DL` scenario generated by `forAllDL`. It is turned into a `Actions` before
--   being passed to the property argument of `forAllDL`, but in case of a failure the generated
--   `DLTest` is printed. This test can then be rerun using `withDLTest`.
data DLTest state =
    BadPrecondition [TestStep state] [FailedStep state] state
        -- ^ An explicit `action` failed its precondition (@[Action](#v:Action)@), or an assertion failed (`Assert`).
        --   There is a list of `FailedStep`s because there may be multiple branches
        --   (`Control.Applicative.<|>`) in the scenario that fail. Contains the contract state at
        --   the point of failure.
  | Looping         [TestStep state]
        -- ^ Test case generation from the `DL` scenario failed to terminate. See `stopping` for
        --   more information.
  | Stuck           [TestStep state] state
        -- ^ There are no possible next steps in the scenario. Corresponds to a call to
        --  `Control.Applicative.empty`. Contains the contract state at the point where the scenario
        --  got stuck.
  | DLScript        [TestStep state]
        -- ^ A successfully generated test case.

-- | This type captures the two different kinds of `BadPrecondition`s that can occur.
data FailedStep state = Action (Act state)
                        -- ^ A call to `action` that does not satisfy its `precondition`.
                      | Assert String
                        -- ^ A call to `DL.assert` or `assertModel` failed, or a `fail` in the `DL`
                        --   monad. Stores the string argument of the corresponding call.

deriving instance ContractModel s => Show (FailedStep s)
instance ContractModel s => Eq (FailedStep s) where
  Assert s == Assert s'                                   = s == s'
  Action (ActWaitUntil _ n) == Action (ActWaitUntil _ n') = n == n'
  Action a == Action a'                                   = actionOf a == actionOf a'
  _ == _                                                  = False

instance ContractModel s => Show (DLTest s) where
    show (BadPrecondition as bads s) =
        unlines $ ["BadPrecondition"] ++
                  bracket (map show as) ++
                  ["  " ++ show (nub bads)] ++
                  ["  " ++ showsPrec 11 s ""]
    show (Looping as) =
        unlines $ ["Looping"] ++ bracket (map show as)
    show (Stuck as s) =
        unlines $ ["Stuck"] ++ bracket (map show as) ++ ["  " ++ showsPrec 11 s ""]
    show (DLScript as) =
        unlines $ ["DLScript"] ++ bracket (map show as)

bracket :: [String] -> [String]
bracket []  = ["  []"]
bracket [s] = ["  [" ++ s ++ "]"]
bracket (first:rest) = ["  ["++first++", "] ++
                       map (("   "++).(++", ")) (init rest) ++
                       ["   " ++ last rest ++ "]"]

-- | One step of a test case. Either an `Action` (`Do`) or a value generated by a `DL.forAllQ`
--   (`Witness`). When a `DLTest` is turned into a `Actions` to be executed the witnesses are
--   stripped away.
data TestStep s = Do (Act s)
                | forall a. (Eq a, Show a, Typeable a) => Witness a

instance ContractModel s => Show (TestStep s) where
  show (Do act)    = "Do $ "++show act
  show (Witness a) = "Witness ("++show a++" :: "++show (typeOf a)++")"

toDLTest :: ContractModel state =>
              DLTest state -> DL.DynLogicTest (ModelState state)
toDLTest (BadPrecondition steps acts s) =
  DL.BadPrecondition (toDLTestSteps steps) (map conv acts) (dummyModelState s)
    where
        conv (Action (ActWaitUntil _ n)) = Some (WaitUntil n)
        conv (Action a)                  = Some (ContractAction (isBind a) (actionOf a))
        conv (Assert e)                  = Error e
toDLTest (Looping steps) =
  DL.Looping (toDLTestSteps steps)
toDLTest (Stuck steps s) =
  DL.Stuck (toDLTestSteps steps) (dummyModelState s)
toDLTest (DLScript steps) =
  DL.DLScript (toDLTestSteps steps)

toDLTestSteps :: ContractModel state =>
                   [TestStep state] -> [DL.TestStep (ModelState state)]
toDLTestSteps steps = map toDLTestStep steps

toDLTestStep :: ContractModel state =>
                  TestStep state -> DL.TestStep (ModelState state)
toDLTestStep (Do (ActWaitUntil v n)) = DL.Do $ v StateModel.:= WaitUntil n
toDLTestStep (Do act)                = DL.Do $ varOf act StateModel.:= ContractAction (isBind act) (actionOf act)
toDLTestStep (Witness a)             = DL.Witness a

fromDLTest :: forall s. DL.DynLogicTest (ModelState s) -> DLTest s
fromDLTest (DL.BadPrecondition steps acts s) =
  BadPrecondition (fromDLTestSteps steps) (concatMap conv acts) (_contractState s)
  where conv :: Any (StateModel.Action (ModelState s)) -> [FailedStep s]
        conv (Some (ContractAction _ act)) = [Action $ NoBind (Var 0) act]
        conv (Some (WaitUntil n))          = [Action $ ActWaitUntil (Var 0) n]
        conv (Some Unilateral{})           = []
        conv (Error e)                     = [Assert e]
fromDLTest (DL.Looping steps) =
  Looping (fromDLTestSteps steps)
fromDLTest (DL.Stuck steps s) =
  Stuck (fromDLTestSteps steps) (_contractState s)
fromDLTest (DL.DLScript steps) =
  DLScript (fromDLTestSteps steps)

fromDLTestSteps :: [DL.TestStep (ModelState state)] -> [TestStep state]
fromDLTestSteps steps = concatMap fromDLTestStep steps

fromDLTestStep :: DL.TestStep (ModelState state) -> [TestStep state]
fromDLTestStep (DL.Do (v := ContractAction b act)) = [Do $ if b then Bind v act else NoBind v act]
fromDLTestStep (DL.Do (v := WaitUntil n))          = [Do $ ActWaitUntil v n]
fromDLTestStep (DL.Do (_ := Unilateral{}))         = []
fromDLTestStep (DL.Witness a)                      = [Witness a]

-- | Run a specific `DLTest`. Typically this test comes from a failed run of `forAllDL`
--   applied to the given `DL` scenario and property. Useful to check if a particular problem has
--   been fixed after updating the code or the model.
withDLTest :: (ContractModel state, Testable prop)
           => DL state ()              -- ^ The `DL` scenario
           -> (Actions state -> prop)   -- ^ The property. Typically a call to `propRunActions_`
           -> DLTest state             -- ^ The specific test case to run
           -> Property
withDLTest dl prop test = DL.withDLTest dl (prop . fromStateModelActions) (toDLTest test)

-- $dynamicLogic
--
-- Test scenarios are described in the `DL` monad (based on dynamic logic) which lets you freely mix
-- random sequences of actions (`anyAction`, `anyActions_`, `anyActions`) with specific
-- actions (`action`). It also supports checking properties of the model state (`DL.assert`,
-- `assertModel`), and random generation (`DL.forAllQ`).
--
-- For instance, a unit test for a simple auction contract might look something like this:
--
-- @
--  unitTest :: `DL` AuctionState ()
--  unitTest = do
--      `action` $ Bid w1 100
--      `action` $ Bid w2 150
--      `action` $ Wait endSlot
--      `action` $ Collect
-- @
--
--  and could easily be extended with some randomly generated values
--
-- @
--  unitTest :: `DL` AuctionState ()
--  unitTest = do
--      bid <- `forAllQ` $ `chooseQ` (1, 100)
--      `action` $ Bid w1 bid
--      `action` $ Bid w2 (bid + 50)
--      `action` $ Wait endSlot
--      `action` $ Collect
-- @
--
-- More interesting scenarios can be constructed by mixing random and fixed sequences. The following
-- checks that you can always finish an auction after which point there are no funds locked by the
-- contract:
--
-- @
-- finishAuction :: `DL` AuctionState ()
-- finishAuction = do
--   `anyActions_`
--   `action` $ Wait endSlot
--   `action` $ Collect
--   `assertModel` "Funds are locked!" (`Ledger.Value.isZero` . `lockedValue`)
-- @
--
-- `DL` scenarios are turned into QuickCheck properties using `forAllDL`.

-- $dynamicLogic_errors
--
-- In addition to failing the check that the emulator run matches the model, there are a few other
-- ways that test scenarios can fail:
--
-- * an explicit `action` does not satisfy its `precondition`
-- * a failed `DL.assert` or `assertModel`, or a monad `fail`
-- * an `Control.Applicative.empty` set of `Control.Applicative.Alternative`s
-- * the scenario fails to terminate (see `stopping`)
--
-- All of these occur at test case generation time, and thus do not directly say anything about the
-- contract implementation. However, together with the check that the model agrees with the emulator
-- they indirectly imply properties of the implementation. An advantage of this is that `DL` test
-- scenarios can be checked without running the contract through the emulator, which is much much
-- faster. For instance,
--
-- @
-- prop_FinishModel = `forAllDL` finishAuction $ const True
-- @
--
-- would check that the model does not think there will be any locked funds after the auction is
-- finished. Once this property passes, one can run the slower property that also checks that the
-- emulator agrees.

-- | The monad for writing test scenarios. It supports non-deterministic choice through
--   `Control.Applicative.Alternative`, failure with `MonadFail`, and access to the model state
--   through `GetModelState`. It is lazy, so scenarios can be potentially infinite, although the
--   probability of termination needs to be high enough that concrete test cases are always finite.
--   See `stopping` for more information on termination.
type DL state = DL.DL (ModelState state)

-- | Generate a specific action. Fails if the action's `precondition` is not satisfied.
action :: ContractModel state => Action state -> DL state ()
action cmd = do
  s <- getModelState
  DL.action (contractAction s cmd)

-- | Generate a specific action. Fails if the action's `precondition` is not satisfied.
waitUntilDL :: ContractModel state => Slot -> DL state ()
waitUntilDL = DL.action . WaitUntil

-- | Generate a random action using `arbitraryAction`. The generated action is guaranteed to satisfy
--   its `precondition`. Fails with `Stuck` if no action satisfying the precondition can be found
--   after 100 attempts.
anyAction :: DL state ()
anyAction = DL.anyAction

-- | Generate a sequence of random actions using `arbitraryAction`. All actions satisfy their
--   `precondition`s. The argument is the expected number of actions in the sequence chosen from a
--   geometric distribution, unless in the `stopping` stage, in which case as few actions as
--   possible are generated.
anyActions :: Int -> DL state ()
anyActions = DL.anyActions

-- | Generate a sequence of random actions using `arbitraryAction`. All actions satisfy their
--   `precondition`s. Actions may be generated until the `stopping` stage is reached; the expected length is size/2.
anyActions_ :: DL state ()
anyActions_ = DL.anyActions_

-- | Test case generation from `DL` scenarios have a target length of the action sequence to be
--   generated that is based on the QuickCheck size parameter (see `sized`). However, given that
--   scenarios can contain explicit `action`s it might not be possible to stop the scenario once the
--   target length has been reached.
--
--   Instead, once the target number of actions have been reached, generation goes into the
--   /stopping/ phase. In this phase branches starting with `stopping` are preferred, if possible.
--   Conversely, before the stopping phase, branches starting with `stopping`
--   are avoided unless there are no other possible choices.
--
--   For example, here is the definition of `anyActions`:
--
-- @
-- `anyActions` n = `stopping` `Control.Applicative.<|>` pure ()
--                        `Control.Applicative.<|>` (`weight` (fromIntegral n) >> `anyAction` >> `anyActions` n)
-- @
--
--   The effect of this definition is that the second or third branch will be taken until the desired number
--   of actions have been generated, at which point the `stopping` branch will be taken and
--   generation stops (or continues with whatever comes after the `anyActions` call).
--
--   Now, it might not be possible, or too hard, to find a way to terminate a scenario. For
--   instance, this scenario has no finite test cases:
--
-- @
-- looping = `anyAction` >> looping
-- @
--
--   To prevent test case generation from looping, if a scenario has not terminated after generating
--   @2 * n + 20@ actions, where @n@ is when the stopping phase kicks in, generation fails with a
--   `Looping` error.
stopping :: DL state ()
stopping = DL.stopping

-- | By default, `Control.Applicative.Alternative` choice (`Control.Applicative.<|>`) picks among
--   the next actions with equal probability. So, for instance, this code chooses between the actions
--   @a@, @b@ and @c@, with a probability @1/3@ of choosing each:
--
-- @
-- unbiasedChoice a b c = `action` a `Control.Applicative.<|>` `action` b `Control.Applicative.<|>` `action` c
-- @
--
--   To change this you can use `weight`, which multiplies the
--   relative probability of picking a branch by the given number.
--
--   For instance, the following scenario picks the action @a@ with probability @2/3@ and the action
--   @b@ with probability @1/3@:
--
-- @
-- biasedChoice a b = `weight` 2 (`action` a) `Control.Applicative.<|>` `weight` (`action` b)
-- @
--
--   Calls to `weight` need to appear at the top-level after a choice, preceding any actions
--   (`action`/`anyAction`) or random generation (`forAllQ`), or they will have no effect.
weight :: Double -> DL state ()
weight = DL.weight

-- | Sometimes test case generation should depend on QuickCheck's size
--   parameter. This can be accessed using @getSize@. For example, @anyActions_@ is defined by
--
-- @
-- anyActions_ = do n <- getSize
--                  anyActions (n `div` 2 + 1)
-- @
--
-- so that we generate a random number of actions, but on average half the size (which is about the same as
-- the average random positive integer, or length of a list).

getSize :: DL state Int
getSize = DL.getSize

-- | The `monitor` function allows you to collect statistics of your testing using QuickCheck
--   functions like `Test.QuickCheck.label`, `Test.QuickCheck.collect`, `Test.QuickCheck.classify`,
--   and `Test.QuickCheck.tabulate`. See also the `monitoring` method of `ContractModel` which is
--   called for all actions in a test case (regardless of whether they are generated by an explicit
--   `action` or an `anyAction`).
monitor :: (Property -> Property) -> DL state ()
monitor = DL.monitorDL

-- | Fail unless the given predicate holds of the model state.
--
--   Equivalent to
--
-- @
-- assertModel msg p = do
--   s <- `getModelState`
--   `DL.assert` msg (p s)
-- @
assertModel :: String -> (ModelState state -> Bool) -> DL state ()
assertModel = DL.assertModel

-- | Turn a `DL` scenario into a QuickCheck property. Generates a random `Actions` matching the
--   scenario and feeds it to the given property. The property can be a full property running the
--   emulator and checking the results, defined using `propRunActions_`, `propRunActions`, or
--   `propRunActionsWithOptions`. Assuming a model for an auction contract and `DL` scenario that
--   checks that you can always complete the auction, you can write:
--
-- @
-- finishAuction :: `DL` AuctionState ()
-- prop_Auction  = `propRunActions_` handles
--   where handles = ...
-- prop_Finish = `forAllDL` finishAuction prop_Auction
-- @
--
--   However, there is also value in a property that does not run the emulator at all:
--
-- @
-- prop_FinishModel = `forAllDL` finishAuction $ const True
-- @
--
--   This will check all the assertions and other failure conditions of the `DL` scenario very
--   quickly. Once this property passes a large number of tests, you can run the full property
--   checking that the model agrees with reality.
forAllDL :: (ContractModel state, Testable p) => DL state () -> (Actions state -> p) -> Property
forAllDL dl prop = DL.forAllMappedDL toDLTest fromDLTest fromStateModelActions dl prop

forAllDL_ :: (ContractModel state, Testable p) => DL state () -> (Actions state -> p) -> Property
forAllDL_ dl prop = DL.forAllMappedDL_ toDLTest fromDLTest fromStateModelActions dl prop

forAllUniqueDL :: (ContractModel state, Testable p) => Int -> ModelState state -> DL state () -> (Actions state -> p) -> Property
forAllUniqueDL nextVar state dl prop = DL.forAllUniqueDL nextVar state dl (prop . fromStateModelActions)

instance ContractModel s => DL.DynLogicModel (ModelState s) where
    restricted (ContractAction _ act) = restricted act
    restricted WaitUntil{}            = False
    restricted Unilateral{}           = True

instance GetModelState (DL state) where
    type StateType (DL state) = state
    getModelState = DL.getModelStateDL

-- $quantify
--
-- `DL` scenarios support random generation using `DL.forAllQ`. This does not take a normal
-- QuickCheck `Gen` generator, but a `DL.Quantification`, which aside from a generator also keeps
-- track of which values can be generated. This means test cases coming from scenarios containing
-- `DL.forAll` can be prevented from shrinking to something that could not have been generated in
-- the first place.


-- $runningProperties
--
-- Once you have a `ContractModel` and some `DL` scenarios you need to turn these into QuickCheck
-- properties that can be run by `quickCheck`. The functions `propRunActions_`, `propRunActions`, and
-- `propRunActionsWithOptions` take a sequence of actions (a `Actions`), runs it through the
-- blockchain emulator ("Plutus.Trace.Emulator") and checks that the model and the emulator agree
-- on who owns what tokens at the end.
--
-- To generate a `Actions` you can use the `Arbitrary` instance, which generates a random sequence of
-- actions using `arbitraryAction`, or you can use `forAllDL` to generate a `Actions` from a `DL`
-- scenario.

-- | Options for controlling coverage checking requirements
--
-- * `checkCoverage` tells you whether or not to run the coverage checks at all.
-- * `endpointCoverageEq instance endpointName` tells us what percentage of tests are required to include
-- a call to the endpoint `endpointName` in the contract at `instance`.
-- * `coverIndex` is the coverage index obtained from the `CompiledCodeIn` of the validator.
data CoverageOptions = CoverageOptions { _checkCoverage       :: Bool
                                       , _endpointCoverageReq :: ContractInstanceTag -> String -> Double
                                       , _coverageIndex       :: CoverageIndex
                                       , _coverageIORef       :: Maybe (IORef CoverageData)
                                       }

makeLenses ''CoverageOptions

-- | Default coverage checking options are:
-- * not to check coverage
-- * set the requriements for every endpoint to 20% and
-- * not to cover any source locations in the validator scripts.
defaultCoverageOptions :: CoverageOptions
defaultCoverageOptions = CoverageOptions { _checkCoverage = False
                                         , _endpointCoverageReq = \ _ _ -> 0
                                         , _coverageIndex = mempty
                                         , _coverageIORef = Nothing }

-- | Run QuickCheck on a property that tracks coverage and print its coverage report.
quickCheckWithCoverage :: QC.Testable prop => QC.Args -> CoverageOptions -> (CoverageOptions -> prop) -> IO CoverageReport
quickCheckWithCoverage qcargs opts prop = fst <$> quickCheckWithCoverageAndResult qcargs opts prop

quickCheckWithCoverageAndResult :: QC.Testable prop => QC.Args -> CoverageOptions -> (CoverageOptions -> prop) -> IO (CoverageReport, Result)
quickCheckWithCoverageAndResult qcargs copts prop = do
  copts <- case copts ^. coverageIORef of
    Nothing -> do
      ref <- newIORef mempty
      return $ copts { _coverageIORef = Just ref }
    _ -> return copts
  res <- QC.quickCheckWithResult qcargs $ prop $ copts { _checkCoverage = True }
  case copts ^. coverageIORef of
    Nothing -> fail "Unreachable case in quickCheckWithCoverage"
    Just ref -> do
      covdata <- readIORef ref
      let report = CoverageReport (copts ^. coverageIndex) covdata
      when (chatty qcargs) $ putStrLn . show $ pretty report
      return (report, res)

finalChecks :: ContractModel state
            => CheckOptions
            -> CoverageOptions
            -> ([SomeContractInstanceKey state] -> Env {- Outer env -} -> TracePredicate)
            -> PropertyM (ContractMonad state) Env
            -> PropertyM (ContractMonad state) ()
finalChecks opts copts predicate prop = do
    outerEnv <- prop
    ContractMonadState tr keys' _ <- QC.run State.get
    let innerAction :: EmulatorTrace AssetMap
        innerAction = State.execStateT (runEmulatorAction tr IMNil) Map.empty
        action = do
          -- see note [The Env contract]
          env <- innerAction
          hdl <- activateContract w1 (getEnvContract @()) envContractInstanceTag
          void $ callEndpoint @"register-token-env" hdl env
        stream :: forall effs. S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) (Maybe EmulatorErr)
        stream = fst <$> runEmulatorStream (opts ^. emulatorConfig) action
        (errorResult, events) = S.streamFold (,[]) run (\ (msg S.:> es) -> (fst es, (msg ^. logMessageContent) : snd es)) stream
    case errorResult of
      Just err -> do
        QC.monitor $ counterexample (show err)
        QC.assert False
      _ -> return ()
    let cover report | copts ^. checkCoverage
                     , Just ref <- copts ^. coverageIORef =
                       report `deepseq`
                        (QC.monitor $ \ p -> ioProperty $ do
                           modifyIORef ref (report<>)
                           return p)
                     | otherwise = pure ()
    addEndpointCoverage copts keys' events $ checkPredicateInnerStream opts (noMainThreadCrashes .&&. predicate keys' outerEnv) (void stream) debugOutput assertResult cover
    where
        debugOutput :: Monad m => String -> PropertyM m ()
        debugOutput = QC.monitor . whenFail . putStrLn

        assertResult :: Monad m => Bool -> PropertyM m ()
        assertResult = QC.assert

        -- don't accept traces where the main thread crashes
        noMainThreadCrashes :: TracePredicate
        noMainThreadCrashes = assertUserLog $ \ log -> null [ () | UserThreadErr _ <- view eteEvent <$> log ]

-- | Check endpoint coverage
addEndpointCoverage :: ContractModel state
                    => CoverageOptions
                    -> [SomeContractInstanceKey state]
                    -> [EmulatorEvent]
                    -> PropertyM (ContractMonad state) a
                    -> PropertyM (ContractMonad state) a
addEndpointCoverage copts keys es pm
  | copts ^. checkCoverage = do
    x <- pm
    let -- Endpoint covereage
        epsToCover = [(instanceTag k, contractInstanceEndpoints k) | Key k <- keys]
        epsCovered = getInvokedEndpoints es
        endpointCovers = [ QC.cover (view endpointCoverageReq copts t e)
                                    (e `elem` fold (Map.lookup t epsCovered))
                                    (Text.unpack (unContractInstanceTag t) ++ " at endpoint " ++ e)
                         | (t, eps) <- epsToCover
                         , e <- eps ]
    endpointCovers `deepseq`
      (QC.monitor . foldr (.) id $ endpointCovers)
    return x
  | otherwise = pm

contractInstanceEndpoints :: forall state w s e p. SchemaConstraints w s e => ContractInstanceKey state w s e p -> [String]
contractInstanceEndpoints _ = labels' @(Input s)

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

-- | Default check options that include a large amount of Ada in the initial distributions to avoid having
-- to write `ContractModel`s that keep track of balances.
defaultCheckOptionsContractModel :: CheckOptions
defaultCheckOptionsContractModel =
  defaultCheckOptions & emulatorConfig . initialChainState .~ (Left . Map.fromList $ zip knownWallets (repeat (Ada.lovelaceValueOf 100_000_000_000_000_000)))

-- | Run a `Actions` in the emulator and check that the model and the emulator agree on the final
--   wallet balance changes, and that the given `TracePredicate` holds at the end. Equivalent to:
--
-- @
-- propRunActions = `propRunActionsWithOptions` `defaultCheckOptionsContractModel` `defaultCoverageOptions`
-- @
propRunActions ::
    ContractModel state
    => (ModelState state -> TracePredicate) -- ^ Predicate to check at the end
    -> Actions state                         -- ^ The actions to run
    -> Property
propRunActions = propRunActionsWithOptions defaultCheckOptionsContractModel defaultCoverageOptions

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
    => CheckOptions                          -- ^ Emulator options
    -> CoverageOptions                       -- ^ Coverage options
    -> (ModelState state -> TracePredicate)  -- ^ Predicate to check at the end
    -> Actions state                         -- ^ The actions to run
    -> Property
propRunActionsWithOptions opts copts predicate actions' =
  propRunActionsWithOptions' opts copts predicate (toStateModelActions actions')

initiateWallets :: ContractModel state => ContractMonad state ()
initiateWallets = do
  addInstances initialInstances
  setHandles $ lift (activateWallets (\ _ -> error "activateWallets: no sym tokens should have been created yet") initialInstances)
  return ()

finalPredicate :: ContractModel state
               => ModelState state
               -> (ModelState state -> TracePredicate)
               -> [SomeContractInstanceKey state]
               -> Env
               -> TracePredicate
finalPredicate finalState predicate keys' outerEnv =
        predicate finalState
        .&&. checkBalances finalState outerEnv
        .&&. checkNoCrashes keys'
        .&&. checkNoOverlappingTokens
        .&&. checkSlot finalState

propRunActionsWithOptions' :: forall state.
    ContractModel state
    => CheckOptions                          -- ^ Emulator options
    -> CoverageOptions                       -- ^ Coverage options
    -> (ModelState state -> TracePredicate)  -- ^ Predicate to check at the end
    -> StateModel.Actions (ModelState state) -- ^ The actions to run
    -> Property
propRunActionsWithOptions' opts copts predicate actions =
    asserts finalState QC..&&.
    (monadic (flip State.evalState mempty) $ finalChecks opts copts (finalPredicate finalState predicate) $ do
            QC.run initiateWallets
            snd <$> runActionsInState StateModel.initialState actions)
    where
        finalState = StateModel.stateAfter actions

asserts :: ModelState state -> Property
asserts finalState = foldr (QC..&&.) (property True) [ counterexample ("assertSpec failed: " ++ s) b
                                                     | (s, b) <- finalState ^. assertions ]

stateAfter :: ContractModel state => Actions state -> ModelState state
stateAfter actions = StateModel.stateAfter $ toStateModelActions actions

{- Note [The Env contract]

 In order to get the environment that maps symbolic variables to actual
 tokens out of the emulator we need to use a contract and the `instanceOutcome`
 fold. All this contract does is it materialises the `AssetMap` that we carry
 around inside the emulator.

 At the end of an emulator execution in which we want to check a property
 of the symbolic tokens we need to add a call to to the `register-token-env`
 endpoint and make sure that the `getEnvContract` is running.
-}


type EnvContractSchema = Endpoint "register-token-env" AssetMap

envContractInstanceTag :: ContractInstanceTag
envContractInstanceTag = "supercalifragilisticexpialidocious"

getEnvContract :: Contract w EnvContractSchema ContractError AssetMap
getEnvContract = toContract $ endpoint @"register-token-env" pure

checkBalances :: ModelState state
              -> Env -- ^ Outer env
              -> TracePredicate
checkBalances s envOuter = Map.foldrWithKey (\ w sval p -> walletFundsChange w sval .&&. p) (pure True) (s ^. balanceChanges)
  where
    walletFundsChange :: Wallet -> SymValue -> TracePredicate
    walletFundsChange w sval = TracePredicate $
      -- see Note [The Env contract]
      flip postMapM ((,) <$> Folds.instanceOutcome @() (toContract getEnvContract) envContractInstanceTag
                         <*> L.generalize ((,,) <$> Folds.walletFunds w <*> Folds.walletFees w <*> Folds.walletsAdjustedTxEvents)) $ \(outcome, (finalValue', fees, allWalletsTxOutCosts)) -> do
        dist <- Freer.ask @InitialDistribution
        case outcome of
          Done envInner -> do
            let lookupMaybe (SymToken outerVar idx) = do
                  innerVar <- lookUpVarMaybe envOuter outerVar
                  tokenMap <- Map.lookup innerVar envInner
                  Map.lookup idx tokenMap
                lookup st = case lookupMaybe st of
                  Nothing  -> error $ "Trying to look up unknown symbolic token: " ++ show st ++ ",\nare you using a custom implementation of getAllSymtokens? If not, please report this as a bug."
                  Just tok -> tok
                invert m = Map.fromList [(v, k) | (k, v) <- Map.toList m]
                invLookup ac = do
                  (innerVar, idx) <- listToMaybe [ (innerVar, idx) | (innerVar, tm) <- Map.toList envInner
                                                                   , idx <- maybeToList $ Map.lookup ac (invert tm) ]
                  outerVar <- invertLookupVarMaybe envOuter innerVar
                  return (SymToken outerVar idx)
                initialValue = fold (dist ^. at w)
                dlt' = toValue lookup sval
                finalValue = finalValue' P.+ fees
                dlt = calculateDelta dlt' (Ada.fromValue initialValue) (Ada.fromValue finalValue) allWalletsTxOutCosts
                result = initialValue P.+ dlt == finalValue
            unless result $ do
                tell @(Doc Void) $ vsep $
                    [ "Expected funds of" <+> pretty w <+> "to change by"
                    , " " <+> viaShow sval] ++
                    if initialValue == finalValue
                    then ["but they did not change"]
                    else ["but they changed by", " " <+> viaShow (toSymVal invLookup (finalValue P.- initialValue)),
                          "a discrepancy of",    " " <+> viaShow (toSymVal invLookup (finalValue P.- initialValue P.- dlt))]
            pure result
          _ -> error "I am the pope"

checkSlot :: ModelState state
          -> TracePredicate
checkSlot st =
  let lastSlot@(Slot s) = 1 + st ^. currentSlot in
  assertChainEvents' (\evs -> "Final emulator slot " ++ show (maximum [ s | SlotAdd (Slot s) <- evs ]) ++ " doesn't match model slot " ++ show s)
                                  (\evs -> lastSlot == maximum [ s | SlotAdd s <- evs ])

-- See the uniqueness requirement in Note [Symbolic Tokens and Symbolic Values]
checkNoOverlappingTokens :: TracePredicate
checkNoOverlappingTokens = TracePredicate $ flip postMapM (Folds.instanceOutcome @() (toContract getEnvContract) envContractInstanceTag) $ \ outcome ->
  case outcome of
    Done envInner -> do
     let tokens = concatMap Map.elems $ Map.elems envInner
         result = Set.size (Set.fromList tokens) == length tokens
     unless result $
       tell @(Doc Void) $ "Tokens:" <+> pretty tokens <+> "contain overlapping tokens at end of execution."
     pure result
    _ -> error "I am the pope"

checkNoCrashes :: forall state. ContractModel state => [SomeContractInstanceKey state] -> TracePredicate
checkNoCrashes = foldr (\ (Key k) -> (assertInstanceLog (instanceTag k) notError .&&.))
                       (pure True)
    where
        notError log = null [ () | StoppedWithError _ <- view (eteEvent . cilMessage) <$> log ]

-- | Sanity check a `ContractModel`. Ensures that wallet balances are not always unchanged.
propSanityCheckModel :: forall state. ContractModel state => Property
propSanityCheckModel =
  QC.expectFailure (noBalanceChanges . stateAfter @state)
  where
    noBalanceChanges s = all symIsZero (s ^. balanceChanges)

-- | Sanity check a `ContractModel`. Ensures that all assertions in
-- the property generation succeed.
propSanityCheckAssertions :: forall state. ContractModel state => Actions state -> Property
propSanityCheckAssertions as = monadic (flip State.evalState mempty) $ do
  -- We do this to gather all the statistics we need
  _ <- runActionsInState StateModel.initialState (toStateModelActions as)
  return (asserts $ stateAfter as)

-- | Sanity check a `ContractModel`. Ensures that `nextReactiveState` is idempotent.
propSanityCheckReactive :: forall state. (ContractModel state, Eq state) => Actions state -> Positive Integer -> Positive Integer -> Property
propSanityCheckReactive as (Positive sl) (Positive sl') =
    let s0 = stateAfter as
        sl1 = s0 ^. currentSlot + Slot sl
        sl2 = sl1 + Slot sl'
        s1 = runSpec (nextReactiveState sl1 >> nextReactiveState sl2) (error "unreachable") s0
        s2 = runSpec (nextReactiveState sl2) (error "unreachable") s0
    in counterexample "Balance changes not idempotent" (s1 ^. balanceChanges === s2 ^. balanceChanges)
    QC..&&. counterexample "Contract state changes not idempotent" (s1 ^. contractState === s2 ^. contractState)

-- $noLockedFunds
-- Showing that funds can not be locked in the contract forever.

-- | A "proof" that you can always recover the funds locked by a contract. The first component is
--   a strategy that from any state of the contract can get all the funds out. The second component
--   is a strategy for each wallet that from the same state, shows how that wallet can recover the
--   same (or bigger) amount as using the first strategy, without relying on any actions being taken
--   by the other wallets.
--
--   For instance, in a two player game where each player bets some amount of funds and the winner
--   gets the pot, there needs to be a mechanism for the players to recover their bid if the other
--   player simply walks away (perhaps after realising the game is lost). If not, it won't be
--   possible to construct a `NoLockedFundsProof` that works in a state where both players need to
--   move before any funds can be collected.
data NoLockedFundsProof model = NoLockedFundsProof
  { nlfpMainStrategy   :: DL model ()
    -- ^ Strategy to recover all funds from the contract in any reachable state.
  , nlfpWalletStrategy :: Wallet -> DL model ()
    -- ^ A strategy for each wallet to recover as much (or more) funds as the main strategy would
    --   give them in a given state, without the assistance of any other wallet.
  , nlfpOverhead       :: ModelState model -> SymValue
    -- ^ An initial amount of overhead value that may be lost - e.g. setup fees for scripts that
    -- can't be recovered.
  , nlfpErrorMargin    :: ModelState model -> SymValue
    -- ^ The total amount of margin for error in the value collected by the WalletStrategy compared
    -- to the MainStrategy. This is useful if your contract contains rounding code that makes the order
    -- of operations have a small but predictable effect on the value collected by different wallets.
  }
data NoLockedFundsProofLight model = NoLockedFundsProofLight
  { nlfplMainStrategy :: DL model () }

-- | The default skeleton of a NoLockedFundsProof - doesn't permit any overhead or error margin.
defaultNLFP :: NoLockedFundsProof model
defaultNLFP = NoLockedFundsProof { nlfpMainStrategy = return ()
                                 , nlfpWalletStrategy = const (return ())
                                 , nlfpOverhead = const mempty
                                 , nlfpErrorMargin = const mempty }

-- | Check a `NoLockedFundsProof`. Each test will generate an arbitrary sequence of actions
--   (`anyActions_`) and ask the `nlfpMainStrategy` to recover all funds locked by the contract
--   after performing those actions. This results in some distribution of the contract funds to the
--   wallets, and the test then asks each `nlfpWalletStrategy` to show how to recover their
--   allotment of funds without any assistance from the other wallets (assuming the main strategy
--   did not execute). When executing wallet strategies, the off-chain instances for other wallets
--   are killed and their private keys are deleted from the emulator state.

checkNoLockedFundsProof
  :: (ContractModel model)
  => NoLockedFundsProof model
  -> Property
checkNoLockedFundsProof = checkNoLockedFundsProofWithOptions defaultCheckOptionsContractModel

checkNoLockedFundsProofFast
  :: (ContractModel model)
  => NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofFast = checkNoLockedFundsProofFastWithOptions defaultCheckOptionsContractModel

checkNoLockedFundsProofWithOptions
  :: (ContractModel model)
  => CheckOptions
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofWithOptions options =
  checkNoLockedFundsProof' prop
  where
    prop = propRunActionsWithOptions' options defaultCoverageOptions (\ _ -> TracePredicate $ pure True)

checkNoLockedFundsProofFastWithOptions
  :: (ContractModel model)
  => CheckOptions
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofFastWithOptions _ = checkNoLockedFundsProof' (const $ property True)

checkNoLockedFundsProof'
  :: (ContractModel model)
  => (StateModel.Actions (ModelState model) -> Property)
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProof' run NoLockedFundsProof{nlfpMainStrategy   = mainStrat,
                                                nlfpWalletStrategy = walletStrat,
                                                nlfpOverhead       = overhead,
                                                nlfpErrorMargin    = wiggle } =
    forAllDL anyActions_ $ \ (Actions as) ->
    forAllUniqueDL (nextVarIdx as) (stateAfter $ Actions as) mainStrat $ \ (Actions as') ->
          let s0 = (stateAfter $ Actions as)
              s = stateAfter $ Actions (as ++ as') in
            foldl (QC..&&.) (counterexample "Main run prop" (run (toStateModelActions $ Actions $ as ++ as')) QC..&&. (counterexample "Main strategy" . counterexample (show . Actions $ as ++ as') $ prop s0 s))
                            [ walletProp s0 as w bal | (w, bal) <- Map.toList (s ^. balanceChanges)
                                                     , not $ bal `symLeq` (s0 ^. balanceChange w) ]
                                                     -- if the main strategy leaves w with <= the starting value, then doing nothing is a good wallet strategy.
    where
        nextVarIdx as = 1 + maximum ([0] ++ [ i | i <- varNumOf <$> as ])
        prop s0 s =
          let lockedVal = lockedValue s
          in (counterexample ("Locked funds should be at most " ++ show (overhead s0) ++ ", but they are\n  " ++ show lockedVal)
            $ symLeq lockedVal (overhead s0))

        walletProp s0 as w bal =
          DL.forAllUniqueDL (nextVarIdx as) s0 (DL.action (Unilateral w) >> walletStrat w) $ \ acts ->
          let Actions as' = fromStateModelActions acts
              wig = wiggle s0
              err  = "Unilateral strategy for " ++ show w ++ " should have gotten it at least\n" ++
                     "  " ++ show bal ++ "\n" ++
                     concat [ "with wiggle room\n" ++ "  " ++ show wig ++ "\n"
                            | wig /= mempty ] ++
                     "but it got\n" ++
                     "  " ++ show bal'
              bal' = stateAfter (Actions $ as ++ as') ^. balanceChange w
              smacts = toStateModelActions (Actions as) <> acts
              err' = "The ContractModel's Unilateral behaviour for " ++ show w ++ " does not match the actual behaviour for actions:\n"
                   ++ show smacts
          in counterexample err (symLeq bal (bal' <> wig))
          QC..&&. counterexample err' (run smacts)

checkNoLockedFundsProofLight
  :: ContractModel model
  => NoLockedFundsProofLight model
  -> Property
checkNoLockedFundsProofLight NoLockedFundsProofLight{nlfplMainStrategy = mainStrat} =
  forAllDL anyActions_ $ \ (Actions as) ->
    forAllUniqueDL (nextVarIdx as) (stateAfter $ Actions as) mainStrat $ \ (Actions as') ->
      counterexample "Main run prop" (run (toStateModelActions $ Actions $ as ++ as'))
  where
    nextVarIdx as = 1 + maximum ([0] ++ [ i | Var i <- varOf <$> as ])
    run = propRunActionsWithOptions' defaultCheckOptionsContractModel
                                     defaultCoverageOptions (\ _ -> TracePredicate $ pure True)

-- | A whitelist entry tells you what final log entry prefixes
-- are acceptable for a given error
data Whitelist = Whitelist { errorPrefixes :: Set Text.Text }

instance Semigroup Whitelist where
  Whitelist wl <> Whitelist wl' = Whitelist $ wl <> wl'

instance Monoid Whitelist where
  mempty = defaultWhitelist

-- | Check that the last entry in a log is accepted by a whitelist entry
isAcceptedBy :: Maybe Text.Text -> Whitelist -> Bool
isAcceptedBy Nothing _           = False
isAcceptedBy (Just lastEntry) wl = any (`Text.isPrefixOf` lastEntry) (Set.toList $ errorPrefixes wl)

{- Note [Maintaining `whitelistOk` and `checkErrorWhitelist`]
   The intended use case of `checkErrorWhitelist` is to be able to assert that failures of
   validation only happen for reasons that the programmer intended. However, to avoid
   degenerate whitelists that accept obvious mistakes like validation failing because a
   partial function in the prelude failed we need to make sure that any whitelist used passes
   the `whitelistOk` check. This means in turn that we need to maintain `whitelistOk` when we
   introduce new failure modes or change existing failure modes in the prelude or elsewhere
   in the plutus system.
-}

-- | Check that a whitelist does not accept any partial functions
whitelistOk :: Whitelist -> Bool
whitelistOk wl = noPreludePartials
  where
    noPreludePartials =
      -- We specifically ignore `checkHasFailed` here because it is the failure you get when a
      -- validator that returns a boolean fails correctly.
      all (\ec -> Prelude.not $ (Just $ Builtins.fromBuiltin ec) `isAcceptedBy` wl) (Map.keys allErrorCodes \\ [checkHasFailedError])

mkWhitelist :: [Text.Text] -> Whitelist
mkWhitelist txs = defaultWhitelist <> Whitelist (Set.fromList txs)

defaultWhitelist :: Whitelist
defaultWhitelist = Whitelist . Set.singleton $ Builtins.fromBuiltin checkHasFailedError

-- | Check that running a contract model does not result in validation
-- failures that are not accepted by the whitelist.
checkErrorWhitelist :: ContractModel m
                    => Whitelist
                    -> Actions m
                    -> Property
checkErrorWhitelist = checkErrorWhitelistWithOptions defaultCheckOptionsContractModel defaultCoverageOptions

-- | Check that running a contract model does not result in validation
-- failures that are not accepted by the whitelist.
checkErrorWhitelistWithOptions :: forall m. ContractModel m
                               => CheckOptions
                               -> CoverageOptions
                               -> Whitelist
                               -> Actions m
                               -> Property
checkErrorWhitelistWithOptions opts copts whitelist acts = property $ go check acts
  where
    check :: TracePredicate
    check = checkOnchain .&&. (assertNoFailedTransactions .||. checkOffchain)

    checkOnchain :: TracePredicate
    checkOnchain = assertChainEvents checkEvents

    checkOffchain :: TracePredicate
    checkOffchain = assertFailedTransaction (\ _ _ -> all (either checkEvent (const True) . sveResult))

    checkEvent :: ScriptError -> Bool
    checkEvent (EvaluationError log "CekEvaluationFailure") = listToMaybe (reverse log) `isAcceptedBy` whitelist
    checkEvent (EvaluationError _ msg) | "BuiltinEvaluationFailure" `isPrefixOf` msg = False
    checkEvent _                                            = False

    checkEvents :: [ChainEvent] -> Bool
    checkEvents events = all checkEvent [ f | (TxnValidationFail _ _ _ (ScriptFailure f) _ _) <- events ]

    go :: TracePredicate -> Actions m -> Property
    go check actions = monadic (flip State.evalState mempty) $ finalChecks opts copts (\ _ _ -> check) $ do
                        QC.run initiateWallets
                        snd <$> runActionsInState StateModel.initialState (toStateModelActions actions)

