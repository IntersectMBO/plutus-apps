{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveAnyClass         #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MonoLocalBinds         #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeApplications       #-}
module Plutus.Contract.StateMachine(
    -- $statemachine
    StateMachineClient(..)
    , TxConstraints
    , SMContractError(..)
    , AsSMContractError(..)
    , SM.StateMachine(..)
    , SM.StateMachineInstance(..)
    , SM.State(..)
    , OnChainState(..)
    , WaitingResult(..)
    , InvalidTransition(..)
    , TransitionResult(..)
    , ThreadToken(..)
    -- * Constructing the machine instance
    , SM.mkValidator
    , SM.mkStateMachine
    -- * Constructing the state machine client
    , mkStateMachineClient
    , defaultChooser
    , getStates
    -- * Running the state machine
    , runGuardedStep
    , runStep
    , runInitialise
    , runGuardedStepWith
    , runStepWith
    , runInitialiseWith
    , getThreadToken
    , getOnChainState
    , getStateData
    , waitForUpdate
    , waitForUpdateUntilSlot
    , waitForUpdateUntilTime
    , waitForUpdateTimeout
    -- * Lower-level API
    , StateMachineTransition(..)
    , mkStep
    -- * Re-exports
    , Void
    ) where

import Control.Lens (_Right, makeClassyPrisms, review, (^?))
import Control.Monad (unless)
import Control.Monad.Error.Lens
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Ledger (POSIXTime, Slot, TxOutRef, Value)
import Ledger qualified
import Ledger.Constraints (ScriptLookups, TxConstraints, mintingPolicy, mustMintValueWithRedeemer, mustPayToTheScript,
                           mustSpendPubKeyOutput)
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Constraints.OffChain qualified as Constraints
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icRedeemer, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocValue), txOwnInputs,
                                         txOwnOutputs)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as Value
import Plutus.ChainIndex (ChainIndexTx (_citxInputs, _citxRedeemers))
import Plutus.Contract (AsContractError (_ConstraintResolutionContractError, _ContractError), Contract, ContractError,
                        Promise, adjustUnbalancedTx, awaitPromise, isSlot, isTime, logWarn, mapError, never,
                        ownFirstPaymentPubKeyHash, ownUtxos, promiseBind, select, submitTxConfirmed, utxoIsProduced,
                        utxoIsSpent, utxosAt, utxosTxOutTxFromTx)
import Plutus.Contract.Request (mkTxContract)
import Plutus.Contract.StateMachine.MintingPolarity (MintingPolarity (Burn, Mint))
import Plutus.Contract.StateMachine.OnChain (State (State, stateData, stateValue),
                                             StateMachine (StateMachine, smFinal, smThreadToken, smTransition),
                                             StateMachineInstance (StateMachineInstance, stateMachine, typedValidator))
import Plutus.Contract.StateMachine.OnChain qualified as SM
import Plutus.Contract.StateMachine.ThreadToken (ThreadToken (ThreadToken), curPolicy, ttOutRef)
import Plutus.Contract.Wallet (getUnspentOutput)
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import Plutus.Script.Utils.V1.Typed.Scripts qualified as Typed
import Plutus.V1.Ledger.Api qualified as Ledger
import PlutusTx qualified
import PlutusTx.Monoid (inv)

-- $statemachine
-- To write your contract as a state machine you need
-- * Two types @state@ and @input@ for the state and inputs of the machine
-- * A 'SM.StateMachineInstance state input' describing the transitions and
--   checks of the state machine (this is the on-chain code)
-- * A 'StateMachineClient state input' with the state machine instance and
--   an allocation function
--
-- In many cases it is enough to define the transition function
-- @t :: (state, Value) -> input -> Maybe (TxConstraints state)@ and use
-- 'mkStateMachine' and 'mkStateMachineClient' to get the client.
-- You can then use 'runInitialise' and 'runStep' to initialise and transition
-- the state machine. 'runStep' gets the current state from the utxo set and
-- makes the transition to the next state using the given input and taking care
-- of all payments.

-- | Typed representation of the on-chain state of a state machine instance
newtype OnChainState s i =
    OnChainState
        { ocsTxOutRef :: Typed.TypedScriptTxOutRef (SM.StateMachine s i) -- ^ Typed UTXO
        }

getStateData :: OnChainState s i -> s
getStateData = Typed.tyTxOutData . Typed.tyTxOutRefOut . ocsTxOutRef

getInput ::
    forall i.
    (PlutusTx.FromData i)
    => TxOutRef
    -> ChainIndexTx
    -> Maybe i
getInput outRef tx = do
    -- We retrieve the correspondent redeemer according to the index of txIn in the list
    let findRedeemer (ix, _) = Map.lookup (Tx.RedeemerPtr Tx.Spend ix) (_citxRedeemers tx)
    Ledger.Redeemer r <- listToMaybe $ mapMaybe findRedeemer $ filter (\(_, Tx.TxIn{Tx.txInRef}) -> outRef == txInRef) $ zip [0..] $ _citxInputs tx
    PlutusTx.fromBuiltinData r

getStates
    :: forall s i
    . (PlutusTx.FromData s, PlutusTx.ToData s)
    => SM.StateMachineInstance s i
    -> Map Tx.TxOutRef Tx.ChainIndexTxOut
    -> [OnChainState s i]
getStates (SM.StateMachineInstance _ si) refMap =
    flip mapMaybe (Map.toList refMap) $ \(txOutRef, ciTxOut) -> do
      let txOut = Tx.toTxOut ciTxOut
      datum <- ciTxOut ^? Tx.ciTxOutDatum . _Right
      ocsTxOutRef <- either (const Nothing) Just $ Typed.typeScriptTxOutRef si txOutRef txOut datum
      pure OnChainState{ocsTxOutRef}

-- | An invalid transition
data InvalidTransition s i =
    InvalidTransition
        { tfState :: Maybe (State s) -- ^ Current state. 'Nothing' indicates that there is no current state.
        , tfInput :: i -- ^ Transition that was attempted but failed
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

-- | Result of an attempted transition
data TransitionResult s i =
    TransitionFailure (InvalidTransition s i) -- ^ The transition is not allowed
    | TransitionSuccess s -- ^ The transition is allowed and results in a new state

data SMContractError =
    ChooserError Text
    | UnableToExtractTransition
    | SMCContractError ContractError
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''SMContractError

instance AsContractError SMContractError where
    _ContractError = _SMCContractError

-- | Client-side definition of a state machine.
data StateMachineClient s i = StateMachineClient
    { scInstance :: SM.StateMachineInstance s i
    -- ^ The instance of the state machine, defining the machine's transitions,
    --   its final states and its check function.
    , scChooser  :: [OnChainState s i] -> Either SMContractError (OnChainState s i)
    -- ^ A function that chooses the relevant on-chain state, given a list of
    --   all potential on-chain states found at the contract address.
    }

-- | A state chooser function that fails if confronted with anything other
--   than exactly one output
defaultChooser ::
    forall state input
    . [OnChainState state input]
    -> Either SMContractError (OnChainState state input)
defaultChooser [x] = Right x
defaultChooser xs  =
    let msg = "Found " <> show (length xs) <> " outputs, expected 1"
    in Left (ChooserError (Text.pack msg))

-- | A state chooser function that searches for an output with the thread token
threadTokenChooser ::
    forall state input
    . Value
    -> [OnChainState state input]
    -> Either SMContractError (OnChainState state input)
threadTokenChooser val states =
    let hasToken OnChainState{ocsTxOutRef} = val `Value.leq` (Tx.txOutValue $ Typed.tyTxOutTxOut $ Typed.tyTxOutRefOut ocsTxOutRef) in
    case filter hasToken states of
        [x] -> Right x
        xs ->
            let msg = unwords ["Found", show (length xs), "outputs with thread token", show val, "expected 1"]
            in Left (ChooserError (Text.pack msg))

-- | A state machine client with the 'defaultChooser' function
mkStateMachineClient ::
    forall state input
    . SM.StateMachineInstance state input
    -> StateMachineClient state input
mkStateMachineClient inst =
    let threadTokenVal = SM.threadTokenValueOrZero inst
        scChooser = if Value.isZero threadTokenVal then defaultChooser else threadTokenChooser threadTokenVal
    in StateMachineClient
        { scInstance = inst
        , scChooser
        }

{-| Get the current on-chain state of the state machine instance.
    Return Nothing if there is no state on chain.
    Throws an @SMContractError@ if the number of outputs at the machine address is greater than one.
-}
getOnChainState ::
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    )
    => StateMachineClient state i
    -> Contract w schema e (Maybe (OnChainState state i, Map TxOutRef Tx.ChainIndexTxOut))
getOnChainState StateMachineClient{scInstance, scChooser} = mapError (review _SMContractError) $ do
    utxoTx <- utxosAt (SM.machineAddress scInstance)
    let states = getStates scInstance utxoTx
    case states of
        [] -> pure Nothing
        _  -> case scChooser states of
                Left err    -> throwing _SMContractError err
                Right state -> pure $ Just (state, utxoTx)

-- | The outcome of 'waitForUpdateTimeout'
data WaitingResult t i s
    = Timeout t -- ^ The timeout happened before any change of the on-chain state was detected
    | ContractEnded i -- ^ The state machine instance ended
    | Transition i s -- ^ The state machine instance transitioned to a new state
    | InitialState s -- ^ The state machine instance was initialised
  deriving stock (Show,Generic,Functor)
  deriving anyclass (ToJSON, FromJSON)

-- | Wait for the on-chain state of the state machine instance to change until timeoutSlot,
--   and return the new state, or return 'ContractEnded' if the instance has been
--   terminated. If 'waitForUpdate' is called before the instance has even
--   started then it returns the first state of the instance as soon as it
--   has started.
waitForUpdateUntilSlot ::
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClient state i
    -> Slot
    -> Contract w schema e (WaitingResult Slot i state)
waitForUpdateUntilSlot client timeoutSlot = do
  result <- waitForUpdateTimeout client (isSlot timeoutSlot) >>= awaitPromise
  pure $ fmap getStateData result

-- | Same as 'waitForUpdateUntilSlot', but works with 'POSIXTime' instead.
waitForUpdateUntilTime ::
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClient state i
    -> POSIXTime
    -> Contract w schema e (WaitingResult POSIXTime i state)
waitForUpdateUntilTime client timeoutTime = do
  result <- waitForUpdateTimeout client (isTime timeoutTime) >>= awaitPromise
  pure $ fmap getStateData result

-- | Wait until the on-chain state of the state machine instance has changed,
--   and return the new state, or return 'Nothing' if the instance has been
--   terminated. If 'waitForUpdate' is called before the instance has even
--   started then it returns the first state of the instance as soon as it
--   has started.
waitForUpdate ::
    forall state i w schema e.
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClient state i
    -> Contract w schema e (Maybe (OnChainState state i))
waitForUpdate client = do
  result <- waitForUpdateTimeout client never >>= awaitPromise
  case result of
    Timeout t       -> absurd t
    ContractEnded{} -> pure Nothing
    InitialState r  -> pure (Just r)
    Transition _ r  -> pure (Just r)

-- | Construct a 'Promise' that waits for an update to the state machine's
--   on-chain state, or a user-defined timeout (whichever happens first).
waitForUpdateTimeout ::
    forall state i t w schema e.
    ( AsSMContractError e
    , AsContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.FromData i
    )
    => StateMachineClient state i -- ^ The state machine client
    -> Promise w schema e t -- ^ The timeout
    -> Contract w schema e (Promise w schema e (WaitingResult t i (OnChainState state i)))
waitForUpdateTimeout client@StateMachineClient{scInstance, scChooser} timeout = do
    currentState <- getOnChainState client
    let projectFst = (\(a, (b, _)) -> (a, b))
    let success = case currentState of
                    Nothing ->
                        -- There is no on-chain state, so we wait for an output to appear
                        -- at the address. Any output that appears needs to be checked
                        -- with scChooser'
                        let addr = Scripts.validatorAddress $ typedValidator scInstance in
                        promiseBind (utxoIsProduced addr) $ \txns -> do
                            outRefMaps <- traverse utxosTxOutTxFromTx txns
                            let produced = getStates @state @i scInstance (Map.fromList $ map projectFst $ concat outRefMaps)
                            case scChooser produced of
                                Left e             -> throwing _SMContractError e
                                Right onChainState -> pure $ InitialState onChainState
                    Just (OnChainState{ocsTxOutRef}, _) ->
                        promiseBind (utxoIsSpent (Typed.tyTxOutRefRef ocsTxOutRef)) $ \txn -> do
                            outRefMap <- Map.fromList . map projectFst <$> utxosTxOutTxFromTx txn
                            let newStates = getStates @state @i scInstance outRefMap
                                inp       = getInput (Typed.tyTxOutRefRef ocsTxOutRef) txn
                            case (newStates, inp) of
                                ([], Just i) -> pure (ContractEnded i)
                                (xs, Just i) -> case scChooser xs of
                                    Left e         -> throwing _SMContractError e
                                    Right newState -> pure (Transition i newState)
                                _ -> throwing_ _UnableToExtractTransition
    pure $ select success (Timeout <$> timeout)

-- | Tries to run one step of a state machine: If the /guard/ (the last argument) returns @'Nothing'@ when given the
-- unbalanced transaction to be submitted, the old state and the new step, the step is run and @'Right'@ the new state is returned.
-- If the guard returns @'Just' a@, @'Left' a@ is returned instead.
runGuardedStep ::
    forall w a e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => StateMachineClient state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract w schema e (Either a (TransitionResult state input))
runGuardedStep = runGuardedStepWith mempty mempty

-- | Run one step of a state machine, returning the new state.
runStep ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => StateMachineClient state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> Contract w schema e (TransitionResult state input)
runStep = runStepWith mempty mempty

-- | Create a thread token. The thread token contains a reference to an unspent output of the wallet,
-- so it needs to used with 'mkStateMachine' immediately, and the machine must be initialised,
-- to prevent the output from getting spent in the mean time.
getThreadToken :: AsSMContractError e => Contract w schema e ThreadToken
getThreadToken = mapError (review _SMContractError) $ do
    txOutRef <- getUnspentOutput
    pure $ ThreadToken txOutRef (scriptCurrencySymbol (curPolicy txOutRef))

-- | Initialise a state machine
runInitialise ::
    forall w e state schema input.
    ( PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    , AsSMContractError e
    )
    => StateMachineClient state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> Contract w schema e state
runInitialise = runInitialiseWith mempty mempty

-- | Constraints & lookups needed to transition a state machine instance
data StateMachineTransition state input =
    StateMachineTransition
        { smtConstraints :: TxConstraints input state
        , smtOldState    :: State state
        , smtNewState    :: State state
        , smtLookups     :: ScriptLookups (StateMachine state input)
        }

-- | Initialise a state machine and supply additional constraints and lookups for transaction.
runInitialiseWith ::
    forall w e state schema input.
    ( PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    , AsSMContractError e
    )
    => ScriptLookups (StateMachine state input)
    -- ^ Additional lookups
    -> TxConstraints input state
    -- ^ Additional constraints
    -> StateMachineClient state input
    -- ^ The state machine
    -> state
    -- ^ The initial state
    -> Value
    -- ^ The value locked by the contract at the beginning
    -> Contract w schema e state
runInitialiseWith customLookups customConstraints StateMachineClient{scInstance} initialState initialValue =
    mapError (review _SMContractError) $ do
      utxo <- ownUtxos
      let StateMachineInstance{stateMachine, typedValidator} = scInstance
          constraints = mustPayToTheScript initialState (initialValue <> SM.threadTokenValueOrZero scInstance)
              <> foldMap ttConstraints (smThreadToken stateMachine)
              <> customConstraints
          red = Ledger.Redeemer (PlutusTx.toBuiltinData (Scripts.validatorHash typedValidator, Mint))
          ttConstraints ThreadToken{ttOutRef} =
              mustMintValueWithRedeemer red (SM.threadTokenValueOrZero scInstance)
              <> mustSpendPubKeyOutput ttOutRef
          lookups = Constraints.typedValidatorLookups typedValidator
              <> foldMap (mintingPolicy . curPolicy . ttOutRef) (smThreadToken stateMachine)
              <> Constraints.unspentOutputs utxo
              <> customLookups
      utx <- mapError (review _ConstraintResolutionContractError) (mkTxContract lookups constraints)
      adjustedUtx <- adjustUnbalancedTx utx
      unless (utx == adjustedUtx) $
        logWarn @Text $ "Plutus.Contract.StateMachine.runInitialise: "
                      <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
      submitTxConfirmed adjustedUtx
      pure initialState

-- | Run one step of a state machine, returning the new state. We can supply additional constraints and lookups for transaction.
runStepWith ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => ScriptLookups (StateMachine state input)
    -- ^ Additional lookups
    -> TxConstraints input state
    -- ^ Additional constraints
    -> StateMachineClient state input
    -- ^ The state machine
    -> input
    -- ^ The input to apply to the state machine
    -> Contract w schema e (TransitionResult state input)
runStepWith lookups constraints smc input =
    runGuardedStepWith lookups constraints smc input (\_ _ _ -> Nothing) >>= pure . \case
        Left a  -> absurd a
        Right a -> a

-- | The same as 'runGuardedStep' but we can supply additional constraints and lookups for transaction.
runGuardedStepWith ::
    forall w a e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    , PlutusTx.ToData input
    )
    => ScriptLookups (StateMachine state input)    -- ^ Additional lookups
    -> TxConstraints input state                   -- ^ Additional constraints
    -> StateMachineClient state input              -- ^ The state machine
    -> input                                       -- ^ The input to apply to the state machine
    -> (UnbalancedTx -> state -> state -> Maybe a) -- ^ The guard to check before running the step
    -> Contract w schema e (Either a (TransitionResult state input))
runGuardedStepWith userLookups userConstraints smc input guard =
  mapError (review _SMContractError) $ mkStep smc input >>= \case
    Right StateMachineTransition{smtConstraints,smtOldState=State{stateData=os}, smtNewState=State{stateData=ns}, smtLookups} -> do
        pk <- ownFirstPaymentPubKeyHash
        let lookups = smtLookups { Constraints.slOwnPaymentPubKeyHash = Just pk }
        utx <- either (throwing _ConstraintResolutionContractError)
                      pure
                      (Constraints.mkTx (lookups <> userLookups) (smtConstraints <> userConstraints))
        adjustedUtx <- adjustUnbalancedTx utx
        unless (utx == adjustedUtx) $
          logWarn @Text $ "Plutus.Contract.StateMachine.runStep: "
                       <> "Found a transaction output value with less than the minimum amount of Ada. Adjusting ..."
        case guard adjustedUtx os ns of
            Nothing -> do
                submitTxConfirmed adjustedUtx
                pure $ Right $ TransitionSuccess ns
            Just a  -> pure $ Left a
    Left e -> pure $ Right $ TransitionFailure e

-- | Given a state machine client and an input to apply to
--   the client's state machine instance, compute the 'StateMachineTransition'
--   that can produce an actual transaction performing the transition
mkStep ::
    forall w e state schema input.
    ( AsSMContractError e
    , PlutusTx.FromData state
    , PlutusTx.ToData state
    )
    => StateMachineClient state input
    -> input
    -> Contract w schema e (Either (InvalidTransition state input) (StateMachineTransition state input))
mkStep client@StateMachineClient{scInstance} input = do
    let StateMachineInstance{stateMachine, typedValidator} = scInstance
        StateMachine{smTransition} = stateMachine
    maybeState <- getOnChainState client
    case maybeState of
        Nothing -> pure $ Left $ InvalidTransition Nothing input
        Just (onChainState, utxo) -> do
            let OnChainState{ocsTxOutRef} = onChainState
                oldState = State
                    { stateData = getStateData onChainState
                      -- Hide the thread token value from the client code
                    , stateValue = Ledger.txOutValue (Typed.tyTxOutTxOut $ Typed.tyTxOutRefOut ocsTxOutRef) <> inv (SM.threadTokenValueOrZero scInstance)
                    }
                inputConstraints = [ScriptInputConstraint{icRedeemer=input, icTxOutRef = Typed.tyTxOutRefRef ocsTxOutRef }]

            case smTransition oldState input of
                Just (newConstraints, newState)  ->
                    let isFinal = smFinal stateMachine (stateData newState)
                        lookups =
                            Constraints.typedValidatorLookups typedValidator
                            <> Constraints.unspentOutputs utxo
                            <> if isFinal then foldMap (mintingPolicy . curPolicy . ttOutRef) (smThreadToken stateMachine) else mempty
                        red = Ledger.Redeemer (PlutusTx.toBuiltinData (Scripts.validatorHash typedValidator, Burn))
                        unmint = if isFinal then mustMintValueWithRedeemer red (inv $ SM.threadTokenValueOrZero scInstance) else mempty
                        outputConstraints =
                            [ ScriptOutputConstraint
                                { ocDatum = stateData newState
                                  -- Add the thread token value back to the output
                                , ocValue = stateValue newState <> SM.threadTokenValueOrZero scInstance
                                }
                            | not isFinal ]
                    in pure
                        $ Right
                        $ StateMachineTransition
                            { smtConstraints =
                                (newConstraints <> unmint)
                                    { txOwnInputs = inputConstraints
                                    , txOwnOutputs = outputConstraints
                                    }
                            , smtOldState = oldState
                            , smtNewState = newState
                            , smtLookups = lookups
                            }
                Nothing -> pure $ Left $ InvalidTransition (Just oldState) input
