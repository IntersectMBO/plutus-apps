{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}
-- You need to use all of these to get coverage

-- | A guessing game that
--
--   * Uses a state machine to keep track of the current secret word
--   * Uses a token to keep track of who is allowed to make a guess
--

module Plutus.Contracts.GameStateMachine(
    contract
    , typedValidator
    , GameParam(..)
    , GuessToken
    , mkValidator
    , mintingPolicy
    , LockArgs(..)
    , GuessArgs(..)
    , GameStateMachineSchema
    , GameError
    , token
    , covIdx
    ) where

import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as C
import GHC.Generics (Generic)
import Ledger (POSIXTime, PaymentPubKeyHash, TokenName, Value)
import Ledger.Ada qualified as Ada
import Ledger.Constraints (TxConstraints)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value qualified as V
import Plutus.Contract (AsContractError (_ContractError), Contract, ContractError, Endpoint, Promise, endpoint,
                        selectList, type (.\/))
import Plutus.Contract.Secrets (SecretArgument, escape_sha2_256, extractSecret)
import Plutus.Contract.StateMachine (State (State, stateData, stateValue), Void)
import Plutus.Contract.StateMachine qualified as SM
import Plutus.V1.Ledger.Scripts (MintingPolicyHash)
import PlutusTx qualified
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude (Bool (False, True), BuiltinByteString, Eq, Maybe (Just, Nothing), sha2_256, toBuiltin,
                         traceIfFalse, ($), (&&), (-), (.), (<$>), (<>), (==), (>>))
import Schema (ToSchema)

import Prelude qualified as Haskell

-- | Datatype for creating a parameterized validator.
data GameParam = GameParam
    { gameParamPayeePkh  :: PaymentPubKeyHash
    -- ^ Payment public key hash of the wallet locking some funds
    , gameParamStartTime :: POSIXTime
    -- ^ Starting time of the game
    } deriving (Haskell.Show, Generic)
      deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''GameParam

newtype HashedString = HashedString BuiltinByteString
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString
    deriving newtype (Eq, PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''ClearString

-- | Arguments for the @"lock"@ endpoint
data LockArgs =
    LockArgs
        { lockArgsGameParam :: GameParam
        -- ^ The parameters for parameterizing the validator.
        , lockArgsSecret    :: SecretArgument Haskell.String
        -- ^ The secret
        , lockArgsValue     :: Value
        -- ^ Value that is locked by the contract initially
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs =
    GuessArgs
        { guessArgsGameParam     :: GameParam
        -- ^ The parameters for parameterizing the validator.
        , guessArgsOldSecret     :: Haskell.String
        -- ^ The guess
        , guessArgsNewSecret     :: SecretArgument Haskell.String
        -- ^ The new secret
        , guessArgsValueTakenOut :: Value
        -- ^ How much to extract from the contract
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | The schema of the contract. It consists of the two endpoints @"lock"@
--   and @"guess"@ with their respective argument types.
type GameStateMachineSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "guess" GuessArgs

data GameError =
    GameContractError ContractError
    | GameSMError SM.SMContractError
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''GameError

instance AsContractError GameError where
    _ContractError = _GameContractError . _ContractError

instance SM.AsSMContractError GameError where
    _SMContractError = _GameSMError . SM._SMContractError

-- | Top-level contract, exposing both endpoints.
contract :: Contract () GameStateMachineSchema GameError ()
contract = selectList [lock, guess] >> contract

-- | The token that represents the right to make a guess
newtype GuessToken = GuessToken { unGuessToken :: Value }
    deriving newtype (Eq, Haskell.Show)

token :: MintingPolicyHash -> TokenName -> Value
token mps tn = V.singleton (V.mpsSymbol mps) tn 1

-- | State of the guessing game
data GameState =
    Initialised MintingPolicyHash TokenName HashedString
    -- ^ Initial state. In this state only the 'MintTokens' action is allowed.
    | Locked MintingPolicyHash TokenName HashedString
    -- ^ Funds have been locked. In this state only the 'Guess' action is
    --   allowed.
    | Finished
    -- ^ All funds were unlocked.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Eq GameState where
    {-# INLINABLE (==) #-}
    (Initialised sym tn s) == (Initialised sym' tn' s') = sym == sym' && s == s' && tn == tn'
    (Locked sym tn s) == (Locked sym' tn' s')           = sym == sym' && s == s' && tn == tn'
    Finished == Finished                                = True
    _ == _                                              = traceIfFalse "states not equal" False

-- | Check whether a 'ClearString' is the preimage of a
--   'HashedString'
checkGuess :: HashedString -> ClearString -> Bool
checkGuess (HashedString actual) (ClearString gss) = actual == sha2_256 gss

-- | Inputs (actions)
data GameInput =
      MintToken
    -- ^ Mint the "guess" token
    | Guess ClearString HashedString Value
    -- ^ Make a guess, extract the funds, and lock the remaining funds using a
    --   new secret word.
    deriving stock (Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- The 'GameParam' parameter is not used in the validation. It is meant to
-- parameterize the script address depending based on the value of 'GaramParam'.
{-# INLINABLE transition #-}
transition
    :: GameParam
    -> State GameState
    -> GameInput
    -> Maybe (TxConstraints Void Void, State GameState)
transition _ State{stateData=oldData, stateValue=oldValue} input = case (oldData, input) of
    (Initialised mph tn s, MintToken) ->
        let constraints = Constraints.mustMintCurrency mph tn 1 in
        Just ( constraints
             , State
                { stateData = Locked mph tn s
                , stateValue = oldValue
                }
             )
    (Locked mph tn currentSecret, Guess theGuess nextSecret takenOut)
        | checkGuess currentSecret theGuess ->
        let constraints = Constraints.mustSpendAtLeast (token mph tn)
                       <> Constraints.mustMintCurrency mph tn 0
            newValue = oldValue - takenOut
         in Just ( constraints
                 , State
                    { stateData = if V.isZero (Ada.toValue $ Ada.fromValue newValue)
                                     then Finished
                                     else Locked mph tn nextSecret
                    , stateValue = newValue
                    }
                 )
    _ -> Nothing

type GameStateMachine = SM.StateMachine GameState GameInput

{-# INLINABLE machine #-}
machine :: GameParam -> GameStateMachine
machine gameParam = SM.mkStateMachine Nothing (transition gameParam) isFinal where
    isFinal Finished = True
    isFinal _        = False

{-# INLINABLE mkValidator #-}
mkValidator :: GameParam -> Scripts.ValidatorType GameStateMachine
mkValidator gameParam = SM.mkValidator (machine gameParam)

typedValidator :: GameParam -> Scripts.TypedValidator GameStateMachine
typedValidator = Scripts.mkTypedValidatorParam @GameStateMachine
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.mkUntypedValidator

-- TODO: Ideas welcome for how to make this interface suck less.
-- Doing it this way actually generates coverage locations that we don't care about(!)
covIdx :: GameParam -> CoverageIndex
covIdx gp = getCovIdx ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode gp)

mintingPolicy :: GameParam -> Scripts.MintingPolicy
mintingPolicy gp = Scripts.forwardingMintingPolicy $ typedValidator gp

client :: GameParam -> SM.StateMachineClient GameState GameInput
client gp = SM.mkStateMachineClient $ SM.StateMachineInstance (machine gp) $ typedValidator gp

-- | The @"lock"@ endpoint.
lock :: Promise () GameStateMachineSchema GameError ()
lock = endpoint @"lock" $ \LockArgs{lockArgsGameParam, lockArgsSecret, lockArgsValue} -> do
    let secret = HashedString (escape_sha2_256 (toBuiltin . C.pack <$> extractSecret lockArgsSecret))
        sym = Scripts.forwardingMintingPolicyHash $ typedValidator lockArgsGameParam
    _ <- SM.runInitialise (client lockArgsGameParam) (Initialised sym "guess" secret) lockArgsValue
    void $ SM.runStep (client lockArgsGameParam) MintToken

-- | The @"guess"@ endpoint.
guess :: Promise () GameStateMachineSchema GameError ()
guess = endpoint @"guess" $ \GuessArgs{guessArgsGameParam, guessArgsOldSecret, guessArgsNewSecret, guessArgsValueTakenOut} -> do

    let guessedSecret = ClearString (toBuiltin (C.pack guessArgsOldSecret))
        newSecret     = HashedString (escape_sha2_256 (toBuiltin . C.pack <$> extractSecret guessArgsNewSecret))

    void
        $ SM.runStep (client guessArgsGameParam)
            (Guess guessedSecret newSecret guessArgsValueTakenOut)

PlutusTx.unstableMakeIsData ''GameState
PlutusTx.makeLift ''GameState
PlutusTx.unstableMakeIsData ''GameInput
PlutusTx.makeLift ''GameInput
PlutusTx.makeLift ''GuessToken
PlutusTx.unstableMakeIsData ''GuessToken
