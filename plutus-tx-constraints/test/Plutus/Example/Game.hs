{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

-- | A guessing game. A simplified version of 'Plutus.Contract.GameStateMachine'
-- using 'Cardano.Node.Emulator.MTL'.
module Plutus.Example.Game
    ( GameParam(..)
    , LockArgs (..)
    , GuessArgs (..)
    , mkLockTx
    , mkGuessTx
    , mkGameAddress
    ) where

import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.Params (testnet)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import GHC.Generics (Generic)
import Ledger (CardanoAddress, POSIXTime, TxOutRef)
import Ledger.Tx (DecoratedTxOut)
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Typed (ScriptContextV2)
import Plutus.Script.Utils.V2.Address (mkValidatorCardanoAddress)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value)
import Plutus.V2.Ledger.Api (Address, Validator)
import Plutus.V2.Ledger.Contexts qualified as V2
import PlutusTx (FromData, ToData)
import PlutusTx qualified
import PlutusTx.Prelude ()
import PlutusTx.Prelude qualified as PlutusTx

-- | Datatype for creating a parameterized validator.
data GameParam = GameParam
    { gameParamPayeeAddr :: !Address
    -- ^ Payment public key hash of the wallet locking some funds
    , gameParamStartTime :: !POSIXTime
    -- ^ Starting time of the game
    } deriving (Show, Generic)

PlutusTx.makeLift ''GameParam

newtype HashedString = HashedString PlutusTx.BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString PlutusTx.BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

data GameError =
      MkTxError !Constraints.MkTxError
    | ToCardanoError !C.ToCardanoError
    deriving (Show)

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

-- | The address of the game (the hash of its validator script)
mkGameAddress :: GameParam -> CardanoAddress
mkGameAddress = mkValidatorCardanoAddress testnet . mkGameValidator

-- | The validator script of the game.
mkGameValidator :: GameParam -> Validator
mkGameValidator = Scripts.validatorScript . mkGameInstance

mkGameInstance :: GameParam -> V2.TypedValidator Game
mkGameInstance = V2.mkTypedValidatorParam @Game
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @ScriptContextV2 @HashedString @ClearString

-- | The validation function (Datum -> Redeemer -> ScriptContext -> Bool)
--
-- The 'GameParam' parameter is not used in the validation. It is meant to
-- parameterize the script address depending based on the value of 'GaramParam'.
{-# INLINABLE mkValidator #-}
mkValidator :: GameParam -> HashedString -> ClearString -> V2.ScriptContext -> Bool
mkValidator _ hs cs _ = isGoodGuess hs cs

{-# INLINABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool
isGoodGuess (HashedString actual) (ClearString guess') = actual PlutusTx.== PlutusTx.sha2_256 guess'

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: String -> HashedString
hashString = HashedString . PlutusTx.sha2_256 . PlutusTx.toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: String -> ClearString
clearString = ClearString . PlutusTx.toBuiltin . C.pack

-- | Arguments for the @"lock"@ endpoint
data LockArgs =
    LockArgs
        { lockArgsGameParam :: !GameParam
        -- ^ The parameters for parameterizing the validator.
        , lockArgsSecret    :: !String
        -- ^ The secret
        , lockArgsValue     :: !Value
        -- ^ Value that is locked by the contract initially
        } deriving stock (Show, Generic)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs =
    GuessArgs
        { guessArgsGameParam :: !GameParam
        -- ^ The parameters for parameterizing the validator.
        , guessArgsSecret    :: !String
        -- ^ The guess
        } deriving stock (Show, Generic)

-- | The "lock" contract
-- TODO Replace use of 'Params' as it's only used for emulator
mkLockTx :: E.Params -> LockArgs -> Either GameError Constraints.UnbalancedTx
mkLockTx params LockArgs { lockArgsGameParam, lockArgsSecret, lockArgsValue } = do
    let lookups = Constraints.typedValidatorLookups (mkGameInstance lockArgsGameParam)
        constraints = Constraints.mustPayToTheScriptWithDatumInTx (hashString lockArgsSecret) lockArgsValue
    tx <- first MkTxError $ Constraints.mkTx params lookups constraints
    adjustedTx <- first ToCardanoError $ Constraints.adjustUnbalancedTx (E.emulatorPParams params) tx
    pure $ snd adjustedTx

-- | The "guess" contract
mkGuessTx
    :: E.Params
    -> Map TxOutRef DecoratedTxOut -- ^ Script utxos to spend
    -> GuessArgs
    -> Either GameError Constraints.UnbalancedTx
mkGuessTx params utxos GuessArgs { guessArgsGameParam, guessArgsSecret } = do
    let lookups = Constraints.typedValidatorLookups (mkGameInstance guessArgsGameParam)
               <> Constraints.unspentOutputs utxos
        redeemer = clearString guessArgsSecret
        constraints = Constraints.spendUtxosFromTheScript utxos redeemer
    tx <- first MkTxError $ Constraints.mkTx params lookups constraints
    adjustedTx <- first ToCardanoError $ Constraints.adjustUnbalancedTx (E.emulatorPParams params) tx
    pure $ snd adjustedTx
