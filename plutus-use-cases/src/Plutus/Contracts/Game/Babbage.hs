{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

-- | A guessing game. A simplified version of 'Plutus.Contract.GameStateMachine'
-- not using 'Plutus.Contract.StateMachine' and using `yieldUnbalancedTx' for
-- balancing, signing and submitting transactions.
--
-- Currently, remote wallets (anything other than WBE) can only handles
-- `yieldUnbalancedTx` requests, and not `balanceTx`, `signTx` and `submitTx`
-- requests.
--
-- This is the "Babbage version" of the game, it uses features up to the Babbage era.
-- It introduces reference script.
module Plutus.Contracts.Game.Babbage
    ( contract
    , GameParam(..)
    , GameSchema
    , LockArgs(..)
    , GuessArgs(..)
    -- * Scripts
    , gameInstance
    , mkValidator
    -- * Address
    , gameAddress
    , covIdx
    ) where

import Cardano.Node.Emulator.Internal.Node (testnet)
import Control.Lens (_2, (^?))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Ledger (CardanoAddress, DecoratedTxOut, POSIXTime, TxOutRef)
import Ledger.Tx (datumInDatumFromQuery, decoratedTxOutDatum)
import Ledger.Tx.Constraints (mustReferenceOutput)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Contract (AsContractError, Contract, Endpoint, Promise, adjustUnbalancedTx, endpoint,
                        findReferenceValidatorScripByHash, logInfo, mkTxConstraints, ownUtxos, selectList, type (.\/),
                        utxosAt, yieldUnbalancedTx)
import Plutus.Script.Utils.Ada (toValue)
import Plutus.Script.Utils.Typed (ScriptContextV2, validatorHash)
import Plutus.Script.Utils.V2.Address (mkValidatorCardanoAddress)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value)
import Plutus.V2.Ledger.Api (Address, Datum (Datum), Validator)
import Plutus.V2.Ledger.Contexts qualified as V2
import PlutusTx qualified
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude qualified as Haskell

-- | Datatype for creating a parameterized validator.
data GameParam = GameParam
    { gameParamOwner     :: Address
    -- ^ Payment public key hash of the wallet locking some funds
    , gameParamStartTime :: POSIXTime
    -- ^ Starting time of the game
    } deriving (Haskell.Show, Generic)
      deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''GameParam

newtype HashedString = HashedString BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString

type GameSchema =
        Endpoint "init" GameParam
        .\/ Endpoint "lock" LockArgs
        .\/ Endpoint "guess" GuessArgs

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

-- | The address of the game (the hash of its validator script)
gameAddress :: GameParam -> CardanoAddress
gameAddress = mkValidatorCardanoAddress testnet . gameValidator

-- | The validator script of the game.
gameValidator :: GameParam -> Validator
gameValidator = Scripts.validatorScript . gameInstance

gameInstance :: GameParam -> V2.TypedValidator Game
gameInstance = V2.mkTypedValidatorParam @Game
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
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- TODO: Ideas welcome for how to make this interface suck less.
-- Doing it this way actually generates coverage locations that we don't care about(!)
covIdx :: GameParam -> CoverageIndex
covIdx gameParam =
    getCovIdx ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode gameParam)

-- create a data script for the guessing game by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: Haskell.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

-- | Arguments for the @"lock"@ endpoint
data LockArgs =
    LockArgs
        { lockArgsGameParam   :: GameParam
        -- ^ The parameters for parameterizing the validator.
        , lockArgsGameAddress :: CardanoAddress
        -- ^ The addcess that hosts the game
        , lockArgsSecret      :: Haskell.String -- SecretArgument Haskell.String
        -- ^ The secret
        , lockArgsValue       :: Value
        -- ^ Value that is locked by the contract initially
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs =
    GuessArgs
        { guessArgsGameParam   :: GameParam
        -- ^ The parameters for parameterizing the validator.
        , guessArgsGameAddress :: CardanoAddress
        -- ^ The addcess that hosts the game
        , guessArgsSecret      :: Haskell.String
        -- ^ The guess
        } deriving stock (Haskell.Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

-- | The "init" contract endpoint. Create a utxo that has a script reference to the game contract
init :: AsContractError e => Promise () GameSchema e ()
init = endpoint @"init" $ \gameParam -> do
    logInfo @Haskell.String $ "Game rule owned by " <> Haskell.show (gameParamOwner gameParam)
    let game = gameInstance gameParam
        lookups = Constraints.typedValidatorLookups game
        gameHash = validatorHash game
        tx = Constraints.mustPayToAddressWithReferenceValidator (gameParamOwner gameParam) gameHash Nothing (toValue 2)
    mkTxConstraints lookups tx >>= adjustUnbalancedTx >>= yieldUnbalancedTx

-- | The "lock" contract endpoint. See note [Contract endpoints]
lock :: AsContractError e => Promise () GameSchema e ()
lock = endpoint @"lock" $ \LockArgs { lockArgsGameParam, lockArgsGameAddress, lockArgsSecret, lockArgsValue } -> do
    logInfo @Haskell.String $ "Pay " <> Haskell.show lockArgsValue <> " to the script"
    let lookups = Constraints.typedValidatorLookups (gameInstance lockArgsGameParam)
        gameHash = validatorHash $ gameInstance lockArgsGameParam
    gameRef <- findReferenceValidatorScripByHash gameHash lockArgsGameAddress
    let tx =  mustReferenceOutput gameRef Haskell.<>
              Constraints.mustPayToTheScriptWithDatumInTx (hashString lockArgsSecret) lockArgsValue
    mkTxConstraints lookups tx >>= adjustUnbalancedTx >>= yieldUnbalancedTx

-- | The "guess" contract endpoint. See note [Contract endpoints]
guess :: AsContractError e => Promise () GameSchema e ()
guess = endpoint @"guess" $ \GuessArgs { guessArgsGameParam, guessArgsGameAddress, guessArgsSecret } -> do
    -- Wait for script to have a UTxO of a least 1 lovelace
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- utxosAt $ gameAddress guessArgsGameParam
    let game = gameInstance guessArgsGameParam
        gameHash = validatorHash game
    gameUtxos <- utxosAt guessArgsGameAddress
    gameRef <- findReferenceValidatorScripByHash gameHash guessArgsGameAddress
    collateral <- fmap (head . Map.toList) ownUtxos
    let lookups = Constraints.typedValidatorLookups (gameInstance guessArgsGameParam)
               Haskell.<> Constraints.unspentOutputs utxos
               Haskell.<> Constraints.unspentOutputs gameUtxos
               Haskell.<> Constraints.unspentOutputs (uncurry Map.singleton collateral)
        redeemer = clearString guessArgsSecret
        tx       = Constraints.spendUtxosFromTheReferencedScript utxos redeemer gameRef
                <> Constraints.mustUseOutputAsCollateral (fst collateral)
    unbalancedTx <- mkTxConstraints lookups tx
    yieldUnbalancedTx unbalancedTx

-- | Find the secret word in the Datum of the UTxOs
findSecretWordValue :: Map TxOutRef DecoratedTxOut -> Maybe HashedString
findSecretWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output is possible
secretWordValue :: DecoratedTxOut -> Maybe HashedString
secretWordValue o = do
  Datum d <- o ^? decoratedTxOutDatum . _2 . datumInDatumFromQuery
  PlutusTx.fromBuiltinData d

contract :: AsContractError e => Contract () GameSchema e ()
contract = do
    logInfo @Haskell.String "Waiting for lock or guess endpoint..."
    selectList [init, lock, guess] >> contract
