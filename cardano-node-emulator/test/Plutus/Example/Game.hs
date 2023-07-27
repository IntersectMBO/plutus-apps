{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

-- | A guessing game. A simplified version of 'Plutus.Contract.GameStateMachine'
-- using 'Cardano.Node.Emulator.MTL'.
module Plutus.Example.Game
    ( GameParam(..)
    , LockArgs (..)
    , GuessArgs (..)
    , submitLockTx
    , submitGuessTx
    , mkLockTx
    , mkGuessTx
    , mkGameAddress
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.Internal.Node.Params (testnet)
import Control.Monad (void)
import Data.ByteString.Char8 qualified as C
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Ledger (CardanoAddress, POSIXTime, PaymentPrivateKey, UtxoIndex, getValidator)
import Ledger.Address (mkValidatorCardanoAddress)
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts qualified as Scripts
import Plutus.Script.Utils.Typed (ScriptContextV2, Versioned)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import PlutusLedgerApi.V2 (Address, Validator)
import PlutusLedgerApi.V2.Contexts qualified as V2
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

data Game
instance Scripts.ValidatorTypes Game where
    type instance RedeemerType Game = ClearString
    type instance DatumType Game = HashedString

-- | The address of the game (the hash of its validator script)
mkGameAddress :: GameParam -> CardanoAddress
mkGameAddress = mkValidatorCardanoAddress testnet . mkGameValidator

-- | The validator script of the game.
mkGameValidator :: GameParam -> Versioned Validator
mkGameValidator = Scripts.vValidatorScript . mkGameInstance

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
        , lockArgsValue     :: !C.Value
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
mkLockTx :: LockArgs -> (C.CardanoBuildTx, UtxoIndex)
mkLockTx LockArgs { lockArgsGameParam, lockArgsSecret, lockArgsValue } =
    let gameAddr = mkGameAddress lockArgsGameParam
        datum = C.fromPlutusData $ PlutusTx.toData $ hashString lockArgsSecret
        txOut = C.TxOut
            gameAddr
            (C.toCardanoTxOutValue lockArgsValue)
            (C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra datum)
            C.ReferenceScriptNone
        tx = E.emptyTxBodyContent
            { C.txOuts = [txOut]
            }
    in (C.CardanoBuildTx tx, mempty)

-- | The "guess" contract
mkGuessTx
    :: UtxoIndex -- ^ Script utxos to spend
    -> GuessArgs
    -> (C.CardanoBuildTx, UtxoIndex)
mkGuessTx utxos GuessArgs { guessArgsGameParam, guessArgsSecret } =
    let witnessHeader = either (error . show) id $ C.toCardanoTxInScriptWitnessHeader (getValidator <$> mkGameValidator guessArgsGameParam)
        redeemer = C.fromPlutusData $ PlutusTx.toData $ clearString guessArgsSecret
        witness = C.BuildTxWith $ C.ScriptWitness C.ScriptWitnessForSpending $
            witnessHeader C.InlineScriptDatum redeemer C.zeroExecutionUnits
        txIns = (, witness) <$> Map.keys (C.unUTxO utxos)
        tx = E.emptyTxBodyContent
            { C.txIns = txIns
            }
    in (C.CardanoBuildTx tx, utxos)

submitLockTx :: E.MonadEmulator m => CardanoAddress -> PaymentPrivateKey -> LockArgs -> m ()
submitLockTx wallet privateKey lockArgs@LockArgs { lockArgsValue } = do
    E.logInfo @String $ "Pay " <> show lockArgsValue <> " to the script"
    let (utx, utxoIndex) = mkLockTx lockArgs
    void $ E.submitTxConfirmed utxoIndex wallet [privateKey] utx

submitGuessTx :: E.MonadEmulator m => CardanoAddress -> PaymentPrivateKey -> GuessArgs -> m ()
submitGuessTx wallet privateKey guessArgs@GuessArgs { guessArgsGameParam } = do
    E.logInfo @String "Taking a guess"
    utxos <- E.utxosAt (mkGameAddress guessArgsGameParam)
    let (utx, utxoIndex) = mkGuessTx utxos guessArgs
    void $ E.submitTxConfirmed utxoIndex wallet [privateKey] utx
