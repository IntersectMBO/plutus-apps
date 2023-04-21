{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

-- | A guessing game. A simplified version of 'Plutus.Contract.GameStateMachine'
-- using 'Cardano.Node.Emulator.MTL'.
module Game (tests) where

import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.MTL
import Cardano.Node.Emulator.MTL.Test
import Cardano.Node.Emulator.Params (testnet)
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Control.Lens (_2, makeLenses, (%=), (&), (.=), (^.), (^?))
import Control.Monad (void, when)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.RWS.Strict (ask, evalRWS)
import Data.ByteString.Char8 qualified as C
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger (CardanoAddress, CardanoTx, POSIXTime, PaymentPubKeyHash, TxOutRef)
import Ledger qualified
import Ledger.Tx (DecoratedTxOut (..), datumInDatumFromQuery, decoratedTxOutDatum)
import Ledger.Tx.CardanoAPI (fromCardanoSlotNo)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value.CardanoAPI qualified as C
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Typed (ScriptContextV2)
import Plutus.Script.Utils.V2.Address (mkValidatorCardanoAddress)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as V2
import Plutus.Script.Utils.Value (Value)
import Plutus.V2.Ledger.Api (Datum (Datum), Validator)
import Plutus.V2.Ledger.Contexts qualified as V2
import PlutusTx (FromData, ToData, toBuiltinData)
import PlutusTx qualified
import PlutusTx.Code (getCovIdx)
import PlutusTx.Coverage (CoverageIndex)
import PlutusTx.Prelude hiding (pure, (<$>), (<*>))
import Prelude (Show, String, pure, subtract, (<$>), (<*>))
import Prelude qualified as Haskell

import Control.Monad.Identity (Identity)
import Control.Monad.RWS (RWST)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (WriterT)
import Data.Default (def)
import Ledger.Value.CardanoAPI qualified as Value
import Test.QuickCheck qualified as QC
import Test.QuickCheck.ContractModel as QCCM
import Test.QuickCheck.DynamicLogic (chooseQ, elementsQ, forAllQ)
import Test.QuickCheck.StateModel (Realized)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck


-- | Datatype for creating a parameterized validator.
data GameParam = GameParam
    { gameParamPayeeAddr :: Ledger.Address
    -- ^ Payment public key hash of the wallet locking some funds
    , gameParamStartTime :: POSIXTime
    -- ^ Starting time of the game
    } deriving (Show, Generic)

PlutusTx.makeLift ''GameParam

newtype HashedString = HashedString BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString
    deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString



mkTx
  :: ( FromData (Scripts.DatumType a)
       , ToData (Scripts.DatumType a)
       , ToData (Scripts.RedeemerType a)
       )
  => E.Params
  -> Constraints.ScriptLookups a
  -> Constraints.TxConstraints (Scripts.RedeemerType a) (Scripts.DatumType a)
  -> Constraints.UnbalancedTx
mkTx params lookups constraints =
  Constraints.mkTx params lookups constraints
  & either (Haskell.error . Haskell.show) id
  & Constraints.adjustUnbalancedTx (E.emulatorPParams params)
  & either (Haskell.error . Haskell.show) snd

submitTxConfirmed :: MonadEmulator m => CardanoAddress -> Constraints.UnbalancedTx -> m CardanoTx
submitTxConfirmed addr (Constraints.UnbalancedCardanoTx utx utxoIndex) = do
  let privateKey = Haskell.lookup addr $ zip E.knownAddresses E.knownPaymentPrivateKeys
  tx <- submitUnbalancedTx utxoIndex addr privateKey utx
  nextSlot
  Haskell.pure tx

submitTxConstraints
  :: MonadEmulator m
  => CardanoAddress
  -> Constraints.ScriptLookups Game
  -> Constraints.TxConstraints ClearString HashedString
  -> m CardanoTx
submitTxConstraints addr lookups constraints = do
  params <- ask
  submitTxConfirmed addr
    $ mkTx @Game params lookups constraints

w1, w2, w3 :: CardanoAddress
w1 : w2 : w3 : _ = E.knownAddresses


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
        { lockArgsGameParam :: GameParam
        -- ^ The parameters for parameterizing the validator.
        , lockArgsSecret    :: Haskell.String
        -- ^ The secret
        , lockArgsValue     :: Value
        -- ^ Value that is locked by the contract initially
        } deriving stock (Haskell.Show, Generic)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs =
    GuessArgs
        { guessArgsGameParam :: GameParam
        -- ^ The parameters for parameterizing the validator.
        , guessArgsSecret    :: Haskell.String
        -- ^ The guess
        } deriving stock (Haskell.Show, Generic)

-- | The "lock" contract
lock :: MonadEmulator m => CardanoAddress -> LockArgs -> m ()
lock wallet LockArgs { lockArgsGameParam, lockArgsSecret, lockArgsValue } = do
    logInfo @Haskell.String $ "Pay " <> Haskell.show lockArgsValue <> " to the script"
    let lookups = Constraints.typedValidatorLookups (gameInstance lockArgsGameParam)
        tx = Constraints.mustPayToTheScriptWithDatumInTx (hashString lockArgsSecret) lockArgsValue
    void $ submitTxConstraints wallet lookups tx

-- | The "guess" contract
guess :: MonadEmulator m => CardanoAddress -> GuessArgs -> m ()
guess wallet GuessArgs { guessArgsGameParam, guessArgsSecret } = do
    logInfo @Haskell.String "Waiting for script to have a UTxO of at least 1 lovelace"
    utxos <- utxosAt (gameAddress guessArgsGameParam)

    let lookups = Constraints.typedValidatorLookups (gameInstance guessArgsGameParam)
               Haskell.<> Constraints.unspentOutputs utxos
        redeemer = clearString guessArgsSecret
        tx       = Constraints.spendUtxosFromTheScript utxos redeemer

    void $ submitTxConstraints wallet lookups tx

-- | Find the secret word in the Datum of the UTxOs
findSecretWordValue :: Map TxOutRef DecoratedTxOut -> Maybe HashedString
findSecretWordValue =
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output is possible
secretWordValue :: DecoratedTxOut -> Maybe HashedString
secretWordValue o = do
  Datum d <- o ^? decoratedTxOutDatum . _2 . datumInDatumFromQuery
  PlutusTx.fromBuiltinData d



gameParam :: GameParam
gameParam = GameParam (Ledger.toPlutusAddress w1) (TimeSlot.scSlotZeroTime def)

-- * QuickCheck model

data GameModel = GameModel
    { _gameValue     :: Integer
    , _currentSecret :: String
    }
    deriving (Show, Generic)

makeLenses 'GameModel


type Wallet = Haskell.Integer

instance ContractModel GameModel where

    -- The commands available to a test case
    data Action GameModel = Lock      Wallet String Integer
                          | Guess     Wallet String
        deriving (Haskell.Eq, Show, Generic)

    initialState = GameModel
        { _gameValue     = 0
        , _currentSecret = ""
        }

    -- 'nextState' describes how each command affects the state of the model
    nextState (Lock w secret val) = do
        currentSecret .= secret
        gameValue     .= val
        withdraw (E.knownAddresses !! pred w) $ Value.lovelaceValueOf val
        wait 2

    nextState (Guess w guess) = do
        correct <- (guess Haskell.==) <$> viewContractState currentSecret
        val <- viewContractState gameValue
        when correct $ do
            gameValue     .= 0
            deposit (E.knownAddresses !! pred w) $ Value.lovelaceValueOf val
        wait 1

    -- To generate a random test case we need to know how to generate a random
    -- command given the current model state.
    arbitraryAction s = QC.oneof $
        genLockAction :
        [ pure (Guess w secret) | val > minOut, w <- wallets ]
        where
            minOut = Ada.getLovelace Ledger.minAdaTxOutEstimated
            val = s ^. contractState . gameValue
            secret = s ^. contractState . currentSecret
            genLockAction :: QC.Gen (Action GameModel)
            genLockAction = do
              w <- genWallet
              pure (Lock w) <*> genGuess <*> QC.choose (Ada.getLovelace Ledger.minAdaTxOutEstimated, Ada.getLovelace (Ada.adaOf 100))

    -- The 'precondition' says when a particular command is allowed.
    precondition s cmd = case cmd of
            -- In order to lock funds, we need to satifsy the constraint where
            -- each tx out must have at least N Ada.
            Lock _ _ v -> val == 0 && v >= Ada.getLovelace Ledger.minAdaTxOutEstimated
            Guess{}    -> True
        where
            val = s ^. contractState . gameValue

    shrinkAction _s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- QC.shrink val]
    shrinkAction _s (Guess w guess) =
        [Guess w' guess | w' <- shrinkWallet w]

instance RunModel GameModel EmulatorM where
    -- 'perform' gets a state, which includes the GameModel state, but also contract handles for the
    -- wallets and what the model thinks the current balances are.
    perform s cmd _ = case cmd of
        Lock w secret val -> do
            lift $ lock (E.knownAddresses !! pred w)
                LockArgs { lockArgsGameParam = gameParam
                         , lockArgsSecret = secret
                         , lockArgsValue = Ada.lovelaceValueOf val
                         }
        Guess w secret -> do
            lift $ guess (E.knownAddresses !! pred w)
                GuessArgs { guessArgsGameParam = gameParam
                          , guessArgsSecret = secret
                          }


prop_SanityCheckModel :: QC.Property
prop_SanityCheckModel = propSanityCheckModel @GameModel

prop_SanityCheckAssertions :: Actions GameModel -> QC.Property
prop_SanityCheckAssertions = propSanityCheckAssertions

wallets :: [Wallet]
wallets = [1, 2, 3]

genWallet :: QC.Gen Wallet
genWallet = QC.elements wallets

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = filter (< w) wallets

genGuess :: QC.Gen String
genGuess = QC.elements ["hello", "secret", "hunter2", "*******"]

-- Dynamic Logic ----------------------------------------------------------

prop_UnitTest :: QC.Property
prop_UnitTest = QC.withMaxSuccess 1 $ forAllDL unitTest2 propRunActions_

unitTest1 :: DL GameModel ()
unitTest1 = do
    val <- forAllQ $ chooseQ (5_000_000, 20_000_000)
    action $ Lock 1 "hello" val
    action $ Guess 2 "hello"

unitTest2 :: DL GameModel ()
unitTest2 = do
    unitTest1
    action $ Guess 2 "new secret"

unitTestFail :: DL GameModel ()
unitTestFail = do
    action $ Lock 1 "hello" 8_000_000
    action $ Guess 2 "hola"

noLockedFunds :: DL GameModel ()
noLockedFunds = do
    anyActions_
    w      <- forAllQ $ elementsQ wallets
    secret <- viewContractState currentSecret
    val    <- viewContractState gameValue
    when (val >= Ada.getLovelace Ledger.minAdaTxOutEstimated) $ do
        monitor $ QC.label "Unlocking funds"
        action $ Guess w secret
    assertModel "Locked funds should be zero" $ symIsZero . lockedValue

-- | Check that we can always get the money out of the guessing game (by guessing correctly).
prop_NoLockedFunds :: QC.Property
prop_NoLockedFunds = forAllDL noLockedFunds propRunActions_

tests :: TestTree
tests =
    testGroup "game (MTL) tests"
    [
      testProperty "can always get the funds out" $
        withMaxSuccess 10 prop_NoLockedFunds

    , testProperty "sanity check the contract model" prop_SanityCheckModel
    , testProperty "sanity check assertions" prop_SanityCheckAssertions
    , testProperty "unit test" prop_UnitTest
    ]
