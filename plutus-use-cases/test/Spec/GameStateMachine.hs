{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
module Spec.GameStateMachine
  ( tests, successTrace, successTrace2, traceLeaveOneAdaInScript, failTrace
  , prop_Game, propGame', prop_GameWhitelist
  , check_prop_Game_with_coverage
  , prop_NoLockedFunds
  , prop_CheckNoLockedFundsProof
  , prop_SanityCheckModel
  , prop_GameCrashTolerance
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Freer.Extras.Log (LogLevel (..))
import Data.Maybe
import Test.QuickCheck as QC hiding (checkCoverage, (.&&.))
import Test.Tasty hiding (after)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value, isZero)
import Plutus.Contract.Secrets
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import Plutus.Contracts.GameStateMachine as G
import Plutus.Trace.Emulator as Trace
import PlutusTx qualified
import PlutusTx.Coverage
--
-- * QuickCheck model

data GameModel = GameModel
    { _gameValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String }
    deriving (Show)

makeLenses 'GameModel

deriving instance Eq (ContractInstanceKey GameModel w schema err)
deriving instance Ord (ContractInstanceKey GameModel w schema err)
deriving instance Show (ContractInstanceKey GameModel w schema err)

instance ContractModel GameModel where

    data ContractInstanceKey GameModel w schema err where
        WalletKey :: Wallet -> ContractInstanceKey GameModel () GameStateMachineSchema GameError

    -- The commands available to a test case
    data Action GameModel = Lock      Wallet String Integer
                          | Guess     Wallet String String Integer
                          | GiveToken Wallet
        deriving (Eq, Show)

    initialState = GameModel
        { _gameValue     = 0
        , _hasToken      = Nothing
        , _currentSecret = ""
        }

    initialHandleSpecs = [ ContractInstanceSpec (WalletKey w) w G.contract | w <- wallets ]

    -- 'perform' gets a state, which includes the GameModel state, but also contract handles for the
    -- wallets and what the model thinks the current balances are.
    perform handle s cmd = case cmd of
        Lock w new val -> do
            Trace.callEndpoint @"lock" (handle $ WalletKey w)
                         LockArgs{lockArgsSecret = secretArg new, lockArgsValue = Ada.lovelaceValueOf val}
            delay 2
        Guess w old new val -> do
            Trace.callEndpoint @"guess" (handle $ WalletKey w)
                GuessArgs{ guessArgsOldSecret = old
                         , guessArgsNewSecret = secretArg new
                         , guessArgsValueTakenOut = Ada.lovelaceValueOf val}
            delay 1
        GiveToken w' -> do
            let w = fromJust (s ^. contractState . hasToken)
            _ <- Trace.payToWallet w w' gameTokenVal
            delay 1

    -- 'nextState' descibes how each command affects the state of the model

    nextState (Lock w secret val) = do
        hasToken      $= Just w
        currentSecret $= secret
        gameValue     $= val
        mint gameTokenVal
        deposit w gameTokenVal
        withdraw w $ Ada.lovelaceValueOf val
        wait 2

    nextState (Guess w old new val) = do
        correct <- (old ==) <$> viewContractState currentSecret
        when correct $ do
            currentSecret $= new
            gameValue     $~ subtract val
            deposit w $ Ada.lovelaceValueOf val
        wait 1

    nextState (GiveToken w) = do
        w0 <- fromJust <$> viewContractState hasToken
        withdraw w0 $ Ada.toValue Ledger.minAdaTxOut
        deposit w $ Ada.toValue Ledger.minAdaTxOut
        transfer w0 w gameTokenVal
        hasToken $= Just w
        wait 1

    -- To generate a random test case we need to know how to generate a random
    -- command given the current model state.
    arbitraryAction s = oneof $
        [ genLockAction ] ++
        [ Guess w   <$> genGuess  <*> genGuess <*> genGuessAmount | val > Ada.getLovelace Ledger.minAdaTxOut, Just w <- [tok] ] ++
        [ GiveToken <$> genWallet | isJust tok ]
        where
            genGuessAmount = frequency [(1, pure val), (1, pure $ Ada.getLovelace Ledger.minAdaTxOut), (8, choose (Ada.getLovelace Ledger.minAdaTxOut, val))]
            tok = s ^. contractState . hasToken
            val = s ^. contractState . gameValue
            genLockAction :: Gen (Action GameModel)
            genLockAction = do
              w <- genWallet
              let currentWalletBalance = Ada.getLovelace $ Ada.adaOf 100 + Ada.fromValue (s ^. balanceChange w)
              pure (Lock w) <*> genGuess <*> choose (Ada.getLovelace Ledger.minAdaTxOut, currentWalletBalance)

    -- The 'precondition' says when a particular command is allowed.
    precondition s cmd = case cmd of
            -- In order to lock funds, we need to satifsy the constraint where
            -- each tx out must have at least N Ada.
            -- When we lock a value, we must make sure that we don't lock too
            -- much funds, such that we can't pay to fees anymore and have a tx
            -- out of less than N Ada.
            -- We must also leave enough funds in order to transfer the game
            -- token (along with 2 Ada) to another wallet.
            Lock w _ v    -> let currentWalletBalance = Ada.adaOf 100 + Ada.fromValue (s ^. balanceChange w)
                              in currentWalletBalance - Ada.lovelaceOf v - Ledger.minAdaTxOut >= Ledger.minAdaTxOut + Ledger.maxFee
                                 && v >= Ada.getLovelace Ledger.minAdaTxOut
                                 && isNothing tok
            Guess w _ _ v -> (val == v || Ada.Lovelace (val - v) >= Ledger.minAdaTxOut) && tok == Just w
            GiveToken _   -> isJust tok
        where
            tok = s ^. contractState . hasToken
            val = s ^. contractState . gameValue

    shrinkAction _s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- shrink val]
    shrinkAction _s (GiveToken w) =
        [GiveToken w' | w' <- shrinkWallet w]
    shrinkAction _s (Guess w old new val) =
        [Guess w' old new val | w' <- shrinkWallet w] ++
        [Guess w old new val' | val' <- shrink val]

    monitoring _ _ = id

instance CrashTolerance GameModel where
  available (Lock w _ _) specs    = ContractInstanceSpec (WalletKey w) w G.contract `elem` specs
  available (Guess w _ _ _) specs = ContractInstanceSpec (WalletKey w) w G.contract `elem` specs
  available _ _                   = True

-- | The main property. 'propRunActions_' checks that balances match the model after each test.
prop_Game :: Actions GameModel -> Property
prop_Game = propRunActions_

prop_GameWhitelist :: Actions GameModel -> Property
prop_GameWhitelist = checkErrorWhitelist defaultWhitelist

prop_SanityCheckModel :: Property
prop_SanityCheckModel = propSanityCheckModel @GameModel

check_prop_Game_with_coverage :: IO CoverageReport
check_prop_Game_with_coverage =
  quickCheckWithCoverage (set coverageIndex covIdx $ defaultCoverageOptions) $ \covopts ->
    propRunActionsWithOptions @GameModel defaultCheckOptions
                                         covopts
                                         (const (pure True))

propGame' :: LogLevel -> Actions GameModel -> Property
propGame' l = propRunActionsWithOptions
                (set minLogLevel l defaultCheckOptions)
                defaultCoverageOptions
                (\ _ -> pure True)

prop_GameCrashTolerance :: Actions (WithCrashTolerance GameModel) -> Property
prop_GameCrashTolerance = propRunActions_

wallets :: [Wallet]
wallets = [w1, w2, w3]

genWallet :: Gen Wallet
genWallet = QC.elements wallets

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

genGuess :: Gen String
genGuess = QC.elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = choose (Ada.getLovelace Ledger.minAdaTxOut, 100_000_000)

delay :: Int -> EmulatorTraceNoStartContract ()
delay n = void $ Trace.waitNSlots (fromIntegral n)

-- Dynamic Logic ----------------------------------------------------------

prop_UnitTest :: Property
prop_UnitTest = withMaxSuccess 1 $ forAllDL unitTest prop_Game

unitTest :: DL GameModel ()
unitTest = do
    val <- forAllQ $ chooseQ (5_000_000, 20_000_000)
    action $ Lock w1 "hello" val
    action $ GiveToken w2
    action $ Guess w2 "hello" "new secret" 3_000_000

unitTest2 :: DL GameModel ()
unitTest2 = do
    unitTest
    action $ GiveToken w3
    action $ Guess w3 "new secret" "hello" 4_000_000

unitTestFail :: DL GameModel ()
unitTestFail = do
    action $ Lock w1 "hello" 8_000_000
    action $ GiveToken w2
    action $ Guess w2 "hola" "new secret" 3_000_000

noLockedFunds :: DL GameModel ()
noLockedFunds = do
    anyActions_
    w      <- forAllQ $ elementsQ wallets
    secret <- viewContractState currentSecret
    val    <- viewContractState gameValue
    when (val > Ada.getLovelace Ledger.minAdaTxOut) $ do
        monitor $ label "Unlocking funds"
        action $ GiveToken w
        action $ Guess w secret "" val
    assertModel "Locked funds should be zero" $ isZero . lockedValue

-- | Check that we can always get the money out of the guessing game (by guessing correctly).
prop_NoLockedFunds :: Property
prop_NoLockedFunds = forAllDL noLockedFunds prop_Game

noLockProof :: NoLockedFundsProof GameModel
noLockProof = NoLockedFundsProof{
      nlfpMainStrategy   = mainStrat,
      nlfpWalletStrategy = walletStrat }
    where
        mainStrat = do
            hasTok <- viewContractState hasToken
            secret <- viewContractState currentSecret
            val    <- viewContractState gameValue
            case hasTok of
                Nothing -> return ()
                Just w  -> action (Guess w secret "" val)

        walletStrat w = do
            hasTok <- (== Just w) <$> viewContractState hasToken
            secret <- viewContractState currentSecret
            val    <- viewContractState gameValue
            when hasTok $ action (Guess w secret "" val)

prop_CheckNoLockedFundsProof :: Property
prop_CheckNoLockedFundsProof = checkNoLockedFundsProof defaultCheckOptions noLockProof

-- * Unit tests

tests :: TestTree
tests =
    testGroup "game state machine with secret arguments tests"
    [ checkPredicate "run a successful game trace"
        (walletFundsChange w2 (Ada.toValue Ledger.minAdaTxOut <> Ada.adaValueOf 3 <> gameTokenVal)
        .&&. valueAtAddress (Scripts.validatorAddress G.typedValidator) (Ada.adaValueOf 5 ==)
        .&&. walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> Ada.adaValueOf (-8)))
        successTrace

    , checkPredicate "run a 2nd successful game trace"
        (walletFundsChange w2 (Ada.adaValueOf 3)
        .&&. valueAtAddress (Scripts.validatorAddress G.typedValidator) (Ada.adaValueOf 0 ==)
        .&&. walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> Ada.adaValueOf (-8))
        .&&. walletFundsChange w3 (Ada.toValue Ledger.minAdaTxOut <> Ada.adaValueOf 5 <> gameTokenVal))
        successTrace2

    , checkPredicate "run a successful game trace where we try to leave 1 Ada in the script address"
        (walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> gameTokenVal)
        .&&. valueAtAddress (Scripts.validatorAddress G.typedValidator) (Ada.toValue Ledger.minAdaTxOut ==))
        traceLeaveOneAdaInScript

    , checkPredicate "run a failed trace"
        (walletFundsChange w2 (Ada.toValue Ledger.minAdaTxOut <> gameTokenVal)
        .&&. valueAtAddress (Scripts.validatorAddress G.typedValidator) (Ada.adaValueOf 8 ==)
        .&&. walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> Ada.adaValueOf (-8)))
        failTrace

    , goldenPir "test/Spec/gameStateMachine.pir" $$(PlutusTx.compile [|| mkValidator ||])

    , HUnit.testCaseSteps "script size is reasonable" $ \step ->
        reasonable' step (Scripts.validatorScript G.typedValidator) 49000

    , testProperty "can always get the funds out" $
        withMaxSuccess 10 prop_NoLockedFunds

    , testProperty "sanity check the contract model" $
        prop_SanityCheckModel
    ]

initialVal :: Value
initialVal = Ada.adaValueOf 10

-- | Wallet 1 locks some funds, transfers the token to wallet 2
--   which then makes a correct guess and locks the remaining
--   funds with a new secret
successTrace :: EmulatorTrace ()
successTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs{lockArgsSecret=secretArg "hello", lockArgsValue= Ada.adaValueOf 8}
    -- One slot for sending the Ada to the script and one slot for minting the
    -- guess token
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    Trace.callEndpoint @"guess" hdl2 GuessArgs{guessArgsOldSecret="hello", guessArgsNewSecret=secretArg "new secret", guessArgsValueTakenOut=Ada.adaValueOf 3}
    void $ Trace.waitNSlots 1

-- | Run 'successTrace', then wallet 2 transfers the token to wallet 3, which
--   makes another correct guess
successTrace2 :: EmulatorTrace ()
successTrace2 = do
    successTrace
    _ <- Trace.payToWallet w2 w3 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl3 <- Trace.activateContractWallet w3 G.contract
    Trace.callEndpoint @"guess" hdl3 GuessArgs{guessArgsOldSecret="new secret", guessArgsNewSecret=secretArg "hello", guessArgsValueTakenOut=Ada.adaValueOf 5}
    void $ Trace.waitNSlots 1

-- | Tests whether the contract correctly handles the case where we leave less
-- than 2 Ada in the script address after guessing.
traceLeaveOneAdaInScript :: EmulatorTrace ()
traceLeaveOneAdaInScript = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs{lockArgsSecret=secretArg "hello", lockArgsValue= Ada.adaValueOf 8}
    _ <- Trace.waitNSlots 2
    _ <- Trace.callEndpoint @"guess" hdl GuessArgs{guessArgsOldSecret="hello", guessArgsNewSecret=secretArg "new secret", guessArgsValueTakenOut=Ada.adaValueOf 7}
    void $ Trace.waitNSlots 1

-- | Wallet 1 locks some funds, transfers the token to wallet 2
--   which then makes a wrong guess
failTrace :: EmulatorTrace ()
failTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs{lockArgsSecret=secretArg "hello", lockArgsValue= Ada.adaValueOf 8}
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 gameTokenVal
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    _ <- Trace.callEndpoint @"guess" hdl2 GuessArgs{guessArgsOldSecret="hola", guessArgsNewSecret=secretArg "new secret", guessArgsValueTakenOut=Ada.adaValueOf 3}
    void $ Trace.waitNSlots 1

gameTokenVal :: Value
gameTokenVal =
    let sym = Scripts.forwardingMintingPolicyHash G.typedValidator
    in G.token sym "guess"
