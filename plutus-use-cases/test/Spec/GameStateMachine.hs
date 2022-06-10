{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE ImportQualifiedPost  #-}
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
  ( tests, successTrace, successTrace2, traceLeaveTwoAdaInScript, failTrace
  , runTestsWithCoverage
  , prop_Game, propGame', prop_GameWhitelist
  , check_prop_Game_with_coverage
  , prop_NoLockedFunds
  , prop_CheckNoLockedFundsProof
  , prop_SanityCheckModel
  , prop_SanityCheckAssertions
  , prop_GameCrashTolerance
  , certification
  , covIndex
  ) where

import Control.Exception hiding (handle)
import Control.Lens
import Control.Monad
import Control.Monad.Freer.Extras.Log (LogLevel (..))
import Data.Data
import Data.Maybe
import Test.QuickCheck as QC hiding (checkCoverage, (.&&.))
import Test.Tasty hiding (after)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.QuickCheck (testProperty)

import Data.Default (Default (def))
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Plutus.Contract.Secrets
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.Certification
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import Plutus.Contract.Test.Coverage
import Plutus.Contracts.GameStateMachine as G
import Plutus.Trace.Emulator as Trace
import PlutusTx qualified
import PlutusTx.Coverage

gameParam :: G.GameParam
gameParam = G.GameParam (mockWalletPaymentPubKeyHash w1) (TimeSlot.scSlotZeroTime def)

options :: CheckOptions
options = defaultCheckOptionsContractModel & allowBigTransactions

--
-- * QuickCheck model

data GameModel = GameModel
    { _gameValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String
    }
    deriving (Show, Data)

makeLenses 'GameModel

deriving instance Eq (ContractInstanceKey GameModel w schema err params)
deriving instance Ord (ContractInstanceKey GameModel w schema err params)
deriving instance Show (ContractInstanceKey GameModel w schema err params)

instance ContractModel GameModel where

    data ContractInstanceKey GameModel w schema err params where
        WalletKey :: Wallet -> ContractInstanceKey GameModel () GameStateMachineSchema GameError ()

    -- The commands available to a test case
    data Action GameModel = Lock      Wallet String Integer
                          | Guess     Wallet String String Integer
                          | GiveToken Wallet
        deriving (Eq, Show, Data)

    initialState = GameModel
        { _gameValue     = 0
        , _hasToken      = Nothing
        , _currentSecret = ""
        }

    initialInstances = (`StartContract` ()) . WalletKey <$> wallets

    instanceWallet (WalletKey w) = w

    instanceContract _ WalletKey{} _ = G.contract

    -- 'perform' gets a state, which includes the GameModel state, but also contract handles for the
    -- wallets and what the model thinks the current balances are.
    perform handle _ s cmd = case cmd of
        Lock w new val -> do
            Trace.callEndpoint @"lock" (handle $ WalletKey w)
                LockArgs { lockArgsGameParam = gameParam
                         , lockArgsSecret = secretArg new
                         , lockArgsValue = Ada.lovelaceValueOf val
                         }
            delay 2
        Guess w old new val -> do
            Trace.callEndpoint @"guess" (handle $ WalletKey w)
                GuessArgs { guessArgsGameParam = gameParam
                          , guessArgsOldSecret = old
                          , guessArgsNewSecret = secretArg new
                          , guessArgsValueTakenOut = Ada.lovelaceValueOf val
                          }
            delay 1
        GiveToken w' -> do
            let w = fromJust (s ^. contractState . hasToken)
            _ <- Trace.payToWallet w w' guessTokenVal
            delay 1

    -- 'nextState' descibes how each command affects the state of the model

    nextState (Lock w secret val) = do
        hasToken      .= Just w
        currentSecret .= secret
        gameValue     .= val
        mint guessTokenVal
        deposit w guessTokenVal
        withdraw w $ Ada.lovelaceValueOf val
        wait 2

    nextState (Guess w old new val) = do
        correct <- (old ==) <$> viewContractState currentSecret
        when correct $ do
            currentSecret .= new
            gameValue     %= subtract val
            deposit w $ Ada.lovelaceValueOf val
        wait 1

    nextState (GiveToken w) = do
        w0 <- fromJust <$> viewContractState hasToken
        transfer w0 w guessTokenVal
        hasToken .= Just w
        wait 1

    -- To generate a random test case we need to know how to generate a random
    -- command given the current model state.
    arbitraryAction s = oneof $
        [ genLockAction | Nothing <- [tok] ] ++
        [ Guess w   <$> genGuess  <*> genGuess <*> genGuessAmount
          | val > minOut, Just w <- [tok] ] ++
        [ GiveToken <$> genWallet | isJust tok ]
        where
            genGuessAmount = frequency $ [(1, pure val)] ++
                                         [(1, pure $ minOut)               | 2*minOut <= val] ++
                                         [(8, choose (minOut, val-minOut)) | minOut <= val-minOut]
            minOut = Ada.getLovelace Ledger.minAdaTxOut
            tok = s ^. contractState . hasToken
            val = s ^. contractState . gameValue
            genLockAction :: Gen (Action GameModel)
            genLockAction = do
              w <- genWallet
              pure (Lock w) <*> genGuess <*> choose (Ada.getLovelace Ledger.minAdaTxOut, Ada.getLovelace (Ada.adaOf 100))

    -- The 'precondition' says when a particular command is allowed.
    precondition s cmd = case cmd of
            -- In order to lock funds, we need to satifsy the constraint where
            -- each tx out must have at least N Ada.
            Lock _ _ v    -> v >= Ada.getLovelace Ledger.minAdaTxOut
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
  available (Lock w _ _) alive    = (Key $ WalletKey w) `elem` alive
  available (Guess w _ _ _) alive = (Key $ WalletKey w) `elem` alive
  available _ _                   = True

  restartArguments _ WalletKey{} = ()

-- | The main property. 'propRunActions_' checks that balances match the model after each test.
prop_Game :: Actions GameModel -> Property
prop_Game = propRunActionsWithOptions options defaultCoverageOptions (\ _ -> pure True)

prop_GameWhitelist :: Actions GameModel -> Property
prop_GameWhitelist = checkErrorWhitelist defaultWhitelist

prop_SanityCheckModel :: Property
prop_SanityCheckModel = propSanityCheckModel @GameModel

prop_SanityCheckAssertions :: Actions GameModel -> Property
prop_SanityCheckAssertions = propSanityCheckAssertions

check_prop_Game_with_coverage :: IO ()
check_prop_Game_with_coverage = do
  cr <- quickCheckWithCoverage stdArgs (set coverageIndex covIndex defaultCoverageOptions) $ \covopts ->
    propRunActionsWithOptions @GameModel defaultCheckOptionsContractModel
                                         covopts
                                         (const (pure True))
  writeCoverageReport "GameStateMachine" covIndex cr

covIndex :: CoverageIndex
covIndex = covIdx gameParam

propGame' :: LogLevel -> Actions GameModel -> Property
propGame' l = propRunActionsWithOptions
                (set minLogLevel l defaultCheckOptionsContractModel)
                defaultCoverageOptions
                (\ _ -> pure True)

prop_GameCrashTolerance :: Actions (WithCrashTolerance GameModel) -> Property
prop_GameCrashTolerance = propRunActionsWithOptions options defaultCoverageOptions (\ _ -> pure True)

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

-- Dynamic Logic ----------------------------------------------------------

prop_UnitTest :: Property
prop_UnitTest = withMaxSuccess 1 $ forAllDL unitTest1 prop_Game

unitTest1 :: DL GameModel ()
unitTest1 = do
    val <- forAllQ $ chooseQ (5_000_000, 20_000_000)
    action $ Lock w1 "hello" val
    action $ GiveToken w2
    action $ Guess w2 "hello" "new secret" 3_000_000

unitTest2 :: DL GameModel ()
unitTest2 = do
    unitTest1
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
    when (val >= Ada.getLovelace Ledger.minAdaTxOut) $ do
        monitor $ label "Unlocking funds"
        action $ GiveToken w
        action $ Guess w secret "" val
    assertModel "Locked funds should be zero" $ symIsZero . lockedValue

-- | Check that we can always get the money out of the guessing game (by guessing correctly).
prop_NoLockedFunds :: Property
prop_NoLockedFunds = forAllDL noLockedFunds prop_Game

noLockProof :: NoLockedFundsProof GameModel
noLockProof = defaultNLFP {
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
prop_CheckNoLockedFundsProof = checkNoLockedFundsProofWithOptions options noLockProof

-- * Unit tests

tests :: TestTree
tests =
    testGroup "game state machine with secret arguments tests"
    [ checkPredicateOptions options "run a successful game trace"
        (walletFundsChange w2 (Ada.adaValueOf 3 <> guessTokenVal)
        .&&. valueAtAddress (Scripts.validatorAddress $ G.typedValidator gameParam) (Ada.adaValueOf 5 ==)
        .&&. walletFundsChange w1 (Ada.adaValueOf (-8)))
        successTrace

    , checkPredicateOptions options "run a 2nd successful game trace"
        (walletFundsChange w2 (Ada.adaValueOf 3)
        .&&. valueAtAddress (Scripts.validatorAddress $ G.typedValidator gameParam) (Ada.adaValueOf 0 ==)
        .&&. walletFundsChange w1 (Ada.adaValueOf (-8))
        .&&. walletFundsChange w3 (Ada.adaValueOf 5 <> guessTokenVal))
        successTrace2

    , checkPredicateOptions options "run a successful game trace where we try to leave 1 Ada in the script address"
        (walletFundsChange w1 (Ada.toValue (-2_000_000) <> guessTokenVal)
        .&&. valueAtAddress (Scripts.validatorAddress $ G.typedValidator gameParam) (Ada.toValue 2_000_000 ==))
        traceLeaveTwoAdaInScript

    , checkPredicateOptions options "run a failed trace"
        (walletFundsChange w2 (Ada.toValue 2_000_000 <> guessTokenVal)
        .&&. valueAtAddress (Scripts.validatorAddress $ G.typedValidator gameParam) (Ada.adaValueOf 8 ==)
        .&&. walletFundsChange w1 (Ada.toValue (-2_000_000) <> Ada.adaValueOf (-8)))
        failTrace

    , goldenPir "test/Spec/gameStateMachine.pir" $$(PlutusTx.compile [|| mkValidator ||])

    , HUnit.testCaseSteps "script size is reasonable" $ \step ->
        reasonable' step (Scripts.validatorScript $ G.typedValidator gameParam) 49000

    , testProperty "can always get the funds out" $
        withMaxSuccess 10 prop_NoLockedFunds

    , testProperty "sanity check the contract model" prop_SanityCheckModel

    , testProperty "game state machine crash tolerance" $ withMaxSuccess 20 prop_GameCrashTolerance
    ]

initialVal :: Value
initialVal = Ada.adaValueOf 10

runTestsWithCoverage :: IO ()
runTestsWithCoverage = do
  ref <- newCoverageRef
  defaultMain (coverageTests ref)
    `catch` \(e :: SomeException) -> do
                report <- readCoverageRef ref
                putStrLn . show $ pprCoverageReport (covIdx gameParam) report
                throwIO e
  where
    coverageTests ref = testGroup "game state machine tests"
                         [ checkPredicateCoverage "run a successful game trace"
                            ref
                            (walletFundsChange w2 (Ada.toValue Ledger.minAdaTxOut <> Ada.adaValueOf 3 <> guessTokenVal)
                            .&&. valueAtAddress (Scripts.validatorAddress $ G.typedValidator gameParam) (Ada.adaValueOf 5 ==)
                            .&&. walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> Ada.adaValueOf (-8)))
                            successTrace

                        , checkPredicateCoverage "run a 2nd successful game trace"
                            ref
                            (walletFundsChange w2 (Ada.adaValueOf 3)
                            .&&. valueAtAddress (Scripts.validatorAddress $ G.typedValidator gameParam) (Ada.adaValueOf 0 ==)
                            .&&. walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> Ada.adaValueOf (-8))
                            .&&. walletFundsChange w3 (Ada.toValue Ledger.minAdaTxOut <> Ada.adaValueOf 5 <> guessTokenVal))
                            successTrace2
                        ]

-- | Wallet 1 locks some funds, transfers the token to wallet 2
--   which then makes a correct guess and locks the remaining
--   funds with a new secret
successTrace :: EmulatorTrace ()
successTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs { lockArgsGameParam = gameParam
                                            , lockArgsSecret = secretArg "hello"
                                            , lockArgsValue = Ada.adaValueOf 8
                                            }
    -- One slot for sending the Ada to the script and one slot for minting the
    -- guess token
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 guessTokenVal
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    Trace.callEndpoint @"guess" hdl2 GuessArgs { guessArgsGameParam = gameParam
                                               , guessArgsOldSecret = "hello"
                                               , guessArgsNewSecret = secretArg "new secret"
                                               , guessArgsValueTakenOut = Ada.adaValueOf 3
                                               }
    void $ Trace.waitNSlots 1

-- | Run 'successTrace', then wallet 2 transfers the token to wallet 3, which
--   makes another correct guess
successTrace2 :: EmulatorTrace ()
successTrace2 = do
    successTrace
    _ <- Trace.payToWallet w2 w3 guessTokenVal
    _ <- Trace.waitNSlots 1
    hdl3 <- Trace.activateContractWallet w3 G.contract
    Trace.callEndpoint @"guess" hdl3 GuessArgs { guessArgsGameParam = gameParam
                                               , guessArgsOldSecret = "new secret"
                                               , guessArgsNewSecret = secretArg "hello"
                                               , guessArgsValueTakenOut = Ada.adaValueOf 5
                                               }
    void $ Trace.waitNSlots 1

-- | Tests whether the contract correctly handles the case where we leave less
-- than 3 Ada in the script address after guessing.
traceLeaveTwoAdaInScript :: EmulatorTrace ()
traceLeaveTwoAdaInScript = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs { lockArgsGameParam = gameParam
                                            , lockArgsSecret = secretArg "hello"
                                            , lockArgsValue = Ada.adaValueOf 9
                                            }
    _ <- Trace.waitNSlots 2
    _ <- Trace.callEndpoint @"guess" hdl GuessArgs { guessArgsGameParam = gameParam
                                                   , guessArgsOldSecret = "hello"
                                                   , guessArgsNewSecret = secretArg "new secret"
                                                   , guessArgsValueTakenOut = Ada.adaValueOf 7
                                                   }
    void $ Trace.waitNSlots 1

-- | Wallet 1 locks some funds, transfers the token to wallet 2
--   which then makes a wrong guess
failTrace :: EmulatorTrace ()
failTrace = do
    hdl <- Trace.activateContractWallet w1 G.contract
    Trace.callEndpoint @"lock" hdl LockArgs { lockArgsGameParam = gameParam
                                            , lockArgsSecret = secretArg "hello"
                                            , lockArgsValue = Ada.adaValueOf 8
                                            }
    _ <- Trace.waitNSlots 2
    _ <- Trace.payToWallet w1 w2 (Ada.toValue 2_000_000 <> guessTokenVal)
    _ <- Trace.waitNSlots 1
    hdl2 <- Trace.activateContractWallet w2 G.contract
    _ <- Trace.callEndpoint @"guess" hdl2 GuessArgs { guessArgsGameParam = gameParam
                                                    , guessArgsOldSecret = "hola"
                                                    , guessArgsNewSecret = secretArg "new secret"
                                                    , guessArgsValueTakenOut = Ada.adaValueOf 3
                                                    }
    void $ Trace.waitNSlots 1

guessTokenVal :: Value
guessTokenVal =
    let sym = Scripts.forwardingMintingPolicyHash $ G.typedValidator gameParam
    in G.token sym "guess"

-- | Certification.
certification :: Certification GameModel
certification = defaultCertification {
    certNoLockedFunds      = Just noLockProof,
    certUnitTests          = Just unitTest,
    certCoverageIndex      = covIdx gameParam,
    certCrashTolerance     = Just Instance
  }
  where
    unitTest ref =
      checkPredicateCoverage "run a successful game trace" ref
        (walletFundsChange w2 (Ada.toValue Ledger.minAdaTxOut <> Ada.adaValueOf 3 <> guessTokenVal)
        .&&. valueAtAddress (Scripts.validatorAddress $ G.typedValidator gameParam) (Ada.adaValueOf 5 ==)
        .&&. walletFundsChange w1 (Ada.toValue (-Ledger.minAdaTxOut) <> Ada.adaValueOf (-8)))
        successTrace
