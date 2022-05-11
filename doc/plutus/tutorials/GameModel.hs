{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -Wno-name-shadowing -Wno-unused-matches -Wno-unused-do-bind -Wno-unused-local-binds #-}

module GameModel where

import Control.Applicative ((<|>))
import Control.Lens (makeLenses, set, (^.))
import Control.Monad (when)
import Data.Data (Data)
import System.Random (Random)

-- START import Log
import Control.Monad.Freer.Extras.Log (LogLevel)
-- END import Log

import Data.Maybe (fromJust, isJust, isNothing)
import Test.QuickCheck (Arbitrary, Gen, Property, arbitrary, choose, elements, frequency, getNonNegative, label, oneof,
                        shrink, tabulate, withMaxSuccess)

-- START import Contract.Test
import Plutus.Contract.Test (Wallet, minLogLevel, mockWalletPaymentPubKeyHash, w1, w2, w3)
-- END import Contract.Test

-- START import ContractModel
import Plutus.Contract.Test.ContractModel qualified as CM
-- END import ContractModel

-- END import ContractModel
import Test.QuickCheck.StateModel (Var (Var))

-- START import Game
import Plutus.Contracts.GameStateMachine qualified as G
-- END import Game

-- START import Ada
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value
-- END import Ada

-- START import Scripts
import Ledger.Typed.Scripts qualified as Scripts
-- END import Scripts

-- START import Emulator
import Plutus.Trace.Emulator qualified as Trace
-- END import Emulator

-- START import Contract.Security
import Plutus.Contract.Secrets (secretArg)
-- END import Contract.Security

-- START import TimeSlot
import Ledger.TimeSlot qualified as TimeSlot
-- END import TimeSlot

-- START import Data.Default
import Data.Default (Default (def))
-- END import Data.Default

-- START gameParam
gameParam :: G.GameParam
gameParam = G.GameParam (mockWalletPaymentPubKeyHash w1) (TimeSlot.scSlotZeroTime def)
-- END gameParam

-- * QuickCheck model

-- START GameModel
data GameModel = GameModel
    { _gameValue     :: Integer
    , _hasToken      :: Maybe Wallet
    , _currentSecret :: String }
  deriving (Show, Data)

makeLenses 'GameModel
-- END GameModel

deriving instance Eq (CM.ContractInstanceKey GameModel w schema err param)
deriving instance Show (CM.ContractInstanceKey GameModel w schema err param)

-- START instance ContractModel and Action type
instance CM.ContractModel GameModel where

    data Action GameModel = Lock      Wallet String Integer
                          | Guess     Wallet String String Integer
                          | GiveToken Wallet
        deriving (Eq, Show, Data)
-- END instance ContractModel and Action type

-- START ContractInstanceKey
    data ContractInstanceKey GameModel w schema err param where
        WalletKey :: Wallet -> CM.ContractInstanceKey GameModel () G.GameStateMachineSchema G.GameError ()
-- END ContractInstanceKey

-- START initialState
    initialState = GameModel
        { _gameValue     = 0
        , _hasToken      = Nothing
        , _currentSecret = ""
        }
-- END initialState

-- START initialHandleSpecs
    initialInstances = (`CM.StartContract` ()) . WalletKey <$> wallets

    instanceContract _ WalletKey{} _ = G.contract

    instanceWallet (WalletKey w) = w
-- END initialHandleSpecs

-- START perform
    perform handle _ s cmd = case cmd of
        Lock w new val -> do
            Trace.callEndpoint @"lock" (handle $ WalletKey w)
                G.LockArgs
                    { G.lockArgsGameParam = gameParam
                    , G.lockArgsSecret    = secretArg new
                    , G.lockArgsValue     = Ada.lovelaceValueOf val
                    }
            CM.delay 2
        Guess w old new val -> do
            Trace.callEndpoint @"guess" (handle $ WalletKey w)
                G.GuessArgs
                    { G.guessArgsGameParam     = gameParam
                    , G.guessArgsOldSecret     = old
                    , G.guessArgsNewSecret     = secretArg new
                    , G.guessArgsValueTakenOut = Ada.lovelaceValueOf val
                    }
            CM.delay 1
        GiveToken w' -> do
            let w = fromJust (s ^. CM.contractState . hasToken)
            Trace.payToWallet w w' guessTokenVal
            CM.delay 1
-- END perform

    -- 'nextState' descibes how each command affects the state of the model
    nextState (Lock w secret val) = do
        wasUnlocked <- (Nothing ==) <$> CM.viewContractState hasToken
        when wasUnlocked $ do
          hasToken      CM.$= Just w
          currentSecret CM.$= secret
          gameValue     CM.$= val
          CM.mint guessTokenVal
          CM.deposit  w guessTokenVal
        CM.withdraw w $ Ada.lovelaceValueOf val
        CM.wait 2

-- START nextState Guess
    nextState (Guess w old new val) = do
        correctGuess <- (old ==)    <$> CM.viewContractState currentSecret
        holdsToken   <- (Just w ==) <$> CM.viewContractState hasToken
        enoughAda    <- (val <=)    <$> CM.viewContractState gameValue
        when (correctGuess && holdsToken && enoughAda) $ do
            currentSecret CM.$= new
            gameValue     CM.$~ subtract val
            CM.deposit w $ Ada.lovelaceValueOf val
        CM.wait 1
-- END nextState Guess

    nextState (GiveToken w) = do
        w0 <- fromJust <$> CM.viewContractState hasToken
        CM.transfer w0 w guessTokenVal
        hasToken CM.$= Just w
        CM.wait 1

-- START arbitraryAction
    arbitraryAction s = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue ] ++
        [ frequency $
          [ (10, Guess w   <$> genGuess  <*> genGuess <*> choose (0, val))
          | Just w <- [tok] ] ++
          [ (1, Guess <$> genWallet <*> genGuess <*> genGuess <*> genValue) ] ] ++
        [ GiveToken <$> genWallet ]
        where
            tok = s ^. CM.contractState . hasToken
            val = s ^. CM.contractState . gameValue
-- END arbitraryAction

-- START precondition
    precondition s cmd = case cmd of
            Lock _ _ v    -> isNothing tok
            Guess w _ _ v -> tok == Just w && v <= val
            GiveToken w   -> isJust tok
        where
            tok = s ^. CM.contractState . hasToken
            val = s ^. CM.contractState . gameValue
-- END precondition

-- START shrinkAction
    shrinkAction _s (Lock w secret val) =
        [Lock w' secret val | w' <- shrinkWallet w] ++
        [Lock w secret val' | val' <- shrink val]
    shrinkAction _s (GiveToken w) =
        [GiveToken w' | w' <- shrinkWallet w]
    shrinkAction _s (Guess w old new val) =
        [Guess w' old new val | w' <- shrinkWallet w] ++
        [Guess w old new val' | val' <- shrink val]
-- END shrinkAction

-- START monitoring
    monitoring (s, _) (Guess w guess new v) =
        tabulate "Guesses" [if guess == secret then "Right" else "Wrong"]
        where secret = s ^. CM.contractState . currentSecret
    monitoring _ _ = id
-- END monitoring

-- START prop_Game
prop_Game :: CM.Actions GameModel -> Property
prop_Game actions = CM.propRunActions_ actions
-- END prop_Game

-- START propGame'
propGame' :: LogLevel -> CM.Actions GameModel -> Property
propGame' l s = CM.propRunActionsWithOptions
                    (set minLogLevel l CM.defaultCheckOptionsContractModel)
                    CM.defaultCoverageOptions
                    (\ _ -> pure True)
                    s
-- END propGame'

-- START Generators
genWallet :: Gen Wallet
genWallet = elements wallets

genGuess :: Gen String
genGuess = elements ["hello", "secret", "hunter2", "*******"]

genValue :: Gen Integer
genValue = getNonNegative <$> arbitrary
-- END Generators

-- START shrinkWallet
shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]
-- END shrinkWallet

guesses :: [String]
guesses = ["hello", "secret", "hunter2", "*******"]

-- Dynamic Logic ----------------------------------------------------------

prop_UnitTest :: Property
prop_UnitTest = withMaxSuccess 1 $ CM.forAllDL unitTest prop_Game

-- START propDL
propDL :: CM.DL GameModel () -> Property
propDL dl = CM.forAllDL dl prop_Game
-- END propDL

-- START unitTest
unitTest :: CM.DL GameModel ()
unitTest = do
    val <- CM.forAllQ $ CM.chooseQ (3, 20)
    CM.action $ Lock w1 "hello" val
    CM.action $ GiveToken w2
    CM.action $ Guess w2 "hello" "new secret" 3
-- END unitTest

-- START badUnitTest
badUnitTest :: CM.DLTest GameModel
badUnitTest =
    CM.BadPrecondition
      [CM.Witness (1 :: Integer),
       CM.Do $ CM.NoBind (Var 1) $ Lock w1 "hello" 1,
       CM.Do $ CM.NoBind (Var 2) $ GiveToken w2]
      [CM.Action (CM.NoBind (Var 3) (Guess w2 "hello" "new secret" 3))]
      (GameModel {_gameValue = 1, _hasToken = Just w2, _currentSecret = "hello"})
-- END badUnitTest

unitTest2 :: CM.DL GameModel ()
unitTest2 = do
    unitTest
    CM.action $ GiveToken w3
    CM.action $ Guess w3 "new secret" "hello" 4

unitTestFail :: CM.DL GameModel ()
unitTestFail = do
    CM.action $ Lock w1 "hello" 8
    CM.action $ GiveToken w2
    CM.action $ Guess w2 "hola" "new secret" 3

-- START noLockedFunds
noLockedFunds :: CM.DL GameModel ()
noLockedFunds = do
    (w0, funds, pass) <- CM.forAllQ (CM.elementsQ wallets, CM.chooseQ (1, 10000), CM.elementsQ guesses)
    CM.action $ Lock w0 pass funds
    CM.anyActions_
    w      <- CM.forAllQ $ CM.elementsQ wallets
    secret <- CM.viewContractState currentSecret
    val    <- CM.viewContractState gameValue
    when (val > 0) $ do
        CM.monitor $ label "Unlocking funds"
        CM.action $ GiveToken w
        CM.action $ Guess w secret "" val
    CM.assertModel "Locked funds should be zero" $ CM.symIsZero . CM.lockedValue
-- END noLockedFunds

-- | Check that we can always get the money out of the guessing game (by guessing correctly).
prop_NoLockedFunds :: Property
prop_NoLockedFunds = CM.forAllDL noLockedFunds prop_Game

-- | Wallets and game token.

-- START wallets
wallets :: [Wallet]
wallets = [w1, w2, w3]
-- END wallets

-- START guessTokenVal
guessTokenVal :: Value.Value
guessTokenVal =
    let sym = Scripts.forwardingMintingPolicyHash $ G.typedValidator gameParam
    in G.token sym "guess"
-- END guessTokenVal

-- START testLock v1
testLock :: Property
testLock = prop_Game $ CM.actionsFromList [Lock w1 "hunter2" 0]
-- END testLock v1

testLockWithMaxSuccess :: ()
testLockWithMaxSuccess = ()
 where
-- START testLock withMaxSuccess
 testLock :: Property
 testLock = withMaxSuccess 1 . prop_Game $ CM.actionsFromList [Lock w1 "hunter2" 0]
-- END testLock withMaxSuccess

-- START testDoubleLock
testDoubleLock :: Property
testDoubleLock = prop_Game $
  CM.actionsFromList
    [Lock w1 "*******" 0,
     Lock w1 "secret" 0]
-- END testDoubleLock

anyActionsDef :: ()
anyActionsDef = ()
 where
-- START anyActions
 anyActions :: Int -> CM.DL s ()
 anyActions n = CM.stopping
            <|> CM.weight (1 / fromIntegral n)
            <|> (CM.anyAction >> anyActions n)
-- END anyActions

-- Code for preliminary versions

v1_model :: ()
v1_model = ()
  where
    arbitraryAction :: CM.ModelState GameModel -> Gen (CM.Action GameModel)
-- START arbitraryAction v1
    arbitraryAction s = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue              ] ++
        [ Guess     <$> genWallet <*> genGuess <*> genGuess <*> genValue ] ++
        [ GiveToken <$> genWallet                                        ]
-- END arbitraryAction v1

    nextState :: CM.Action GameModel -> CM.Spec GameModel ()
-- START nextState Lock v1
    nextState (Lock w secret val) = do
        hasToken      CM.$= Just w
        currentSecret CM.$= secret
        gameValue     CM.$= val
        CM.mint guessTokenVal
        CM.deposit  w guessTokenVal
        CM.withdraw w $ Ada.lovelaceValueOf val
-- END nextState Lock v1
-- START nextState Guess v1
    nextState (Guess w old new val) = do
        correctGuess <- (old ==)    <$> CM.viewContractState currentSecret
        holdsToken   <- (Just w ==) <$> CM.viewContractState hasToken
        enoughAda    <- (val <=)    <$> CM.viewContractState gameValue
        when (correctGuess && holdsToken && enoughAda) $ do
            currentSecret CM.$= new
            gameValue     CM.$~ subtract val
            CM.deposit w $ Ada.lovelaceValueOf val
-- END nextState Guess v1
-- START nextState GiveToken v1
    nextState (GiveToken w) = do
        w0 <- fromJust <$> CM.viewContractState hasToken
        CM.transfer w0 w guessTokenVal
        hasToken CM.$= Just w
-- END nextState GiveToken v1

    precondition :: CM.ModelState GameModel -> CM.Action GameModel -> Bool
-- START precondition v1
    precondition s (GiveToken _) = isJust tok
        where
            tok = s ^. CM.contractState . hasToken
    precondition s _             = True
-- END precondition v1

    perform
        :: CM.HandleFun GameModel
        -> (CM.SymToken -> Value.AssetClass)
        -> CM.ModelState GameModel
        -> CM.Action GameModel
        -> CM.SpecificationEmulatorTrace ()
-- START perform v1
    perform handle _ s cmd = case cmd of
        Lock w new val -> do
            Trace.callEndpoint @"lock" (handle $ WalletKey w)
                G.LockArgs
                    { G.lockArgsGameParam = gameParam
                    , G.lockArgsSecret = secretArg new
                    , G.lockArgsValue = Ada.lovelaceValueOf val
                    }
        Guess w old new val -> do
            Trace.callEndpoint @"guess" (handle $ WalletKey w)
                G.GuessArgs
                    { G.guessArgsGameParam = gameParam
                    , G.guessArgsOldSecret = old
                    , G.guessArgsNewSecret = secretArg new
                    , G.guessArgsValueTakenOut = Ada.lovelaceValueOf val
                    }
        GiveToken w' -> do
            let w = fromJust (s ^. CM.contractState . hasToken)
            Trace.payToWallet w w' guessTokenVal
            return ()
-- END perform v1

v2_model :: ()
v2_model = ()
  where
    nextState :: CM.Action GameModel -> CM.Spec GameModel ()
-- START nextState Lock v2
    nextState (Lock w secret val) = do
        hasToken      CM.$= Just w
        currentSecret CM.$= secret
        gameValue     CM.$= val
        CM.mint guessTokenVal
        CM.deposit  w guessTokenVal
        CM.withdraw w $ Ada.lovelaceValueOf val
        CM.wait 2
-- END nextState Lock v2
-- START nextState Guess partial
    nextState (Guess w old new val) = do
        correctGuess <- (old ==)    <$> CM.viewContractState currentSecret
        -- ...
-- END nextState Guess partial
        return ()
    nextState _ = return ()

    precondition :: CM.ModelState GameModel -> CM.Action GameModel -> Bool
-- START precondition v2
    precondition s cmd = case cmd of
            Lock {}     -> isNothing tok
            Guess {}    -> True
            GiveToken _ -> isJust tok
        where
            tok = s ^. CM.contractState . hasToken
-- END precondition v2

    arbitraryAction :: CM.ModelState GameModel -> Gen (CM.Action GameModel)
-- START arbitaryAction v2
    arbitraryAction s = oneof $
        [ Lock      <$> genWallet <*> genGuess <*> genValue ] ++
        [ Guess w   <$> genGuess  <*> genGuess <*> choose (0, val)
        | Just w <- [tok] ] ++
        [ GiveToken <$> genWallet ]
        where
            tok = s ^. CM.contractState . hasToken
            val = s ^. CM.contractState . gameValue
-- END arbitaryAction v2

v3_model :: ()
v3_model = ()
  where
    precondition :: CM.ModelState GameModel -> CM.Action GameModel -> Bool
-- START precondition v3
    precondition s cmd = case cmd of
            Lock {}       -> isNothing tok
            Guess w _ _ _ -> tok == Just w
            GiveToken _   -> isJust tok
        where
            tok = s ^. CM.contractState . hasToken
-- END precondition v3

unitTest_v1 :: ()
unitTest_v1 = ()
 where
-- START unitTest v1
 unitTest :: CM.DL GameModel ()
 unitTest = do
     CM.action $ Lock w1 "hello" 10
     CM.action $ GiveToken w2
     CM.action $ Guess w2 "hello" "new secret" 3
-- END unitTest v1

unitTest_v2 :: ()
unitTest_v2 = ()
 where
-- START unitTest v2
 unitTest :: CM.DL GameModel ()
 unitTest = do
     val <- CM.forAllQ $ CM.chooseQ (1, 20)
     CM.action $ Lock w1 "hello" val
     CM.action $ GiveToken w2
     CM.action $ Guess w2 "hello" "new secret" 3
-- END unitTest v2

noLockedFunds_v1 :: ()
noLockedFunds_v1 = ()
 where
-- START noLockedFunds v1
 noLockedFunds :: CM.DL GameModel ()
 noLockedFunds = do
     CM.anyActions_
     CM.assertModel "Locked funds should be zero" $ CM.symIsZero . CM.lockedValue
-- END noLockedFunds v1

noLockedFunds_v2 :: ()
noLockedFunds_v2 = ()
 where
-- START noLockedFunds v2
 noLockedFunds :: CM.DL GameModel ()
 noLockedFunds = do
     CM.anyActions_
     w      <- CM.forAllQ $ CM.elementsQ wallets
     secret <- CM.viewContractState currentSecret
     val    <- CM.viewContractState gameValue
     CM.action $ Guess w "" secret val
     CM.assertModel "Locked funds should be zero" $ CM.symIsZero . CM.lockedValue
-- END noLockedFunds v2

noLockedFunds_v3 :: ()
noLockedFunds_v3 = ()
 where
-- START noLockedFunds v3
 noLockedFunds :: CM.DL GameModel ()
 noLockedFunds = do
     CM.anyActions_
     w      <- CM.forAllQ $ CM.elementsQ wallets
     secret <- CM.viewContractState currentSecret
     val    <- CM.viewContractState gameValue
     when (val > 0) $ do
         CM.action $ Guess w "" secret val
     CM.assertModel "Locked funds should be zero" $ CM.symIsZero . CM.lockedValue
-- END noLockedFunds v3

noLockedFunds_v4 :: ()
noLockedFunds_v4 = ()
 where
-- START noLockedFunds v4
 noLockedFunds :: CM.DL GameModel ()
 noLockedFunds = do
     CM.anyActions_
     w      <- CM.forAllQ $ CM.elementsQ wallets
     secret <- CM.viewContractState currentSecret
     val    <- CM.viewContractState gameValue
     when (val > 0) $ do
         CM.action $ GiveToken w
         CM.action $ Guess w "" secret val
     CM.assertModel "Locked funds should be zero" $ CM.symIsZero . CM.lockedValue
-- END noLockedFunds v4

noLockedFunds_v5 :: ()
noLockedFunds_v5 = ()
 where
-- START noLockedFunds v5
 noLockedFunds :: CM.DL GameModel ()
 noLockedFunds = do
     CM.anyActions_
     w      <- CM.forAllQ $ CM.elementsQ wallets
     secret <- CM.viewContractState currentSecret
     val    <- CM.viewContractState gameValue
     when (val > 0) $ do
         CM.monitor $ label "Unlocking funds"
         CM.action $ GiveToken w
         CM.action $ Guess w secret "" val
     CM.assertModel "Locked funds should be zero" $ CM.symIsZero . CM.lockedValue
-- END noLockedFunds v5

typeSignatures :: forall state. CM.ContractModel state => state -> state
typeSignatures = id
 where
-- START nextState type
 nextState :: CM.Action state -> CM.Spec state ()
-- END nextState type
 nextState = CM.nextState
-- START precondition type
 precondition :: CM.ModelState state -> CM.Action state -> Bool
-- END precondition type
 precondition = CM.precondition
-- START perform type
 perform
     :: CM.HandleFun state
     -> (CM.SymToken -> Value.AssetClass)
     -> CM.ModelState state
     -> CM.Action state
     -> CM.SpecificationEmulatorTrace ()
-- END perform type
 perform = CM.perform
-- START shrinkAction type
 shrinkAction :: CM.ModelState state -> CM.Action state -> [CM.Action state]
-- END shrinkAction type
 shrinkAction = CM.shrinkAction
-- START monitoring type
 monitoring :: (CM.ModelState state, CM.ModelState state) -> CM.Action state -> Property -> Property
-- END monitoring type
 monitoring = CM.monitoring
-- START chooseQ type
 chooseQ :: (Arbitrary a, Random a, Ord a) => (a, a) -> CM.Quantification a
-- END chooseQ type
 chooseQ = CM.chooseQ
