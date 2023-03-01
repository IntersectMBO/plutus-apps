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
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS_GHC -Wno-redundant-constraints -fno-warn-name-shadowing -Wno-orphans #-}

module Plutus.Contract.Test.ContractModel.Internal
  ( module Internal
  , module Plutus.Contract.Test.ContractModel.Internal
  ) where

import Cardano.Node.Emulator.Chain
import Cardano.Node.Emulator.Params
import Control.DeepSeq
import Control.Monad.Freer.Reader (Reader, ask, runReader)
import Control.Monad.Freer.State (State, get, modify, runState)
import Control.Monad.Writer as Writer (WriterT (..), runWriterT)
import Ledger.Blockchain
import Ledger.Tx
import Plutus.Trace.Effects.Waiting (Waiting)
import Plutus.Trace.Emulator (initialChainState, waitUntilSlot)
import Plutus.Trace.Emulator.Types (ContractHandle (..), ContractInstanceTag, UserThreadMsg (..))

import Control.Lens
import Control.Monad.Cont
import Control.Monad.Freer (Eff, Member, raise)
import Data.Data
import Data.IORef
import Data.List as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics

import Cardano.Api (AssetId, SlotNo (..))
import Cardano.Api qualified as CardanoAPI
import Cardano.Api.Shelley (ProtocolParameters)
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Node.Emulator.Params ()
import Ledger.Address
import Ledger.Index as Index
import Ledger.Scripts
import Ledger.Slot
import Plutus.Contract (ContractInstanceId)
import Plutus.Contract.Test hiding (not)
import Plutus.Trace.Effects.EmulatorControl (discardWallets)
import Plutus.Trace.Emulator as Trace (BaseEmulatorEffects, EmulatorEffects, EmulatorTrace, activateContract,
                                       freezeContractInstance, waitNSlots)
import Plutus.V1.Ledger.Crypto
import PlutusTx.Builtins qualified as Builtins
import PlutusTx.Coverage hiding (_coverageIndex)
import PlutusTx.ErrorCodes
import Test.QuickCheck.DynamicLogic qualified as DL
import Test.QuickCheck.StateModel (Realized)
import Test.QuickCheck.StateModel qualified as StateModel

import Test.QuickCheck hiding (ShrinkState, checkCoverage, getSize, (.&&.), (.||.))
import Test.QuickCheck qualified as QC
import Test.QuickCheck.ContractModel as CM
import Test.QuickCheck.ContractModel.Internal (ContractModelResult)
import Test.QuickCheck.ContractModel.Internal.Model (annotatedStateAfter)
import Test.QuickCheck.ContractModel.ThreatModel (ThreatModel, assertThreatModel)
import Test.QuickCheck.Monadic (PropertyM, monadic)
import Test.QuickCheck.Monadic qualified as QC

import Wallet.Emulator.MultiAgent (eteEvent)

import Plutus.Trace.Effects.EmulatorControl qualified as EmulatorControl
import Prettyprinter

import Ledger.Value.CardanoAPI (lovelaceValueOf)
import Plutus.Contract.Test.ContractModel.Internal.ContractInstance as Internal

-- Drops StartContract from EmulatorEffects
type SpecificationEmulatorTrace s =
  Eff ( Reader (Handles s)
     ': BaseEmulatorEffects
      )

type EmulatorTraceWithInstances s =
  Eff ( State (Handles s)
     ': EmulatorEffects
      )

type CheckableContractModel state =
  ( RunModel state (SpecificationEmulatorTrace state)
  , ContractInstanceModel state )

contractHandle :: (ContractInstanceModel state, Typeable w, Typeable schema, Typeable err, Typeable params)
       => ContractInstanceKey state w schema err params
       -> RunMonad (SpecificationEmulatorTrace state) (ContractHandle w schema err)
contractHandle key = do
  handles <- lift ask
  case imLookup key handles of
    Just (WalletContractHandle _ h) -> pure h
    Nothing                         -> error $ "contractHandle: No handle for " ++ show key

activateWallets :: ContractInstanceModel state
                => (SymToken -> AssetId)
                -> [StartContract state]
                -> EmulatorTraceWithInstances state ()
activateWallets _ [] = return ()
activateWallets sa (StartContract key params : starts) = do
    let wallet = instanceWallet key
    h <- activateContract wallet (instanceContract sa key params) (instanceTag key)
    modify $ IMCons key (WalletContractHandle wallet h)
    activateWallets sa starts

-- | Used to freeze other wallets when checking a `NoLockedFundsProof`.
instancesForOtherWallets :: Wallet -> Handles state -> [ContractInstanceId]
instancesForOtherWallets _ IMNil = []
instancesForOtherWallets w (IMCons _ (WalletContractHandle w' h) m)
  | w /= w'   = chInstanceId h : instancesForOtherWallets w m
  | otherwise = instancesForOtherWallets w m

newtype WithInstances s = WithInstances { withoutInstances :: s }
  deriving (Eq, Generic)
  deriving newtype (Show)

instance CheckableContractModel state =>
         RunModel (WithInstances state) (EmulatorTraceWithInstances state) where
  perform si (UnderlyingAction a) symEnv = do
    let s = withoutInstances <$> si
    lift $ activateWallets symEnv $ startInstances s a
    liftRunMonad liftSpecificationTrace $ perform @_ @(SpecificationEmulatorTrace state) s a symEnv
  perform _ (Unilateral w) _ = do
    h <- lift $ get @(Handles state)
    let insts = instancesForOtherWallets w h
    lift $ mapM_ freezeContractInstance insts
    lift $ discardWallets (w /=)
  monitoring (s, s') (UnderlyingAction a) = monitoring @_ @(SpecificationEmulatorTrace state)
                                                       (withoutInstances <$> s, withoutInstances <$> s') a
  monitoring _ Unilateral{} = \ _ _ -> id

liftSpecificationTrace :: SpecificationEmulatorTrace s a -> EmulatorTraceWithInstances s a
liftSpecificationTrace m = do
  s <- get
  raise . raise $ runReader s m

instance HasChainIndex (EmulatorTraceWithInstances state) where
  getChainIndex = do
    nid <- pNetworkId <$> EmulatorControl.getParams
    chainStateToChainIndex nid <$> EmulatorControl.chainState
    -- Note, we don't store the genesis transaction in the index but put it in the before state
    -- instead to avoid showing that as a balance change in the models.
    where chainStateToChainIndex nid cs =
            ChainIndex { -- The Backwards order
                         transactions = fst $ foldr addBlock ([], beforeState)
                                                             ( reverse
                                                             . drop 1
                                                             . reverse
                                                             . _chainNewestFirst
                                                             $ cs)
                       , networkId = nid
                       }
            where beforeState = CM.ChainState { slot = 0
                                              , utxo = makeUTxOs
                                                     $ Index.initialise (take 1 $ reverse (_chainNewestFirst cs))
                                              }
                  addBlock block (txs, state) =
                    ( txs ++ [ TxInState ((\(CardanoEmulatorEraTx tx) -> tx) . unOnChain $ tx)
                                         state
                                         (onChainTxIsValid tx)
                             | tx <- block ]
                    , updateState block state )

                  updateState :: [OnChainTx] -> CM.ChainState -> CM.ChainState
                  updateState block state =
                    CM.ChainState{ slot = slot state + 1
                                 , utxo = foldr addTx (utxo state) block
                                 }

                  addTx :: OnChainTx -> CardanoAPI.UTxO CM.Era -> CardanoAPI.UTxO CM.Era
                  addTx tx (CardanoAPI.UTxO utxos) =
                      CardanoAPI.UTxO $ outputs <> Map.withoutKeys utxos consumed
                    where
                      consumed :: Set CardanoAPI.TxIn
                      consumed = Set.fromList $ map mkTxIn $ consumableInputs tx

                      CardanoAPI.UTxO outputs = makeUTxOs $ UtxoIndex $ outputsProduced tx

          mkTxIn :: TxIn -> CardanoAPI.TxIn
          mkTxIn = mkRef . txInRef

          makeUTxOs :: UtxoIndex -> CardanoAPI.UTxO CM.Era
          makeUTxOs (UtxoIndex i) = CardanoAPI.UTxO $ Map.fromList [ (mkRef ref, mkTxOut utxo)
                                                                   | (ref, utxo) <- Map.toList i ]

          mkRef :: TxOutRef -> CardanoAPI.TxIn
          mkRef (TxOutRef (TxId bs) ix) = CardanoAPI.TxIn
                                            (CardanoAPI.TxId $ makeTheHash bs)
                                            (CardanoAPI.TxIx $ fromIntegral ix)

          mkTxOut :: TxOut -> CardanoAPI.TxOut CardanoAPI.CtxUTxO Era
          mkTxOut (TxOut o) = CardanoAPI.toCtxUTxOTxOut o

makeTheHash :: Crypto.HashAlgorithm crypto => Builtins.BuiltinByteString -> Crypto.Hash crypto stuff
makeTheHash bs =
  case Crypto.hashFromBytes $ Builtins.fromBuiltin bs of
    Nothing   -> error "Bad hash!"
    Just hash -> hash

-- | `delay n` delays emulator execution by `n` slots
delay :: Integer -> RunMonad (SpecificationEmulatorTrace state) ()
delay = lift . void . Trace.waitNSlots . fromInteger

instance Member Waiting effs => IsRunnable (Eff effs) where
  awaitSlot = void . waitUntilSlot . Slot . toInteger . unSlotNo

type instance Realized (Eff effs) a = a

deriving instance Eq (Action s) => Eq (Action (WithInstances s))

instance Show (Action s) => Show (Action (WithInstances s)) where
  showsPrec p (UnderlyingAction a) = showsPrec p a
  showsPrec p (Unilateral w)       = showParen (p > 10) $ showString "Unilateral " . showsPrec 11 w

instance HasSymTokens (Action s) => HasSymTokens (Action (WithInstances s)) where
  getAllSymTokens (UnderlyingAction a) = getAllSymTokens a
  getAllSymTokens Unilateral{}         = mempty

deriving via StateModel.HasNoVariables Wallet instance StateModel.HasVariables Wallet

instance forall s. ContractModel s => ContractModel (WithInstances s) where
  data Action (WithInstances s) = UnderlyingAction (Action s)
                                | Unilateral Wallet
                                deriving Generic
  initialState                        = WithInstances initialState
  waitProbability                     = waitProbability . fmap withoutInstances
  arbitraryWaitInterval               = arbitraryWaitInterval . fmap withoutInstances
  actionName (UnderlyingAction a) = actionName a
  actionName Unilateral{}         = "Unilateral"
  arbitraryAction                     = fmap UnderlyingAction . arbitraryAction . fmap withoutInstances
  precondition s (UnderlyingAction a) = precondition (withoutInstances <$> s) a
  precondition _ Unilateral{}         = True
  nextReactiveState slot              = coerceSpec $ nextReactiveState @s slot
  nextState (UnderlyingAction a) = coerceSpec $ nextState a
  nextState Unilateral{}         = pure ()
  shrinkAction s (UnderlyingAction a) = UnderlyingAction <$> shrinkAction (withoutInstances <$> s) a
  shrinkAction _ Unilateral{}         = []
  restricted (UnderlyingAction a) = restricted a
  restricted Unilateral{}         = True

-- | Options for controlling coverage checking requirements
--
-- * `checkCoverage` tells you whether or not to run the coverage checks at all.
-- * `endpointCoverageEq instance endpointName` tells us what percentage of tests are required to include
-- a call to the endpoint `endpointName` in the contract at `instance`.
-- * `coverIndex` is the coverage index obtained from the `CompiledCodeIn` of the validator.
data CoverageOptions = CoverageOptions
  { _checkCoverage       :: Bool
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
defaultCoverageOptions = CoverageOptions
  { _checkCoverage = False
  , _endpointCoverageReq = \ _ _ -> 0
  , _coverageIndex = mempty
  , _coverageIORef = Nothing }

-- | Default check options that include a large amount of Ada in the initial distributions to avoid having
-- to write `ContractModel`s that keep track of balances.
defaultCheckOptionsContractModel :: CheckOptions
defaultCheckOptionsContractModel =
  let initialValue = lovelaceValueOf 100_000_000_000_000_000 in
  defaultCheckOptions & emulatorConfig
                      . initialChainState .~ (Left . Map.fromList $ zip knownWallets (repeat initialValue))

-- | Run QuickCheck on a property that tracks coverage and print its coverage report.
quickCheckWithCoverage :: QC.Testable prop
                       => QC.Args
                       -> CoverageOptions
                       -> (CoverageOptions -> prop)
                       -> IO CoverageReport
quickCheckWithCoverage qcargs opts prop = fst <$> quickCheckWithCoverageAndResult qcargs opts prop

quickCheckWithCoverageAndResult :: QC.Testable prop
                                => QC.Args
                                -> CoverageOptions
                                -> (CoverageOptions -> prop)
                                -> IO (CoverageReport, Result)
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

balanceChangePredicate :: ProtocolParameters -> ContractModelResult state -> Property
balanceChangePredicate ps result =
  let prettyAddr a = maybe (show a) show $ addressToWallet a
  in assertBalanceChangesMatch (BalanceChangeOptions False signerPaysFees ps prettyAddr) result

threatModelPredicate :: ThreatModel a -> ProtocolParameters -> ContractModelResult state -> Property
threatModelPredicate m ps result = assertThreatModel m ps result

-- | Check a threat model on all transactions produced by the given actions.
checkThreatModel ::
    CheckableContractModel state
    => ThreatModel a
    -> Actions (WithInstances state)                 -- ^ The actions to run
    -> Property
checkThreatModel = checkThreatModelWithOptions defaultCheckOptionsContractModel defaultCoverageOptions

-- | Check a threat model on all transactions produced by the given actions.
checkThreatModelWithOptions ::
    CheckableContractModel state
    => CheckOptions                                  -- ^ Emulator options
    -> CoverageOptions                               -- ^ Coverage options
    -> ThreatModel a
    -> Actions (WithInstances state)                 -- ^ The actions to run
    -> Property
checkThreatModelWithOptions opts covopts m actions =
  propRunActionsWithOptions opts covopts (\ _ -> pure True) (threatModelPredicate m) actions

-- | Run a `Actions` in the emulator and check that the model and the emulator agree on the final
--   wallet balance changes. Equivalent to
--
-- @
-- propRunActions_ hs actions = `propRunActions` hs (`const` `$` `pure` `True`) actions
-- @
propRunActions_ ::
    CheckableContractModel state
    => Actions (WithInstances state)                 -- ^ The actions to run
    -> Property
propRunActions_ actions =
    propRunActions (\ _ -> pure True) balanceChangePredicate actions

-- | Run a `Actions` in the emulator and check that the model and the emulator agree on the final
--   wallet balance changes, and that the given `TracePredicate` holds at the end. Equivalent to:
--
-- @
-- propRunActions = `propRunActionsWithOptions` `defaultCheckOptionsContractModel` `defaultCoverageOptions`
-- @
propRunActions ::
    CheckableContractModel state
    => (ModelState (WithInstances state) -> TracePredicate) -- ^ Predicate to check at the end
    -> (ProtocolParameters -> ContractModelResult (WithInstances state) -> Property) -- ^ Predicate to run on the contract model
    -> Actions (WithInstances state)                        -- ^ The actions to run
    -> Property
propRunActions = propRunActionsWithOptions defaultCheckOptionsContractModel defaultCoverageOptions


propRunActionsWithOptions :: forall state.
    CheckableContractModel state
    => CheckOptions                                         -- ^ Emulator options
    -> CoverageOptions                                      -- ^ Coverage options
    -> (ModelState (WithInstances state) -> TracePredicate) -- ^ Predicate to check at the end of execution
    -> (ProtocolParameters -> ContractModelResult (WithInstances state) -> Property) -- ^ Predicate to run on the contract model
    -> Actions (WithInstances state)                        -- ^ The actions to run
    -> Property
propRunActionsWithOptions opts copts predicate contractmodelPredicate actions =
    asserts finalState QC..&&.
    monadic runFinalPredicate monadicPredicate
    where
        finalState = stateAfter actions

        monadicPredicate :: PropertyM (RunMonad (EmulatorTraceWithInstances state)) Property
        monadicPredicate = do
            ps <- QC.run . lift $ fmap pProtocolParams EmulatorControl.getParams
            QC.run . lift $ activateWallets (\ _ -> error "No SymTokens yet") initialInstances
            result <- runContractModel actions
            pure $ contractmodelPredicate ps result

        runFinalPredicate :: RunMonad (EmulatorTraceWithInstances state) Property
                          -> Property
        runFinalPredicate emulatorTrace =
          let traceWithResult :: EmulatorTrace Property
              traceWithResult = fmap fst
                              . runState IMNil
                              . fmap fst
                              . runWriterT
                              . unRunMonad
                              $ emulatorTrace
          in QC.monadicIO $ checkPredicateInner opts (noMainThreadCrashes .&&. predicate finalState)
                                                traceWithResult debugOutput
                                                assertResult cover
                            >>= \res -> case res of
                                  Left err   -> return $ counterexample ("checkPredicateInner terminated with error: " ++ show err)
                                                       $ property False
                                  Right prop -> return prop

        cover report | copts ^. checkCoverage
                     , Just ref <- copts ^. coverageIORef =
                       report `deepseq`
                        (QC.monitor $ \ p -> ioProperty $ do
                           modifyIORef ref (report<>)
                           return p)
                     | otherwise = pure ()

        debugOutput :: Monad m => String -> PropertyM m ()
        debugOutput = QC.monitor . counterexample

        assertResult :: Monad m => Bool -> PropertyM m ()
        assertResult = QC.assert

        -- don't accept traces where the main thread crashes
        noMainThreadCrashes :: TracePredicate
        noMainThreadCrashes = assertUserLog $ \ log -> null [ () | UserThreadErr _ <- view eteEvent <$> log ]

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
  { nlfpMainStrategy   :: DL (WithInstances model) ()   -- TODO: wrapper for DL
    -- ^ Strategy to recover all funds from the contract in any reachable state.
  , nlfpWalletStrategy :: Wallet -> DL (WithInstances model) ()
    -- ^ A strategy for each wallet to recover as much (or more) funds as the main strategy would
    --   give them in a given state, without the assistance of any other wallet.
  , nlfpOverhead       :: ModelState (WithInstances model) -> SymValue
    -- ^ An initial amount of overhead value that may be lost - e.g. setup fees for scripts that
    -- can't be recovered.
  , nlfpErrorMargin    :: ModelState (WithInstances model) -> SymValue
    -- ^ The total amount of margin for error in the value collected by the WalletStrategy compared
    -- to the MainStrategy. This is useful if your contract contains rounding code that makes the order
    -- of operations have a small but predictable effect on the value collected by different wallets.
  }
data NoLockedFundsProofLight model = NoLockedFundsProofLight
  { nlfplMainStrategy :: DL (WithInstances model) () }

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
  :: CheckableContractModel model
  => NoLockedFundsProof model
  -> Property
checkNoLockedFundsProof = checkNoLockedFundsProofWithOptions defaultCheckOptionsContractModel

checkNoLockedFundsProofFast
  :: CheckableContractModel model
  => NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofFast = checkNoLockedFundsProofFastWithOptions defaultCheckOptionsContractModel

checkNoLockedFundsProofWithOptions
  :: CheckableContractModel model
  => CheckOptions
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofWithOptions options =
  checkNoLockedFundsProof' prop
  where
    prop = propRunActionsWithOptions options defaultCoverageOptions (\ _ -> TracePredicate $ pure True) balanceChangePredicate

checkNoLockedFundsProofFastWithOptions
  :: CheckableContractModel model
  => CheckOptions
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProofFastWithOptions _ = checkNoLockedFundsProof' (const $ property True)

checkNoLockedFundsProof'
  :: CheckableContractModel model
  => (Actions (WithInstances model) -> Property)
  -> NoLockedFundsProof model
  -> Property
checkNoLockedFundsProof' run NoLockedFundsProof{nlfpMainStrategy   = mainStrat,
                                                nlfpWalletStrategy = walletStrat,
                                                nlfpOverhead       = overhead,
                                                nlfpErrorMargin    = wiggle } =
    forAllDL anyActions_ $ \ (Actions as) ->
    let ans0 = (annotatedStateAfter $ Actions as) in
    forAllUniqueDL ans0 mainStrat $ \ (Actions as') ->
          let s0 = (stateAfter $ Actions as)
              s = stateAfter $ Actions (as ++ as') in
            foldl (QC..&&.) (counterexample "Main run prop" (run (Actions $ as ++ as')) QC..&&.
                             (counterexample "Main strategy" . counterexample (show . Actions $ as ++ as') $ prop s0 s))
                            [ walletProp ans0 s0 as w bal
                            | (addr, bal) <- Map.toList (s ^. balanceChanges)
                            , Just w <- [addressToWallet addr]
                            , not $ bal `symLeq` (s0 ^. balanceChange addr) ]
                            -- if the main strategy leaves w with <= the starting value, then doing nothing is a good wallet strategy.
    where
        prop s0 s =
          -- TODO: check that nothing is locked by scripts
          let lockedVal = lockedValue s
          in (counterexample ("Locked funds should be at most " ++ show (overhead s0) ++ ", but they are\n  " ++ show lockedVal)
            $ symLeq lockedVal (overhead s0))

        walletProp ans0 s0 as w bal =
          DL.forAllUniqueDL ans0 (DL.action (ContractAction False $ Unilateral w) >> walletStrat w) $ \ acts ->
          let Actions as' = fromStateModelActions acts
              wig = wiggle s0
              err  = "Unilateral strategy for " ++ show w ++ " should have gotten it at least\n" ++
                     "  " ++ show bal ++ "\n" ++
                     concat [ "with wiggle room\n" ++ "  " ++ show wig ++ "\n"
                            | wig /= mempty ] ++
                     "but it got\n" ++
                     "  " ++ show bal'
              bal' = stateAfter (Actions $ as ++ as') ^. balanceChange (walletAddress w)
              smacts = Actions $ as ++ as' -- toStateModelActions (Actions as) <> acts
              err' = "The ContractModel's Unilateral behaviour for " ++ show w ++ " does not match the actual behaviour for actions:\n"
                   ++ show smacts
          in counterexample err (symLeq bal (bal' <> wig))
          QC..&&. counterexample err' (run smacts)

actionsFromList :: [Action s] -> Actions s
actionsFromList = Actions . zipWith NoBind (StateModel.mkVar <$> [0..])

-- TODO: possibly there's a nicer way to do this...?
walletAddress :: Wallet -> CardanoAPI.AddressInEra Era
walletAddress w
  | Just hash <- CardanoAPI.deserialiseFromRawBytes (CardanoAPI.AsHash CardanoAPI.AsPaymentKey)
                                                    (Builtins.fromBuiltin pkh) =
      CardanoAPI.shelleyAddressInEra $
      -- TODO: this is all wrong! It needs to be some magic version of testnet
      CardanoAPI.makeShelleyAddress testnet
                                    (CardanoAPI.PaymentCredentialByKey hash)
                                    CardanoAPI.NoStakeAddress
  | otherwise = error $ "Bad wallet: " ++ show w
  where pkh = getPubKeyHash $ unPaymentPubKeyHash $ mockWalletPaymentPubKeyHash w

addressToWallet :: CardanoAPI.AddressInEra Era -> Maybe Wallet
addressToWallet a = List.lookup a [ (walletAddress w, w) | w <- knownWallets ]

checkNoLockedFundsProofLight
  :: CheckableContractModel model
  => NoLockedFundsProofLight model
  -> Property
checkNoLockedFundsProofLight NoLockedFundsProofLight{nlfplMainStrategy = mainStrat} =
  forAllDL anyActions_ $ \ (Actions as) ->
    forAllUniqueDL (annotatedStateAfter $ Actions as) mainStrat $ \ (Actions as') ->
      counterexample "Main run prop" (run $ Actions $ as ++ as')
  where
    run = propRunActionsWithOptions defaultCheckOptionsContractModel
                                    defaultCoverageOptions (\ _ -> TracePredicate $ pure True)
                                    balanceChangePredicate

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
      all (\ec -> Prelude.not $ (Just $ Builtins.fromBuiltin ec) `isAcceptedBy` wl)
          (Map.keys allErrorCodes \\ [checkHasFailedError])

mkWhitelist :: [Text.Text] -> Whitelist
mkWhitelist txs = defaultWhitelist <> Whitelist (Set.fromList txs)

defaultWhitelist :: Whitelist
defaultWhitelist = Whitelist . Set.singleton $ Builtins.fromBuiltin checkHasFailedError

-- | Check that running a contract model does not result in validation
-- failures that are not accepted by the whitelist.
checkErrorWhitelist :: CheckableContractModel m
                    => Whitelist
                    -> Actions (WithInstances m)
                    -> Property
checkErrorWhitelist = checkErrorWhitelistWithOptions defaultCheckOptionsContractModel defaultCoverageOptions

-- | Check that running a contract model does not result in validation
-- failures that are not accepted by the whitelist.
checkErrorWhitelistWithOptions :: forall m.
                                  CheckableContractModel m
                               => CheckOptions
                               -> CoverageOptions
                               -> Whitelist
                               -> Actions (WithInstances m)
                               -> Property
checkErrorWhitelistWithOptions opts copts whitelist =
  propRunActionsWithOptions opts copts (const check) balanceChangePredicate
  where
    check :: TracePredicate
    check = checkOnchain .&&. (assertNoFailedTransactions .||. checkOffchain)

    checkOnchain :: TracePredicate
    checkOnchain = assertChainEvents checkEvents

    checkOffchain :: TracePredicate
    checkOffchain = assertFailedTransaction (\ _ ve -> case ve of {ScriptFailure e -> checkEvent e; _ -> False})

    checkEvent :: ScriptError -> Bool
    checkEvent (EvaluationError log msg) | "CekEvaluationFailure" `isPrefixOf` msg = listToMaybe (reverse log) `isAcceptedBy` whitelist
    checkEvent (EvaluationError _ msg) | "BuiltinEvaluationFailure" `isPrefixOf` msg = False
    checkEvent _ = False

    checkEvents :: [ChainEvent] -> Bool
    checkEvents events = all checkEvent [ f | (TxnValidationFail _ _ _ (ScriptFailure f) _ _) <- events ]

