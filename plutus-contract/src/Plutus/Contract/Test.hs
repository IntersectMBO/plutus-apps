{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

-- | Testing contracts with HUnit and Tasty
module Plutus.Contract.Test(
      module X
    , TracePredicateF(..)
    , TracePredicate
    , ContractConstraints
    , Plutus.Contract.Test.not
    , (.&&.)
    , (.||.)
    , w1, w2, w3, w4, w5, w6, w7, w8, w9, w10
    -- * Assertions
    , endpointAvailable
    , assertDone
    , assertNotDone
    , assertContractError
    , Outcome(..)
    , assertOutcome
    , assertInstanceLog
    , assertNoFailedTransactions
    , assertValidatedTransactionCount
    , assertFailedTransaction
    , assertHooks
    , assertResponses
    , assertUserLog
    , assertBlockchain
    , assertChainEvents
    , assertChainEvents'
    , assertAccumState
    , Shrinking(..)
    , assertResumableResult
    , tx
    , anyTx
    , assertEvents
    , walletFundsChange
    , walletFundsExactChange
    , walletPaidFees
    , waitingForSlot
    , valueAtAddress
    , dataAtAddress
    , reasonable
    , reasonable'
    -- * Checking predicates
    , checkPredicate
    , checkPredicateCoverage
    , checkPredicateOptions
    , checkPredicateGen
    , checkPredicateGenOptions
    , checkPredicateInner
    , checkPredicateInnerStream
    , checkEmulatorFails
    , CheckOptions
    , defaultCheckOptions
    , minLogLevel
    , emulatorConfig
    , changeInitialWalletValue
    , allowBigTransactions
    -- * Etc
    , goldenPir
    ) where

import Control.Applicative (liftA2)
import Control.Arrow ((>>>))
import Control.Foldl (FoldM)
import Control.Foldl qualified as L
import Control.Lens (_Left, at, ix, makeLenses, over, preview, to, (&), (.~), (^.))
import Control.Monad (unless)
import Control.Monad.Freer (Eff, interpretM, runM)
import Control.Monad.Freer.Error (Error, runError)
import Control.Monad.Freer.Extras.Log (LogLevel (..), LogMessage (..))
import Control.Monad.Freer.Reader
import Control.Monad.Freer.Writer (Writer (..), tell)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default (Default (..))
import Data.Foldable (fold, toList, traverse_)
import Data.Map qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.OpenUnion
import Data.Proxy (Proxy (..))
import Data.String (IsString (..))
import Data.Text qualified as Text
import Data.Void
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Plutus.Contract.Effects (ActiveEndpoint (..), PABReq, PABResp)
import Prettyprinter
import Prettyprinter.Render.Text (renderStrict)


import Hedgehog (Property, forAll, property)
import Hedgehog qualified
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.Providers (TestTree)

import Ledger.Ada qualified as Ada
import Ledger.Constraints.OffChain (UnbalancedTx)
import Ledger.Tx (Tx, onCardanoTx)
import Plutus.Contract.Effects qualified as Requests
import Plutus.Contract.Request qualified as Request
import Plutus.Contract.Resumable (Request (..), Response (..))
import Plutus.Contract.Resumable qualified as State
import Plutus.Contract.Types (Contract (..), IsContract (..), ResumableResult, shrinkResumableResult)
import PlutusTx (CompiledCode, FromData (..), getPir)
import PlutusTx.Prelude qualified as P

import Ledger qualified
import Ledger.Address (Address)
import Ledger.Generators (GeneratorModel, Mockchain (..))
import Ledger.Generators qualified as Gen
import Ledger.Index (ScriptValidationEvent, ValidationError)
import Ledger.Slot (Slot)
import Ledger.Value (Value)
import Plutus.V1.Ledger.Scripts (Validator)
import Plutus.V1.Ledger.Scripts qualified as Ledger

import Data.IORef
import Plutus.Contract.Test.Coverage
import Plutus.Contract.Test.MissingLovelace (calculateDelta)
import Plutus.Contract.Trace as X
import Plutus.Trace.Emulator (EmulatorConfig (..), EmulatorTrace, params, runEmulatorStream)
import Plutus.Trace.Emulator.Types (ContractConstraints, ContractInstanceLog, ContractInstanceState (..),
                                    ContractInstanceTag, UserThreadMsg)
import PlutusTx.Coverage
import Streaming qualified as S
import Streaming.Prelude qualified as S
import Wallet.Emulator (EmulatorEvent, EmulatorTimeEvent)
import Wallet.Emulator.Chain (ChainEvent)
import Wallet.Emulator.Folds (EmulatorFoldErr (..), Outcome (..), describeError, postMapM)
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream (filterLogLevel, foldEmulatorStreamM, initialChainState, initialDist)

type TestEffects = '[Reader InitialDistribution, Error EmulatorFoldErr, Writer (Doc Void), Writer CoverageData]
newtype TracePredicateF a = TracePredicate (forall effs. Members TestEffects effs => FoldM (Eff effs) EmulatorEvent a)
  deriving (Functor)
instance Applicative TracePredicateF where
  pure a = TracePredicate $ pure a
  TracePredicate f <*> TracePredicate a = TracePredicate (f <*> a)
type TracePredicate = TracePredicateF Bool

infixl 3 .&&.
infixl 2 .||.

(.&&.) :: TracePredicate -> TracePredicate -> TracePredicate
(.&&.) = liftA2 (&&)

(.||.) :: TracePredicate -> TracePredicate -> TracePredicate
(.||.) = liftA2 (||)

not :: TracePredicate -> TracePredicate
not = fmap Prelude.not

-- | Options for running the
data CheckOptions =
    CheckOptions
        { _minLogLevel    :: LogLevel -- ^ Minimum log level for emulator log messages to be included in the test output (printed if the test fails)
        , _emulatorConfig :: EmulatorConfig
        }

makeLenses ''CheckOptions

defaultCheckOptions :: CheckOptions
defaultCheckOptions =
    CheckOptions
        { _minLogLevel = Info
        , _emulatorConfig = def
        }

-- | Modify the value assigned to the given wallet in the initial distribution.
changeInitialWalletValue :: Wallet -> (Value -> Value) -> CheckOptions -> CheckOptions
changeInitialWalletValue wallet = over (emulatorConfig . initialChainState . _Left . ix wallet)

-- | Set higher limits on transaction size and execution units.
-- This can be used to work around @MaxTxSizeUTxO@ and @ExUnitsTooBigUTxO@ errors.
-- Note that if you need this your Plutus script will probably not validate on Mainnet.
allowBigTransactions :: CheckOptions -> CheckOptions
allowBigTransactions = over (emulatorConfig . params) Ledger.allowBigTransactions

-- | Check if the emulator trace meets the condition
checkPredicate ::
    String -- ^ Descriptive name of the test
    -> TracePredicate -- ^ The predicate to check
    -> EmulatorTrace ()
    -> TestTree
checkPredicate = checkPredicateOptions defaultCheckOptions

checkPredicateCoverage ::
       String -- ^ Descriptive name of the test
    -> CoverageRef
    -> TracePredicate -- ^ The predicate to check
    -> EmulatorTrace ()
    -> TestTree
checkPredicateCoverage nm (CoverageRef ioref) predicate action =
  HUnit.testCaseSteps nm $ \step -> do
        checkPredicateInner defaultCheckOptions predicate action step (HUnit.assertBool nm) (\ rep -> modifyIORef ioref (rep<>))

-- | Check if the emulator trace fails with the condition
checkEmulatorFails ::
    String -- ^ Descriptive name of the test
    -> CheckOptions
    -> TracePredicate
    -> EmulatorTrace () -- ^ The trace that should fail
    -> TestTree
checkEmulatorFails nm options predicate action = do
    HUnit.testCaseSteps nm $ \step -> do
        checkPredicateInner options predicate action step (HUnit.assertBool nm . Prelude.not) (const $ return ())

-- | Check if the emulator trace meets the condition, using the
--   'GeneratorModel' to generate initial transactions for the blockchain
checkPredicateGen ::
    GeneratorModel
    -> TracePredicate
    -> EmulatorTrace ()
    -> Property
checkPredicateGen = checkPredicateGenOptions defaultCheckOptions

-- | Evaluate a trace predicate on an emulator trace, printing out debug information
--   and making assertions as we go.
checkPredicateInner :: forall m.
    Monad m
    => CheckOptions
    -> TracePredicate
    -> EmulatorTrace ()
    -> (String -> m ()) -- ^ Print out debug information in case of test failures
    -> (Bool -> m ()) -- ^ assert
    -> (CoverageData -> m ())
    -> m ()
checkPredicateInner opts@CheckOptions{_emulatorConfig} predicate action annot assert cover =
    checkPredicateInnerStream opts predicate (S.void $ runEmulatorStream _emulatorConfig action) annot assert cover

checkPredicateInnerStream :: forall m.
    Monad m
    => CheckOptions
    -> TracePredicate
    -> (forall effs. S.Stream (S.Of (LogMessage EmulatorEvent)) (Eff effs) ())
    -> (String -> m ()) -- ^ Print out debug information in case of test failures
    -> (Bool -> m ()) -- ^ assert
    -> (CoverageData -> m ())
    -> m ()
checkPredicateInnerStream CheckOptions{_minLogLevel, _emulatorConfig} (TracePredicate predicate) theStream annot assert cover = do
    let dist = _emulatorConfig ^. initialChainState . to initialDist
        consumedStream :: Eff (TestEffects :++: '[m]) Bool
        consumedStream = S.fst' <$> foldEmulatorStreamM (liftA2 (&&) predicate generateCoverage) theStream

        generateCoverage = flip postMapM (L.generalize Folds.emulatorLog) $ (True <$) . tell @CoverageData . getCoverageData

    result <- runM
                $ interpretM @(Writer CoverageData) @m (\case { Tell r -> cover r })
                $ interpretM @(Writer (Doc Void)) @m (\case { Tell d -> annot $ Text.unpack $ renderStrict $ layoutPretty defaultLayoutOptions d })
                $ runError
                $ runReader dist
                $ consumedStream

    unless (result == Right True) $ do
        annot "Test failed."
        annot "Emulator log:"
        S.mapM_ annot
            $ S.hoist runM
            $ S.map (Text.unpack . renderStrict . layoutPretty defaultLayoutOptions . pretty)
            $ filterLogLevel _minLogLevel
            theStream

        case result of
            Left err -> do
                annot "Error:"
                annot (describeError err)
                annot (show err)
                assert False
            Right r -> assert r

-- | A version of 'checkPredicateGen' with configurable 'CheckOptions'
checkPredicateGenOptions ::
    CheckOptions
    -> GeneratorModel
    -> TracePredicate
    -> EmulatorTrace ()
    -> Property
checkPredicateGenOptions options gm predicate action = property $ do
    Mockchain{mockchainInitialTxPool} <- forAll (Gen.genMockchain' gm)
    let options' = options & emulatorConfig . initialChainState .~ Right mockchainInitialTxPool
    checkPredicateInner options' predicate action Hedgehog.annotate Hedgehog.assert (const $ return ())

-- | A version of 'checkPredicate' with configurable 'CheckOptions'
checkPredicateOptions ::
    CheckOptions -- ^ Options to use
    -> String -- ^ Descriptive name of the test
    -> TracePredicate -- ^ The predicate to check
    -> EmulatorTrace ()
    -> TestTree
checkPredicateOptions options nm predicate action = do
    HUnit.testCaseSteps nm $ \step -> do
        checkPredicateInner options predicate action step (HUnit.assertBool nm) (const $ return ())

endpointAvailable
    :: forall (l :: Symbol) w s e a.
       ( KnownSymbol l
       , Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> TracePredicate
endpointAvailable contract inst = TracePredicate $
    let desc = Request.endpointDescription (Proxy @l) in
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks :: [Request ActiveEndpoint]
            hks = mapMaybe (traverse (preview Requests._ExposeEndpointReq)) rqs
        if any (\ActiveEndpoint{aeDescription} -> aeDescription == desc) (rqRequest <$> hks)
            then pure True
            else do
                tell @(Doc Void) ("missing endpoint:" <+> fromString (symbolVal (Proxy :: Proxy l)))
                pure False

tx
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> (UnbalancedTx -> Bool)
    -> String
    -> TracePredicate
tx contract inst flt nm = TracePredicate $
    flip postMapM (Folds.instanceTransactions contract inst) $ \unbalancedTxns -> do
        if any flt unbalancedTxns
        then pure True
        else do
            tell @(Doc Void) $ hsep
                [ "Unbalanced transactions of" <+> pretty inst <> colon
                    <+> nest 2 (vsep (fmap pretty unbalancedTxns))
                , "No transaction with '" <> fromString nm <> "'"]
            pure False

assertEvents
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> ([PABResp] -> Bool)
    -> String
    -> TracePredicate
assertEvents contract inst pr nm = TracePredicate $
    flip postMapM (Folds.instanceResponses contract inst) $ \rqs -> do
        let responses = fmap State.rspResponse rqs
            result = pr responses
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Event log for" <+> pretty inst <> ":"
                , nest 2 (vsep (fmap pretty responses))
                , "Fails" <+> squotes (fromString nm)
                ]
        pure result

-- | Check that the funds at an address meet some condition.
valueAtAddress :: Address -> (Value -> Bool) -> TracePredicate
valueAtAddress address check = TracePredicate $
    flip postMapM (L.generalize $ Folds.valueAtAddress address) $ \vl -> do
        let result = check vl
        unless result $ do
            tell @(Doc Void) ("Funds at address" <+> pretty address <+> "were" <+> pretty vl)
        pure result


-- | Get a datum of a given type 'd' out of a Transaction Output.
getTxOutDatum ::
  forall d.
  (FromData d) =>
  Ledger.CardanoTx ->
  Ledger.TxOut ->
  Maybe d
getTxOutDatum _ (Ledger.TxOut _ _ Nothing) = Nothing
getTxOutDatum tx' (Ledger.TxOut _ _ (Just datumHash)) =
    Map.lookup datumHash (Ledger.getCardanoTxData tx') >>= (Ledger.getDatum >>> fromBuiltinData @d)

dataAtAddress :: forall d . FromData d => Address -> ([d] -> Bool) -> TracePredicate
dataAtAddress address check = TracePredicate $
    flip postMapM (L.generalize $ Folds.utxoAtAddress address) $ \utxo -> do
      let
        datums = mapMaybe (uncurry $ getTxOutDatum @d) $ toList utxo
        result = check datums
      unless result $ do
          tell @(Doc Void) ("Data at address" <+> pretty address <+> "was"
              <+> foldMap (foldMap pretty . Ledger.getCardanoTxData . fst) utxo)
      pure result

waitingForSlot
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> Slot
    -> TracePredicate
waitingForSlot contract inst sl = TracePredicate $
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks :: [Request Slot]
            hks = mapMaybe (traverse (preview Requests._AwaitSlotReq)) rqs
        case filter ((==) sl) (rqRequest <$> hks) of
            [] -> do
                tell @(Doc Void) $ pretty inst <+> "not waiting for any slot notifications. Expected:" <+>  viaShow sl
                pure False
            _ -> pure True

anyTx
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> TracePredicate
anyTx contract inst = tx contract inst (const True) "anyTx"

assertHooks
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> ([PABReq] -> Bool)
    -> String
    -> TracePredicate
assertHooks contract inst p nm = TracePredicate $
    flip postMapM (Folds.instanceRequests contract inst) $ \rqs -> do
        let hks = rqRequest <$> rqs
            result = p hks
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Handlers for" <+> pretty inst <> colon
                , nest 2 (pretty hks)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

-- | Make an assertion about the responses provided to the contract instance.
assertResponses
    :: forall w s e a.
       ( Monoid w
       )
    => Contract w s e a
    -> ContractInstanceTag
    -> ([Response PABResp] -> Bool)
    -> String
    -> TracePredicate
assertResponses contract inst p nm = TracePredicate $
    flip postMapM (Folds.instanceResponses contract inst) $ \rqs -> do
        let result = p rqs
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Record:"
                , nest 2 (pretty rqs)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

data Shrinking = DoShrink | DontShrink
    deriving (Eq, Ord, Show)

-- | make an assertion about the 'ContractInstanceState' of a contract
--   instance
assertResumableResult ::
    forall w s e a.
    ( Monoid w
    , Show e
    , Show a
    , Show w
    )
    => Contract w s e a
    -> ContractInstanceTag
    -> Shrinking
    -> (ResumableResult w e PABResp PABReq a -> Bool)
    -> String
    -> TracePredicate
assertResumableResult contract inst shrinking p nm = TracePredicate $
    let f = case shrinking of { DontShrink -> id; DoShrink -> shrinkResumableResult } in
    flip postMapM (Folds.instanceState contract inst) $ \case
        Nothing -> do
            tell @(Doc Void) $ "No state for " <+> pretty inst
            pure False
        Just ContractInstanceState{instContractState} -> do
            let shrunkState = f instContractState
                result = p shrunkState
            unless result $ do
                tell @(Doc Void) $ vsep
                    [ "Resumable result for" <+> pretty inst
                    , viaShow shrunkState
                    , "Failed" <+> squotes (fromString nm)
                    ]
            pure result

-- | A 'TracePredicate' checking that the wallet's contract instance finished
--   without errors.
assertDone
    :: forall contract w s e a.
    ( Monoid w
    , IsContract contract
    )
    => contract w s e a
    -> ContractInstanceTag
    -> (a -> Bool)
    -> String
    -> TracePredicate
assertDone contract inst pr = assertOutcome contract inst (\case { Done a -> pr a; _ -> False})

-- | A 'TracePredicate' checking that the wallet's contract instance is
--   waiting for input.
assertNotDone
    :: forall contract w s e a.
    ( Monoid w
    , IsContract contract
    )
    => contract w s e a
    -> ContractInstanceTag
    -> String
    -> TracePredicate
assertNotDone contract inst = assertOutcome contract inst (\case { NotDone -> True; _ -> False})

-- | A 'TracePredicate' checking that the wallet's contract instance
--   failed with an error.
assertContractError
    :: forall contract w s e a.
    ( Monoid w
    , IsContract contract
    )
    => contract w s e a
    -> ContractInstanceTag
    -> (e -> Bool)
    -> String
    -> TracePredicate
assertContractError contract inst p = assertOutcome contract inst (\case { Failed err -> p err; _ -> False })

assertOutcome
    :: forall contract w s e a.
       ( Monoid w
       , IsContract contract
       )
    => contract w s e a
    -> ContractInstanceTag
    -> (Outcome e a -> Bool)
    -> String
    -> TracePredicate
assertOutcome contract inst p nm = TracePredicate $
    flip postMapM (Folds.instanceOutcome (toContract contract) inst) $ \outcome -> do
        let result = p outcome
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Outcome of" <+> pretty inst <> colon
                , indent 2 (viaShow result)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

-- | Check that the funds in the wallet have changed by the given amount, exluding fees.
walletFundsChange :: Wallet -> Value -> TracePredicate
walletFundsChange = walletFundsChangeImpl False
-- | Check that the funds in the wallet have changed by the given amount, including fees.
walletFundsExactChange :: Wallet -> Value -> TracePredicate
walletFundsExactChange = walletFundsChangeImpl True

walletFundsChangeImpl :: Bool -> Wallet -> Value -> TracePredicate
walletFundsChangeImpl exact w dlt' = TracePredicate $
    flip postMapM (L.generalize $ (,,) <$> Folds.walletFunds w <*> Folds.walletFees w <*> Folds.walletsAdjustedTxEvents) $ \(finalValue', fees, allWalletsTxOutCosts) -> do
        dist <- ask @InitialDistribution
        let initialValue = fold (dist ^. at w)
            finalValue = finalValue' P.+ if exact then mempty else fees
            dlt = calculateDelta dlt' (Ada.fromValue initialValue) (Ada.fromValue finalValue) allWalletsTxOutCosts
            result = initialValue P.+ dlt == finalValue
        unless result $ do
            tell @(Doc Void) $ vsep $
                [ "Expected funds of" <+> pretty w <+> "to change by"
                , " " <+> viaShow dlt] ++
                (if exact then [] else ["  (excluding" <+> viaShow (Ada.getLovelace (Ada.fromValue fees)) <+> "lovelace in fees)" ]) ++
                if initialValue == finalValue
                then ["but they did not change"]
                else ["but they changed by", " " <+> viaShow (finalValue P.- initialValue),
                      "a discrepancy of",    " " <+> viaShow (finalValue P.- initialValue P.- dlt)]
        pure result

walletPaidFees :: Wallet -> Value -> TracePredicate
walletPaidFees w val = TracePredicate $
    flip postMapM (L.generalize $ Folds.walletFees w) $ \fees -> do
        let result = fees == val
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Expected" <+> pretty w <+> "to pay"
                , " " <+> viaShow val
                , "lovelace in fees, but they paid"
                , " " <+> viaShow fees ]
        pure result

-- | An assertion about the blockchain
assertBlockchain :: ([Ledger.Block] -> Bool) -> TracePredicate
assertBlockchain predicate = TracePredicate $
    flip postMapM (L.generalize Folds.blockchain) $ \chain -> do
        let passing = predicate chain
        unless passing $ do
            tell @(Doc Void) $ "Blockchain does not match predicate."
        pure passing

-- | An assertion about the chain events
assertChainEvents :: ([ChainEvent] -> Bool) -> TracePredicate
assertChainEvents = assertChainEvents' (const "")

-- | An assertion about the chain events with a custom error message
assertChainEvents' :: ([ChainEvent] -> String) -> ([ChainEvent] -> Bool) -> TracePredicate
assertChainEvents' logMsg predicate = TracePredicate $
    flip postMapM (L.generalize Folds.chainEvents) $ \evts -> do
        let passing = predicate evts
        unless passing $ do
            let msg = logMsg evts
            tell @(Doc Void) $ "Chain events do not match predicate" <> if null msg then "" else ":" <+> fromString msg
            traverse_ (tell @(Doc Void) . pretty) evts
        pure passing

-- | Assert that at least one transaction failed to validate, and that all
--   transactions that failed meet the predicate.
assertFailedTransaction :: (Tx -> ValidationError -> [ScriptValidationEvent] -> Bool) -> TracePredicate
assertFailedTransaction predicate = TracePredicate $
    flip postMapM (L.generalize $ Folds.failedTransactions Nothing) $ \case
        [] -> do
            tell @(Doc Void) $ "No transactions failed to validate."
            pure False
        xs -> pure (all (\(_, t, e, evts, _) -> onCardanoTx (\t' -> predicate t' e evts) (const True) t) xs)

-- | Assert that no transaction failed to validate.
assertNoFailedTransactions :: TracePredicate
assertNoFailedTransactions = TracePredicate $
    flip postMapM (L.generalize $ Folds.failedTransactions Nothing) $ \case
        [] -> pure True
        xs -> do
            let prettyTxFail (i, _, err, _, _) = pretty i <> colon <+> pretty err
            tell @(Doc Void) $ vsep ("Transactions failed to validate:" : fmap prettyTxFail xs)
            pure False

-- | Assert that n transactions validated, and no transaction failed to validate.
assertValidatedTransactionCount :: Int -> TracePredicate
assertValidatedTransactionCount expected =
    assertNoFailedTransactions
    .&&.
    TracePredicate (flip postMapM (L.generalize Folds.validatedTransactions) $ \xs ->
        let actual = length xs - 1 in -- ignore the initial wallet distribution transaction
        if actual == expected then pure True else do
            tell @(Doc Void) $ "Unexpected number of validated transactions:" <+> pretty actual
            pure False
    )

assertInstanceLog ::
    ContractInstanceTag
    -> ([EmulatorTimeEvent ContractInstanceLog] -> Bool)
    -> TracePredicate
assertInstanceLog tag pred' = TracePredicate $ flip postMapM (L.generalize $ Folds.instanceLog tag) $ \lg -> do
    let result = pred' lg
    unless result (tell @(Doc Void) $ vsep ("Contract instance log failed to validate:" : fmap pretty lg))
    pure result

assertUserLog ::
    ([EmulatorTimeEvent UserThreadMsg] -> Bool)
    -> TracePredicate
assertUserLog pred' = TracePredicate $ flip postMapM (L.generalize Folds.userLog) $ \lg -> do
    let result = pred' lg
    unless result (tell @(Doc Void) $ vsep ("User log failed to validate:" : fmap pretty lg))
    pure result

-- | Make an assertion about the accumulated state @w@ of
--   a contract instance.
assertAccumState ::
    forall contract w s e a.
    ( Monoid w
    , Show w
    , IsContract contract
    )
    => contract w s e a
    -> ContractInstanceTag
    -> (w -> Bool)
    -> String
    -> TracePredicate
assertAccumState contract inst p nm = TracePredicate $
    flip postMapM (Folds.instanceAccumState (toContract contract) inst) $ \w -> do
        let result = p w
        unless result $ do
            tell @(Doc Void) $ vsep
                [ "Accumulated state of of" <+> pretty inst <> colon
                , indent 2 (viaShow w)
                , "Failed" <+> squotes (fromString nm)
                ]
        pure result

-- | Assert that the size of a 'Validator' is below
--   the maximum.
reasonable :: Validator -> Integer -> HUnit.Assertion
reasonable = reasonable' putStrLn

reasonable' :: (String -> IO ()) -> Validator -> Integer -> HUnit.Assertion
reasonable' logger (Ledger.unValidatorScript -> s) maxSize = do
    let sz = Ledger.scriptSize s
        msg = "Script too big! Max. size: " <> show maxSize <> ". Actual size: " <> show sz
    -- so the actual size is visible in the log
    liftIO $ logger ("Script size: " ++ show sz)
    HUnit.assertBool msg (sz <= maxSize)

-- | Compare a golden PIR file to the provided 'CompiledCode'.
goldenPir :: FilePath -> CompiledCode a -> TestTree
goldenPir path code = goldenVsString "PIR" path (pure $ fromString $ show $ pretty $ fromJust $ getPir code)

w1, w2, w3, w4, w5, w6, w7, w8, w9, w10 :: Wallet
w1 = X.knownWallet 1
w2 = X.knownWallet 2
w3 = X.knownWallet 3
w4 = X.knownWallet 4
w5 = X.knownWallet 5
w6 = X.knownWallet 6
w7 = X.knownWallet 7
w8 = X.knownWallet 8
w9 = X.knownWallet 9
w10 = X.knownWallet 10
