{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Plutus.Contract.Test.Certification.Run
  ( -- * A certification report holds all the necessary information
    -- to make sense of certification results
    CertificationReport
  , certResJSON
  -- * There are a tonne of lenses
  , certRes_standardPropertyResult
  , certRes_doubleSatisfactionResult
  , certRes_noLockedFundsResult
  , certRes_noLockedFundsLightResult
  , certRes_standardCrashToleranceResult
  , certRes_unitTestResults
  , certRes_coverageReport
  , certRes_whitelistOk
  , certRes_whitelistResult
  , certRes_DLTests
  -- * and we have a function for running certification
  , CertificationOptions(..)
  , CertificationEvent(..)
  , CertificationTask(..)
  , defaultCertificationOptions
  , certify
  , certifyWithOptions
  ) where

import Control.Concurrent.Chan
import Control.Concurrent.STM
import Control.Exception
import Control.Lens
import Control.Monad.Writer
import Data.Aeson (FromJSON (..), ToJSON (..), encode)
import Data.ByteString.Lazy.Char8
import Data.IntMap qualified as IntMap
import Data.Maybe
import GHC.Generics
import Plutus.Contract.Test.Certification
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.CrashTolerance
import Plutus.Contract.Test.Coverage
import PlutusTx.Coverage
import System.Random.SplitMix
import Test.QuickCheck as QC
import Test.QuickCheck.Property
import Test.QuickCheck.Random as QC
import Test.Tasty as Tasty
import Test.Tasty.Runners as Tasty
import Text.Read hiding (lift)

newtype JSONShowRead a = JSONShowRead a

instance Show a => ToJSON (JSONShowRead a) where
  toJSON (JSONShowRead a) = toJSON (show a)

instance Read a => FromJSON (JSONShowRead a) where
  parseJSON v = do
    str <- parseJSON v
    case readMaybe str of
      Nothing -> fail "JSONShowRead: readMaybe Nothing"
      Just a  -> return $ JSONShowRead a

deriving via (JSONShowRead SMGen) instance FromJSON SMGen
deriving via (JSONShowRead SMGen) instance ToJSON SMGen

deriving via SMGen instance FromJSON QCGen
deriving via SMGen instance ToJSON QCGen
deriving instance Generic QC.Result
deriving instance ToJSON QC.Result
deriving instance FromJSON QC.Result

instance ToJSON SomeException where
  toJSON (SomeException e) = toJSON (show e)
instance FromJSON SomeException where
  parseJSON v = do
    str <- parseJSON v
    return $ SomeException (ErrorCall str)

deriving via (JSONShowRead Tasty.Result) instance ToJSON Tasty.Result

data CertificationReport m = CertificationReport {
    _certRes_standardPropertyResult       :: QC.Result,
    _certRes_doubleSatisfactionResult     :: QC.Result,
    _certRes_noLockedFundsResult          :: Maybe QC.Result,
    _certRes_noLockedFundsLightResult     :: Maybe QC.Result,
    _certRes_standardCrashToleranceResult :: Maybe QC.Result,
    _certRes_unitTestResults              :: [Tasty.Result],
    _certRes_coverageReport               :: CoverageReport,
    _certRes_whitelistOk                  :: Maybe Bool,
    _certRes_whitelistResult              :: Maybe QC.Result,
    _certRes_DLTests                      :: [(String, QC.Result)]
  } deriving (Show, Generic, ToJSON)
makeLenses ''CertificationReport

certResJSON :: CertificationReport m -> String
certResJSON = unpack . encode

data CertificationEvent = QuickCheckTestEvent (Maybe Bool)  -- ^ Nothing if discarded, otherwise test result
                        | StartCertificationTask CertificationTask
  deriving (Eq, Show)

data CertificationTask = UnitTestsTask
                       | StandardPropertyTask
                       | DoubleSatisfactionTask
                       | NoLockedFundsTask
                       | NoLockedFundsLightTask
                       | CrashToleranceTask
                       | WhitelistTask
                       | DLTestsTask
  deriving (Eq, Show, Enum, Bounded)

data CertificationOptions = CertificationOptions { certOptNumTests  :: Int
                                                 , certOptOutput    :: Bool
                                                 , certEventChannel :: Maybe (Chan CertificationEvent)
                                                 }

defaultCertificationOptions :: CertificationOptions
defaultCertificationOptions = CertificationOptions { certOptOutput = True
                                                   , certOptNumTests = 100
                                                   , certEventChannel = Nothing }

type CertMonad = WriterT CoverageReport IO

liftIORep :: IO (CoverageReport, a) -> CertMonad a
liftIORep io = do
  (rep, a) <- lift io
  tell rep
  return a

runCertMonad :: CertMonad (CertificationReport m) -> IO (CertificationReport m)
runCertMonad m = do
  (rep, cov) <- runWriterT m
  return $ rep & certRes_coverageReport %~ (<> cov)

addOnTestEvents :: Testable prop => CertificationOptions -> prop -> Property
addOnTestEvents opts prop
  | Just ch <- certEventChannel opts = mapResult (addCallback ch) prop
  | otherwise                        = property prop
  where
    addCallback ch r = r { callbacks = cb : callbacks r }
      where cb = PostTest NotCounterexample $ \ _st res -> writeChan ch $ QuickCheckTestEvent (ok res)

runStandardProperty :: forall m. ContractModel m => CertificationOptions -> CoverageIndex -> CertMonad QC.Result
runStandardProperty opts covIdx = liftIORep $ quickCheckWithCoverageAndResult
                                  (mkQCArgs opts)
                                  (set coverageIndex covIdx defaultCoverageOptions)
                                $ \ covopts -> addOnTestEvents opts $
                                               propRunActionsWithOptions
                                                 @m
                                                 defaultCheckOptionsContractModel
                                                 covopts
                                                 (\ _ -> pure True)

checkDS :: forall m. ContractModel m => CertificationOptions -> CoverageIndex -> CertMonad QC.Result
checkDS opts covIdx = liftIORep $ quickCheckWithCoverageAndResult
                                  (mkQCArgs opts)
                                  (set coverageIndex covIdx defaultCoverageOptions)
                                $ \ covopts -> addOnTestEvents opts $
                                               checkDoubleSatisfactionWithOptions
                                                 @m
                                                 defaultCheckOptionsContractModel
                                                 covopts

checkNoLockedFunds :: ContractModel m => CertificationOptions -> NoLockedFundsProof m -> CertMonad QC.Result
checkNoLockedFunds opts prf = lift $ quickCheckWithResult
                                       (mkQCArgs opts)
                                       $ addOnTestEvents opts $ checkNoLockedFundsProof prf

checkNoLockedFundsLight :: ContractModel m => CertificationOptions -> NoLockedFundsProofLight m -> CertMonad QC.Result
checkNoLockedFundsLight opts prf =
  lift $ quickCheckWithResult
          (mkQCArgs opts)
          $ addOnTestEvents opts $ checkNoLockedFundsProofLight prf

mkQCArgs :: CertificationOptions -> Args
mkQCArgs CertificationOptions{..} = stdArgs { chatty = certOptOutput , maxSuccess = certOptNumTests }

runUnitTests :: (CoverageRef -> TestTree) -> CertMonad [Tasty.Result]
runUnitTests t = liftIORep $ do
    ref <- newCoverageRef
    res <- launchTestTree mempty (t ref) $ \ status -> do
      rs <- atomically $ mapM waitForDone (IntMap.elems status)
      return $ \ _ -> return rs
    cov <- readCoverageRef ref
    return (CoverageReport mempty cov, res)
  where
    waitForDone tv = do
      s <- readTVar tv
      case s of
        Done r -> return r
        _      -> retry

checkDerived :: forall d m c. (c m => ContractModel (d m))
             => Maybe (Instance c m)
             -> CertificationOptions
             -> CoverageIndex
             -> CertMonad (Maybe QC.Result)
checkDerived Nothing _ _                 = return Nothing
checkDerived (Just Instance) opts covIdx = Just <$> runStandardProperty @(d m) opts covIdx

checkWhitelist :: forall m. ContractModel m
               => Maybe Whitelist
               -> CertificationOptions
               -> CoverageIndex
               -> CertMonad (Maybe QC.Result)
checkWhitelist Nothing _ _           = return Nothing
checkWhitelist (Just wl) opts covIdx = do
  a <- liftIORep $ quickCheckWithCoverageAndResult
                  (mkQCArgs opts)
                  (set coverageIndex covIdx defaultCoverageOptions)
                  $ \ covopts -> addOnTestEvents opts $
                                 checkErrorWhitelistWithOptions @m
                                    defaultCheckOptionsContractModel
                                    covopts wl
  return (Just a)

checkDLTests :: forall m. ContractModel m
            => [(String, DL m ())]
            -> CertificationOptions
            -> CoverageIndex
            -> CertMonad [(String, QC.Result)]
checkDLTests tests opts covIdx =
  sequence [(s,) <$> liftIORep (quickCheckWithCoverageAndResult
                                    (mkQCArgs opts)
                                    (set coverageIndex covIdx defaultCoverageOptions)
                                    $ \ covopts ->
                                        addOnTestEvents opts $
                                        forAllDL dl (propRunActionsWithOptions @m defaultCheckOptionsContractModel covopts (const $ pure True)))
           | (s, dl) <- tests ]

startTaskEvent :: CertificationOptions -> CertificationTask -> CertMonad ()
startTaskEvent opts task | Just ch <- certEventChannel opts = liftIO $ writeChan ch $ StartCertificationTask task
                         | otherwise                        = pure ()

certify :: forall m. ContractModel m => Certification m -> IO (CertificationReport m)
certify = certifyWithOptions defaultCertificationOptions

certifyWithOptions :: forall m. ContractModel m
                   => CertificationOptions
                   -> Certification m
                   -> IO (CertificationReport m)
certifyWithOptions opts Certification{..} = runCertMonad $ do
  -- Unit tests
  startTaskEvent opts UnitTestsTask
  unitTests    <- fromMaybe [] <$> traverse runUnitTests certUnitTests
  -- Standard property
  startTaskEvent opts StandardPropertyTask
  qcRes        <- runStandardProperty @m opts certCoverageIndex
  -- Double satisfaction
  startTaskEvent opts DoubleSatisfactionTask
  dsRes        <- checkDS @m opts certCoverageIndex
  -- No locked funds
  startTaskEvent opts NoLockedFundsTask
  noLock       <- traverse (checkNoLockedFunds opts) certNoLockedFunds
  -- No locked funds light
  startTaskEvent opts NoLockedFundsLightTask
  noLockLight  <- traverse (checkNoLockedFundsLight opts) certNoLockedFundsLight
  -- Crash tolerance
  startTaskEvent opts CrashToleranceTask
  ctRes        <- checkDerived @WithCrashTolerance certCrashTolerance opts certCoverageIndex
  -- Whitelist
  startTaskEvent opts WhitelistTask
  wlRes        <- checkWhitelist @m certWhitelist opts certCoverageIndex
  -- DL tests
  startTaskEvent opts DLTestsTask
  dlRes        <- checkDLTests @m certDLTests opts certCoverageIndex
  -- Final results
  return $ CertificationReport
            { _certRes_standardPropertyResult       = qcRes
            , _certRes_doubleSatisfactionResult     = dsRes
            , _certRes_standardCrashToleranceResult = ctRes
            , _certRes_noLockedFundsResult          = noLock
            , _certRes_noLockedFundsLightResult     = noLockLight
            , _certRes_unitTestResults              = unitTests
            , _certRes_coverageReport               = CoverageReport certCoverageIndex mempty
            , _certRes_whitelistOk                  = whitelistOk <$> certWhitelist
            , _certRes_whitelistResult              = wlRes
            , _certRes_DLTests                      = dlRes }
