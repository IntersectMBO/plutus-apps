{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Plutus.Contract.Test.Coverage
  ( getInvokedEndpoints
  , getCoverageReport
  , CoverageRef(..)
  , newCoverageRef
  , readCoverageRef
  , writeCoverageReport
  ) where

import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics

import Data.Text qualified as Text

import Control.DeepSeq
import Control.Lens

import Ledger qualified
import Plutus.V1.Ledger.Scripts qualified as Ledger

import PlutusTx.Coverage

import Plutus.Trace.Emulator.Types

import Wallet.Emulator.Chain
import Wallet.Emulator.MultiAgent (EmulatorEvent, EmulatorEvent' (..), EmulatorTimeEvent (..), eteEvent)
import Wallet.Types

import Data.IORef
import Plutus.Contract.Test.Coverage.ReportCoverage qualified as ReportCoverage


-- | Get every endpoint name that has been invoked in the emulator events in `es`
-- indexed by `ContractInstanceTag`
getInvokedEndpoints :: [EmulatorEvent] -> Map ContractInstanceTag (Set String)
getInvokedEndpoints es =
  let cs = [ (view cilTag c, view cilMessage c) | EmulatorTimeEvent _ (InstanceEvent c) <- es ]
      t2ep = [ (t, ep) | (t, ReceiveEndpointCall (EndpointDescription ep) _) <- cs ]
      epsCovered = foldr (\(t, ep) -> Map.insertWith Set.union t (Set.singleton ep)) Map.empty t2ep
  in epsCovered

-- | Collect every executed coverage annotation in the validators executed in `es`
getCoverageReport :: [EmulatorEvent] -> CoverageReport
getCoverageReport es =
  let extractLog e = case e of
        ChainEvent (TxnValidate _ _ valEvs)             -> logOf . Ledger.sveResult <$> valEvs
        ChainEvent (TxnValidationFail _ _ _ _ valEvs _) -> logOf . Ledger.sveResult <$> valEvs
        _                                               -> []

      logOf (Left (Ledger.EvaluationError lg _)) = lg
      logOf (Left _)                             = []
      logOf (Right (_, lg))                      = lg

  in fold $ do
    event <- es
    log <- extractLog $ event ^. eteEvent
    logEvent <- log
    let msg = Text.unpack logEvent
    return $ coverageReportFromLogMsg msg

newtype CoverageRef = CoverageRef (IORef CoverageReport)

newCoverageRef :: IO CoverageRef
newCoverageRef = CoverageRef <$> newIORef mempty

readCoverageRef :: CoverageRef -> IO CoverageReport
readCoverageRef (CoverageRef ioref) = readIORef ioref

-- | Write a coverage report to name.html for the given index.
writeCoverageReport :: String -> CoverageIndex -> CoverageReport -> IO ()
writeCoverageReport = ReportCoverage.writeCoverageReport

-- TODO: Move this to plutus core to avoid orhpan instance
instance NFData CovLoc where
  rnf (CovLoc f sl el sc ec) =
    rnf f  `seq`
    rnf sl `seq`
    rnf el `seq`
    rnf sc `seq`
    rnf ec
instance NFData CoverageAnnotation where
  rnf (CoverLocation loc) = rnf loc
  rnf (CoverBool loc b)   = rnf b `seq` rnf loc
deriving anyclass instance NFData CoverageReport
deriving instance Generic CoverageReport
deriving anyclass instance ToJSON CoverageReport
deriving anyclass instance FromJSON CoverageReport

deriving anyclass instance ToJSON CoverageIndex
deriving anyclass instance FromJSON CoverageIndex

deriving anyclass instance ToJSON CoverageAnnotation
deriving anyclass instance FromJSON CoverageAnnotation
deriving anyclass instance ToJSONKey CoverageAnnotation
deriving anyclass instance FromJSONKey CoverageAnnotation

deriving anyclass instance ToJSON CovLoc
deriving anyclass instance FromJSON CovLoc

deriving anyclass instance ToJSON CoverageMetadata
deriving anyclass instance FromJSON CoverageMetadata

deriving anyclass instance ToJSON Metadata
deriving anyclass instance FromJSON Metadata
