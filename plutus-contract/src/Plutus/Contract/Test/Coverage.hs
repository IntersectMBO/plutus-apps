{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Plutus.Contract.Test.Coverage
  ( getInvokedEndpoints
  , getCoverageData
  , CoverageRef(..)
  , newCoverageRef
  , readCoverageRef
  , writeCoverageReport
  ) where

import Control.Lens
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text

import Plutus.Trace.Emulator.Types
import PlutusTx.Coverage
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
getCoverageData :: [EmulatorEvent] -> CoverageData
getCoverageData es =
  let extractLog e = case e of
        ChainEvent (TxnValidate _ _ logs)             -> logs
        ChainEvent (TxnValidationFail _ _ _ _ _ logs) -> logs
        _                                             -> []

  in fold $ do
    event <- es
    logEvent <- extractLog $ event ^. eteEvent
    let msg = Text.unpack logEvent
    return $ coverageDataFromLogMsg msg

newtype CoverageRef = CoverageRef (IORef CoverageData)

newCoverageRef :: IO CoverageRef
newCoverageRef = CoverageRef <$> newIORef mempty

readCoverageRef :: CoverageRef -> IO CoverageData
readCoverageRef (CoverageRef ioref) = readIORef ioref

-- | Write a coverage report to name.html for the given index.
writeCoverageReport :: String -> CoverageReport -> IO ()
writeCoverageReport = ReportCoverage.writeCoverageReport
