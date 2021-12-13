{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Plutus.Contract.Test.Coverage
  ( getInvokedEndpoints
  , getCoverageReport
  ) where

import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Data.Text qualified as Text

import Control.Lens

import Ledger qualified

import PlutusTx.Coverage

import Plutus.Trace.Emulator.Types

import Wallet.Emulator.Chain
import Wallet.Emulator.MultiAgent (EmulatorEvent, EmulatorEvent' (..), EmulatorTimeEvent (..), eteEvent)
import Wallet.Types


-- | Get every endpoint name that has been invoced in the emulator events in `es`
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
        ChainEvent (TxnValidate _ _ valEvs)           -> logOf . Ledger.sveResult <$> valEvs
        ChainEvent (TxnValidationFail _ _ _ _ valEvs) -> logOf . Ledger.sveResult <$> valEvs
        _                                             -> []

      logOf (Left (Ledger.EvaluationError lg _)) = lg
      logOf (Left Ledger.EvaluationException{})  = []
      logOf (Left Ledger.MalformedScript{})      = []
      logOf (Right (_, lg))                      = lg

  in fold $ do
    event <- es
    log <- extractLog $ event ^. eteEvent
    logEvent <- log
    return $ coverageReportFromLogMsg (Text.unpack logEvent)
