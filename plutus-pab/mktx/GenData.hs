{-
  Generate some test inputs for the mktx program by running some simulator
  steps
 -}

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE FlexibleContexts   #-}

module Main where

import Plutus.Contract.Wallet (ExportTx, export)
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson as Aeson
import Ledger.Constraints.OffChain (ScriptLookups (..), mkTx)
import Ledger.Constraints.TxConstraints (UntypedConstraints
                                        , TxConstraints(..)
                                        , TxConstraint(..)
                                        , InputConstraint(..)
                                        , OutputConstraint(..))
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.Word (Word32)
import Ledger.Typed.TypeUtils (Any)
import System.Exit (die)
import System.Environment (getArgs)
import Plutus.V1.Ledger.Ada qualified as Ada

import Control.Concurrent.STM.Extras.Stream (readN, readOne)
import Control.Lens ((&), (+~), (^.))
import Control.Monad (replicateM, replicateM_, unless, void)
import Control.Monad.Freer.Extras.Log qualified as EmulatorLog
import Control.Monad.Freer.Extras.State (use)
import Control.Monad.Freer.State (State)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson qualified as JSON
import Data.Foldable (fold, traverse_)

import Control.Concurrent.STM qualified as STM
import Data.Aeson.Types qualified as JSON
import Data.Either (isRight)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Monoid qualified as M
import Data.Proxy (Proxy (Proxy))
import Data.Semigroup (Last (Last))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Ledger (PubKeyHash, getCardanoTxId, getCardanoTxOutRefs, pubKeyAddress, pubKeyHash, pubKeyHashAddress,
               toPubKeyHash, txId, txOutAddress, txOutRefId, txOutRefs, txOutputs, unitRedeemer)
import Ledger qualified
import Ledger.Ada (adaSymbol, adaToken, lovelaceValueOf)
import Ledger.Ada qualified as Ada
import Ledger.AddressMap qualified as AM
import Ledger.CardanoWallet qualified as CW
import Ledger.Value (valueOf)
import Plutus.ChainIndex (Depth (Depth), RollbackState (Committed, TentativelyConfirmed, Unknown),
                          TxOutState (Spent, Unspent), TxValidity (TxValid), chainConstant)
import Plutus.Contract.State (ContractResponse (ContractResponse, hooks))
import Plutus.Contracts.Currency (OneShotCurrency, SimpleMPS (SimpleMPS, amount, tokenName))
import Plutus.Contracts.GameStateMachine qualified as Contracts.GameStateMachine
import Plutus.Contracts.PingPong (PingPongState (Pinged, Ponged))
import Plutus.PAB.Core qualified as Core
import Plutus.PAB.Core.ContractInstance (ContractInstanceMsg)
import Plutus.PAB.Core.ContractInstance.STM (BlockchainEnv)
import Plutus.PAB.Core.ContractInstance.STM qualified as STM
import Plutus.PAB.Effects.Contract (ContractEffect, serialisableState)
import Plutus.PAB.Effects.Contract.Builtin (Builtin)
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Simulator (Simulation, TxCounts)
import Plutus.PAB.Simulator qualified as Simulator
import Plutus.PAB.Webserver.WebSocket qualified as WS
import PlutusTx.Monoid (Group (inv))
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Wallet.API (WalletAPIError, ownPubKeyHash)
import Wallet.API qualified as WAPI
import Wallet.Emulator.Chain qualified as Chain
import Wallet.Emulator.Wallet (Wallet, knownWallet, knownWallets)
import Wallet.Emulator.Wallet qualified as Wallet
import Wallet.Rollup (doAnnotateBlockchain)
import Wallet.Rollup.Types (DereferencedInput, dereferencedInputs, isFound)
import Wallet.Types (ContractInstanceId)

import Control.Monad.Freer
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.Row
import GHC.Generics (Generic)
import Prettyprinter

import ContractExample.AtomicSwap qualified as Contracts.AtomicSwap
import ContractExample.PayToWallet qualified as Contracts.PayToWallet
import Data.Text.Extras (tshow)
import Playground.Types (FunctionSchema)
import Plutus.Contract (awaitPromise)
import Plutus.Contracts.Currency qualified as Contracts.Currency
import Plutus.Contracts.GameStateMachine qualified as Contracts.GameStateMachine
import Plutus.Contracts.PingPong qualified as Contracts.PingPong
import Plutus.PAB.Effects.Contract (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler, HasDefinitions (..), SomeBuiltin (..))
import Plutus.PAB.Effects.Contract.Builtin qualified as Builtin
import Plutus.PAB.Monitoring.PABLogMsg (PABMultiAgentMsg)
import Plutus.PAB.Types (PABError (..))
import Schema (FormSchema)

import Control.Monad.Freer (interpret)
import Data.Default (Default (def))
import Ledger.TimeSlot (SlotConfig (..))
import Plutus.PAB.Core (EffectHandlers)
import Plutus.PAB.Effects.Contract.Builtin (Builtin, BuiltinHandler (contractHandler), handleBuiltin)
import Plutus.PAB.Simulator (Simulation, SimulatorContractHandler, SimulatorState, mkSimulatorHandlers,
                             runSimulationWith)
import Plutus.PAB.Types ( PABError(OtherError), chainOverviewBlockchain, mkChainOverview, PABError(..), PABError )
import PlutusTx qualified


main :: IO ()
main = writeTestOutput

-- | Run the PAB simulator with the test contracts
runSimulation :: Simulation (Builtin TestContracts) a -> IO (Either PABError a)
runSimulation = runSimulationWith simulatorHandlers

-- | 'EffectHandlers' for running the PAB as a simulator (no connectivity to
--   out-of-process services such as wallet backend, node, etc.)
simulatorHandlers :: Core.EffectHandlers (Builtin TestContracts) (Simulator.SimulatorState (Builtin TestContracts))
simulatorHandlers = mkSimulatorHandlers def def { scSlotLength = 1 } handler where
    handler :: Simulator.SimulatorContractHandler (Builtin TestContracts)
    handler = interpret (contractHandler handleBuiltin)


data TestContracts = GameStateMachine | Currency | AtomicSwap | PayToWallet | PingPong
    deriving (Eq, Ord, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Pretty TestContracts where
    pretty = viaShow

instance HasDefinitions TestContracts where
    getDefinitions = [ GameStateMachine, Currency, AtomicSwap, PayToWallet, PingPong ]
    getContract = getTestContracts
    getSchema = getTestContractsSchema

getTestContractsSchema :: TestContracts -> [FunctionSchema FormSchema]
getTestContractsSchema = \case
    GameStateMachine -> Builtin.endpointsToSchemas @Contracts.GameStateMachine.GameStateMachineSchema
    Currency         -> Builtin.endpointsToSchemas @Contracts.Currency.CurrencySchema
    AtomicSwap       -> Builtin.endpointsToSchemas @Contracts.AtomicSwap.AtomicSwapSchema
    PayToWallet      -> Builtin.endpointsToSchemas @Contracts.PayToWallet.PayToWalletSchema
    PingPong         -> Builtin.endpointsToSchemas @Contracts.PingPong.PingPongSchema

getTestContracts :: TestContracts -> SomeBuiltin
getTestContracts = \case
    GameStateMachine -> SomeBuiltin game
    Currency         -> SomeBuiltin $ awaitPromise currency
    AtomicSwap       -> SomeBuiltin $ awaitPromise swp
    PayToWallet      -> SomeBuiltin $ awaitPromise payToWallet
    PingPong         -> SomeBuiltin pingPong
    where
        game = Contracts.GameStateMachine.contract
        currency = Contracts.Currency.mintCurrency
        swp = first tshow Contracts.AtomicSwap.atomicSwap
        payToWallet = Contracts.PayToWallet.payToWallet
        pingPong = Contracts.PingPong.combined


runScenario :: Simulation (Builtin TestContracts) a -> IO ()
runScenario sim = do
    result <- runSimulation sim
    case result of
        Left err -> error (show err)
        Right _  -> pure ()

defaultWallet :: Wallet
defaultWallet = knownWallet 1

defaultWalletPubKeyHash :: PubKeyHash
defaultWalletPubKeyHash = CW.pubKeyHash (CW.fromWalletNumber $ CW.WalletNumber 1)

writeTestOutput :: IO ()
writeTestOutput = runScenario $ do
  liftIO $ putStrLn "hello"
  (w1, pk1) <- Simulator.addWallet
  Simulator.waitNSlots 1
  (w2, pk2) <- Simulator.addWallet
  Simulator.waitNSlots 1

  tx <- Simulator.payToPublicKeyHash w1 pk2 (lovelaceValueOf 100_000_000)
  -- We should have 2 UTxOs present.
  -- We find the 'TxOutRef' from wallet 1
  let txOutRef1 = head $ fmap snd $ filter (\(txOut, txOutref) -> toPubKeyHash (txOutAddress txOut) == Just pk1) $ getCardanoTxOutRefs tx
  -- We find the 'TxOutRef' from wallet 2
  let txOutRef2 = head $ fmap snd $ filter (\(txOut, txOutref) -> toPubKeyHash (txOutAddress txOut) == Just pk2) $ getCardanoTxOutRefs tx
  txOutStatus1 <- Simulator.waitForTxOutStatusChange txOutRef1

  let testTxConstraints :: UntypedConstraints
      testTxConstraints = TxConstraints
          { txConstraints = [ MustIncludeDatum Ledger.unitDatum
                            -- , MustValidateIn POSIXTimeRange
                            , MustBeSignedBy pk1
                            , MustSpendAtLeast (Ada.toValue 123)
                            , MustProduceAtLeast (Ada.toValue 234)
                            , MustSpendPubKeyOutput txOutRef1
                            , MustSpendScriptOutput txOutRef2 Ledger.unitRedeemer
                            -- , MustMintValue MintingPolicyHash Redeemer TokenName Integer
                            , MustPayToPubKey pk2 (Ada.toValue 2345)
                            -- , MustPayToOtherScript ValidatorHash Ledger.unitDatum (Ada.toValue 1234)
                            -- , MustHashDatum DatumHash Ledger.unitDatum
                            , MustSatisfyAnyOf [ MustSpendAtLeast (Ada.toValue 345)
                                               , MustProduceAtLeast (Ada.toValue 456)
                                               ]
                            ]
          , txOwnInputs =   [ InputConstraint { icRedeemer = PlutusTx.toBuiltinData ()
                                              , icTxOutRef = txOutRef1
                                              }
                            , InputConstraint { icRedeemer = PlutusTx.toBuiltinData ()
                                              , icTxOutRef = txOutRef2 
                                              }
                            ]
          , txOwnOutputs =  [ OutputConstraint { ocDatum = PlutusTx.toBuiltinData ()
                                              , ocValue = Ada.toValue 12345
                                              }
                            , OutputConstraint { ocDatum = PlutusTx.toBuiltinData ()
                                              , ocValue = Ada.toValue 23456
                                              }
                            ]
          }

      -- TODO: fill with more interesting data
      testScriptLookups :: ScriptLookups Any
      testScriptLookups = ScriptLookups { slMPS            = mempty
                                        , slTxOutputs      = mempty
                                        , slOtherScripts   = mempty
                                        , slOtherData      = mempty
                                        , slPubKeyHashes   = mempty 
                                        , slTypedValidator = Nothing
                                        , slOwnPubkeyHash  = Just pk1
                                        }

  liftIO $ BSL.writeFile "scriptlookups.json" (Aeson.encode $ Aeson.toJSON testScriptLookups) 
  liftIO $ BSL.writeFile "txconstraints.json" (Aeson.encode $ Aeson.toJSON testTxConstraints) 



