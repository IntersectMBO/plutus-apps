{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Cardano.Wallet.RemoteClientSpec
    ( tests
    ) where

import Cardano.Node.Emulator.Generators qualified as Gen
import Cardano.Node.Emulator.Internal.Node (Params (..), SlotConfig, pParamsFromProtocolParams, pProtocolParams)
import Cardano.Wallet.RemoteClient (handleWalletClient)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Freer (Eff, interpret, runM, type (~>))
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default (def))
import Data.List qualified as List
import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog (Property, (===))
import Hedgehog qualified
import Ledger (Slot)
import Ledger.Tx.Constraints.OffChain (emptyUnbalancedTx)
import Plutus.Contract (WalletAPIError)
import Plutus.PAB.Core.ContractInstance.STM (InstancesState, emptyInstanceState, emptyInstancesState, insertInstance,
                                             instanceState, yieldedExportTxs)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)
import Wallet.Effects (NodeClientEffect (GetClientParams, GetClientSlot, PublishTx), WalletEffect (YieldUnbalancedTx))
import Wallet.Emulator.Error (WalletAPIError (OtherError))
import Wallet.Types (ContractInstanceId, randomID)

tests :: TestTree
tests = testGroup "Cardano.Wallet.RemoteClient"
    [ testGroup "yieldUnbalancedTx"
        [ testPropertyNamed "should put partial tx in contract instance state" "yieldToInstanceState" yieldToInstanceState
        , testPropertyNamed "should throw error when no contract instance id is provided" "yieldNoCid"yieldNoCid
        ]
    ]

-- | Verify that a yielded unbalanced (partial) tx will appear in the contract's
-- instance status.
yieldToInstanceState :: Property
yieldToInstanceState = Hedgehog.property $ do
    pp <- Hedgehog.forAll Gen.genProtocolParameters
    sc <- Hedgehog.forAll Gen.genSlotConfig
    let params = def { emulatorPParams = pParamsFromProtocolParams pp, pSlotConfig = sc }
    sl <- Hedgehog.forAll Gen.genSlot
    cid <- liftIO randomID

    let utx = emptyUnbalancedTx params
    result <- liftIO $ do
      iss <- emptyInstancesState
      is <- STM.atomically $ emptyInstanceState
      insertInstance cid is iss
      yieldedRes <- runRemoteWalletEffects params sl iss (Just cid) (YieldUnbalancedTx utx)
      pure $ fmap (,iss) yieldedRes

    case result of
      Left _ -> Hedgehog.assert False
      Right ((), iss) -> do
        result <- liftIO $ instanceState cid iss >>= traverse (STM.atomically . yieldedExportTxs)
        maybe (Hedgehog.assert False) (\txs -> List.length txs === 1) result

-- | An error should be thrown when no contract instance id is provided.
yieldNoCid :: Property
yieldNoCid = Hedgehog.property $ do
    pp <- Hedgehog.forAll Gen.genProtocolParameters
    sc <- Hedgehog.forAll Gen.genSlotConfig
    let params = def { emulatorPParams = pParamsFromProtocolParams pp, pSlotConfig = sc }
    sl <- Hedgehog.forAll Gen.genSlot
    result <- liftIO $ do
        iss <- emptyInstancesState
        runRemoteWalletEffects params sl iss Nothing (YieldUnbalancedTx $ emptyUnbalancedTx params)
    case result of
      Left (OtherError _) -> Hedgehog.assert True
      _                   -> Hedgehog.assert False

-- | Run the wallet effects in a remote wallet scenario.
runRemoteWalletEffects
    :: Params
    -> Slot
    -> InstancesState
    -> Maybe ContractInstanceId
    -> WalletEffect ()
    -> IO (Either WalletAPIError ())
runRemoteWalletEffects params slot is cidM action = do
    runM
        $ runError @WalletAPIError
        $ runReader (pProtocolParams params)
        $ runReader is
        $ interpret (handleNodeClient params slot)
        $ handleWalletClient cidM action

-- | Handle NodeClientEffect for testing purposes.
handleNodeClient :: Params -> Slot -> NodeClientEffect ~> Eff effs
handleNodeClient params slot  = pure . \case
    PublishTx _     -> ()  -- Do nothing
    GetClientSlot   -> slot
    GetClientParams -> params
