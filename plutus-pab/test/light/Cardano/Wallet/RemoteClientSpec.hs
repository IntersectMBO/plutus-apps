{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Wallet.RemoteClientSpec
    ( tests
    ) where

import Cardano.Api.ProtocolParameters (ProtocolParameters)
import Cardano.Wallet.RemoteClient (handleWalletClient)
import Control.Concurrent.STM qualified as STM
import Control.Monad.Freer (interpret, runM)
import Control.Monad.Freer.Error (runError)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (liftIO)
import Data.Default (Default (def))
import Data.List qualified as List
import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog (Property, (===))
import Hedgehog qualified
import Ledger.Constraints.OffChain (emptyUnbalancedTx)
import Plutus.Contract (WalletAPIError)
import Plutus.PAB.Core.ContractInstance.STM (InstancesState, emptyInstanceState, emptyInstancesState, insertInstance,
                                             instanceState, yieldedExportTxs)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)
import Wallet.Effects (WalletEffect (YieldUnbalancedTx))
import Wallet.Emulator.Error (WalletAPIError (OtherError))
import Wallet.Types (ContractInstanceId, randomID)

tests :: TestTree
tests = testGroup "Cardano.Wallet.RemoteClient"
    [ testGroup "yieldUnbalancedTx"
        [ testProperty "should put partial tx in contract instance state" yieldToInstanceState
        , testProperty "should throw error when no contract instance id is provided" yieldNoCid
        ]
    ]

-- | Verify that a yielded unbalanced (partial) tx will appear in the contract's
-- instance status.
yieldToInstanceState :: Property
yieldToInstanceState = Hedgehog.property $ do
    pp <- Hedgehog.forAll Gen.genProtocolParameters
    cid <- liftIO randomID

    let utx = emptyUnbalancedTx
    result <- liftIO $ do
        iss <- STM.atomically $ do
            iss <- emptyInstancesState
            is <- emptyInstanceState
            insertInstance cid is iss
            pure iss
        yieldedRes <- runRemoteWalletEffects pp iss (Just cid) (YieldUnbalancedTx utx)
        pure $ fmap (,iss) yieldedRes

    case result of
      Left _ -> Hedgehog.assert False
      Right ((), iss) -> do
          txs <- liftIO $ STM.atomically $ instanceState cid iss >>= yieldedExportTxs
          List.length txs === 1

-- | An error should be thrown when no contract instance id is provided.
yieldNoCid :: Property
yieldNoCid = Hedgehog.property $ do
    pp <- Hedgehog.forAll Gen.genProtocolParameters
    result <- liftIO $ do
        iss <- STM.atomically emptyInstancesState
        runRemoteWalletEffects pp iss Nothing (YieldUnbalancedTx emptyUnbalancedTx)
    case result of
      Left (OtherError _) -> Hedgehog.assert True
      _                   -> Hedgehog.assert False

-- | Run the wallet effects in a remote wallet scenario.
runRemoteWalletEffects
    :: ProtocolParameters
    -> InstancesState
    -> Maybe ContractInstanceId
    -> WalletEffect ()
    -> IO (Either WalletAPIError ())
runRemoteWalletEffects protocolParams is cidM action = do
    runM
        $ runError @WalletAPIError
        $ runReader protocolParams
        $ runReader is
        $ handleWalletClient def cidM action
