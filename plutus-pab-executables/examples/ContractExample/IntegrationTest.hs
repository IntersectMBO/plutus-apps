{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}

module ContractExample.IntegrationTest(run) where

import Control.Lens (makeClassyPrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Ledger.Ada qualified as Ada
import Ledger.Constraints qualified as Constraints
import Ledger.Scripts (Datum (..), Redeemer (..), unitRedeemer)
import Ledger.Tx (getCardanoTxId)
import Ledger.Typed.Scripts as Scripts
import Plutus.Contract
import Plutus.Contracts.PubKey qualified as PubKey
import Prelude as Haskell

data IError =
    PKError PubKey.PubKeyError
    | CError ContractError
    deriving stock (Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''IError

instance AsContractError IError where
    _ContractError = _CError

run :: Contract () EmptySchema IError ()
run = runError run' >>= \case
    Left err -> logWarn @Haskell.String (show err)
    Right () -> pure ()

run' :: Contract () EmptySchema IError ()
run' = do
    logInfo @Haskell.String "Starting integration test"
    pkh <- ownPaymentPubKeyHash
    (txOutRef, ciTxOut, pkInst) <- mapError PKError (PubKey.pubKeyContract pkh (Ada.adaValueOf 10))
    logInfo @Haskell.String "pubKey contract complete:"
    let lookups =
            Constraints.otherData (Datum $ getRedeemer unitRedeemer)
            <> Constraints.unspentOutputs (maybe mempty (Map.singleton txOutRef) ciTxOut)
            <> Constraints.otherScript  (Scripts.validatorScript pkInst)
        constraints =
            Constraints.mustSpendScriptOutput txOutRef unitRedeemer
            <> Constraints.mustBeSignedBy pkh
    result <- runError @_ @_ @ContractError $ submitTxConstraintsWith @Scripts.Any lookups constraints
    case result of
        Left err -> do
            logWarn @Haskell.String "An error occurred. Integration test failed."
            logWarn err
        Right redeemingTx -> do
            let txi = getCardanoTxId redeemingTx
            logInfo @Haskell.String $ "Waiting for tx " <> show txi <> " to complete"
            awaitTxConfirmed txi
            logInfo @Haskell.String "Tx confirmed. Integration test complete."
