{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module ContractExample.IntegrationTest(run) where

import           Data.Aeson              (FromJSON, ToJSON)
import qualified Data.Map                as Map
import           GHC.Generics            (Generic)
import qualified Ledger.Ada              as Ada
import qualified Ledger.Constraints      as Constraints
import           Ledger.Scripts          (Datum (..), Redeemer (..), unitRedeemer)
import           Ledger.Tx               (getCardanoTxId)
import           Ledger.Typed.Scripts    as Scripts
import           Plutus.Contract
import qualified Plutus.Contracts.PubKey as PubKey
import           Prelude                 as Haskell

data IError =
    PKError PubKey.PubKeyError
    | CError ContractError
    deriving stock (Eq, Haskell.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

run :: Contract () EmptySchema IError ()
run = do
    logInfo @Haskell.String "Starting integration test"
    pkh <- mapError CError ownPubKeyHash
    (txOutRef, ciTxOut, pkInst) <- mapError PKError (PubKey.pubKeyContract pkh (Ada.adaValueOf 10))
    logInfo @Haskell.String "pubKey contract complete:"
    logInfo txOutRef
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
            logWarn err
        Right redeemingTx -> do
            let txi = getCardanoTxId redeemingTx
            logInfo @Haskell.String $ "Waiting for tx " <> show txi <> " to complete"
            mapError CError $ awaitTxConfirmed txi
            logInfo @Haskell.String "Tx confirmed"
