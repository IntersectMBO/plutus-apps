-- | Calculating transaction fees in the emulator.
module Ledger.Fee(
  evaluateTransactionFee,
  makeTransactionBodyAutoBalance
) where

import Cardano.Api.Shelley qualified as C.Api
import Cardano.Ledger.BaseTypes (Globals (systemStart))
import Data.Bifunctor (bimap, first)
import Data.Map qualified as Map
import Ledger.Address (Address, PaymentPubKeyHash)
import Ledger.Params (EmulatorEra, Params (pNetworkId, pProtocolParams), emulatorEraHistory, emulatorGlobals)
import Ledger.Tx (ToCardanoError (TxBodyError), Tx)
import Ledger.Tx.CardanoAPI (CardanoBuildTx (..), getCardanoBuildTx, toCardanoAddressInEra, toCardanoTxBodyContent)
import Ledger.Validation (CardanoLedgerError, UTxO (..), makeTransactionBody)
import Ledger.Value (Value)
import Plutus.V1.Ledger.Ada (lovelaceValueOf)

evaluateTransactionFee
  :: Params
  -> UTxO EmulatorEra
  -> [PaymentPubKeyHash]
  -> Tx
  -> Either CardanoLedgerError Value
evaluateTransactionFee params utxo requiredSigners tx = do
  txBodyContent <- first Right $ toCardanoTxBodyContent params requiredSigners tx
  let nkeys = C.Api.estimateTransactionKeyWitnessCount (getCardanoBuildTx txBodyContent)
  txBody <- makeTransactionBody params utxo txBodyContent
  case C.Api.evaluateTransactionFee (pProtocolParams params) txBody nkeys 0 of
    C.Api.Lovelace fee -> pure $ lovelaceValueOf fee


makeTransactionBodyAutoBalance
  :: Params
  -> UTxO EmulatorEra
  -> CardanoBuildTx
  -> Address
  -> Either CardanoLedgerError (C.Api.Tx C.Api.AlonzoEra)
makeTransactionBodyAutoBalance params utxo (CardanoBuildTx txBodyContent) pChangeAddr = first Right $ do
  cChangeAddr <- toCardanoAddressInEra (pNetworkId params) pChangeAddr
  -- Compute the change.
  C.Api.BalancedTxBody _ change _ <- first (TxBodyError . C.Api.displayError) $ balance cChangeAddr []
  let
    -- Recompute execution units with full set of UTxOs, including change.
    trial = balance cChangeAddr [change]
    -- Correct for a negative balance in cases where execution units, and hence fees, have increased.
    change' =
      case (change, trial) of
        (C.Api.TxOut addr (C.Api.TxOutValue vtype value) datum, Left (C.Api.TxBodyErrorAdaBalanceNegative delta)) ->
          C.Api.TxOut addr (C.Api.TxOutValue vtype $ value <> C.Api.lovelaceToValue delta) datum
        _ -> change
  -- Construct the body with correct execution units and fees.
  C.Api.BalancedTxBody txBody _ _ <- first (TxBodyError . C.Api.displayError) $ balance cChangeAddr [change']
  pure $ C.Api.makeSignedTransaction [] txBody
  where
    eh = emulatorEraHistory params
    ss = systemStart $ emulatorGlobals params
    utxo' = fromLedgerUTxO utxo
    balance cChangeAddr extraOuts = C.Api.makeTransactionBodyAutoBalance
      C.Api.AlonzoEraInCardanoMode
      ss
      eh
      (pProtocolParams params)
      mempty
      utxo'
      txBodyContent { C.Api.txOuts = C.Api.txOuts txBodyContent ++ extraOuts }
      cChangeAddr
      Nothing


fromLedgerUTxO :: UTxO EmulatorEra
               -> C.Api.UTxO C.Api.AlonzoEra
fromLedgerUTxO (UTxO utxo) =
    C.Api.UTxO
  . Map.fromList
  . map (bimap C.Api.fromShelleyTxIn (C.Api.fromShelleyTxOut C.Api.ShelleyBasedEraAlonzo))
  . Map.toList
  $ utxo
