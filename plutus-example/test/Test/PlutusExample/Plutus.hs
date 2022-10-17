{-# LANGUAGE TemplateHaskell #-}

module Test.PlutusExample.Plutus where

import Cardano.Prelude

import Cardano.Api
import Cardano.Api.Shelley


import Hedgehog (Property, forAll, property, (===))

import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import PlutusExample.ScriptContextChecker

import Gen.Cardano.Api.Typed

prop_TxId_Api_Ledger_Plutus_Roundtrip :: Property
prop_TxId_Api_Ledger_Plutus_Roundtrip =
  property $ do
    -- Api <-> ledger round trip

    (ShelleyTxBody _ txBody _ _ _ _) <- forAll $ genTxBody AlonzoEra
    let apiTxId = getTxIdShelley ShelleyBasedEraAlonzo txBody
        ledgerTxId = toShelleyTxId apiTxId
        roundTripped = fromShelleyTxId ledgerTxId
    apiTxId === roundTripped

    -- Plutus <-> ledger roundtrip
    let plutusTxId = Alonzo.txInfoId ledgerTxId
        rtLedgerTxId = fromPlutusTxId plutusTxId
        rtPlutusTxIf = Alonzo.txInfoId rtLedgerTxId
    rtLedgerTxId === ledgerTxId
    plutusTxId === rtPlutusTxIf



prop_TxId_Api_Ledger_Roundtrip :: Property
prop_TxId_Api_Ledger_Roundtrip =
  property $ do
    (ShelleyTxBody _ txBody _ _ _ _) <- forAll $ genTxBody AlonzoEra
    let apiTxId = getTxIdShelley ShelleyBasedEraAlonzo txBody
        ledgerTxId = toShelleyTxId apiTxId
        roundTripped = fromShelleyTxId ledgerTxId
    apiTxId === roundTripped
