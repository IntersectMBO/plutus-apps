{-# LANGUAGE GADTs #-}

module Test.PlutusExample.Gen where

import Cardano.Api
import Cardano.Api.Shelley
import Prelude

import Data.Map.Strict qualified as Map

import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import Cardano.Ledger.Crypto (StandardCrypto)
import Cardano.Ledger.Era qualified as Ledger
import Cardano.Ledger.Shelley.UTxO qualified as Ledger
import Cardano.Ledger.TxIn qualified as Ledger
import Gen.Cardano.Api.Typed
import Ledger qualified as Plutus
import Plutus.V1.Ledger.Tx qualified as PV1
import PlutusExample.PlutusVersion1.RedeemerContextScripts
import PlutusExample.ScriptContextChecker

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

genPlutusTxOut :: Gen PV1.TxOut
genPlutusTxOut = do
  alonzoTxOut <-
    TxOut <$> (shelleyAddressInEra <$> genAddressShelley)
          <*> genTxOutValue AlonzoEra
          <*> genTxOutDatumHashTxContext AlonzoEra
          <*> genReferenceScript AlonzoEra
  Gen.just . return . Alonzo.txInfoOut
    $ toShelleyTxOut ShelleyBasedEraAlonzo (toCtxUTxOTxOut alonzoTxOut)

genMyCustomRedeemer :: Gen AnyCustomRedeemer
genMyCustomRedeemer =
  AnyPV1CustomRedeemer
    <$> ( PV1CustomRedeemer
            <$> Gen.list (Range.singleton 1) genPlutusTxOut
            <*> return mempty --TODO: Investigate why genTxInfoIn generates Nothing
            <*> (Alonzo.transValue . toMaryValue <$> genValueForMinting)
            <*> genPOSIXTimeRange
            <*> (Alonzo.transValue . toMaryValue <$> genValueForTxOut)
            <*> genDatumMap
            <*> Gen.list (Range.constant 0 2) genPlutusCert
            <*> Gen.list (Range.constant 0 2) genReqSigners
            <*> return Nothing
        )

genTxInfoIn :: Gen Plutus.TxInInfo
genTxInfoIn = do
  txinput <- genTxIn
  txout <- genTxOutTxContext AlonzoEra
  lUTxO <- genLedgerUTxO ShelleyBasedEraAlonzo (txinput, txout)
  let eTxInfoIn = getTxInInfoFromTxIn lUTxO (toShelleyTxIn txinput)
  case eTxInfoIn of
    Nothing   -> error $ "Error: Cannot find the TxIn in the UTxO\n"
                      ++ "Utxo: "
                      ++ show lUTxO
                      ++ "\n"
                      ++ "Txin: "
                      ++ show txinput
    Just txin -> return txin

genReqSigners :: Gen Plutus.PubKeyHash
genReqSigners = do
  PaymentKeyHash kh <- genVerificationKeyHash AsPaymentKey
  return $ Alonzo.transKeyHash kh

genLedgerUTxO
  :: (Ledger.Crypto (ShelleyLedgerEra era) ~ StandardCrypto)
  => ShelleyBasedEra era
  -> (TxIn, TxOut CtxTx era)
  -> Gen (Ledger.UTxO (ShelleyLedgerEra era))
genLedgerUTxO sbe (txin, out) = do
  UTxO utxoMap <- genUTxO (shelleyBasedToCardanoEra sbe)
  return . toLedgerUTxO sbe . UTxO $ Map.insert txin (toCtxUTxOTxOut out) utxoMap

genPlutusCert :: Gen Plutus.DCert
genPlutusCert = Alonzo.transDCert . toShelleyCertificate <$> genCertificate

genLedgerTxIn :: Gen (Ledger.TxIn StandardCrypto)
genLedgerTxIn = toShelleyTxIn <$> genTxIn

genPlutusTxId :: Gen PV1.TxId
genPlutusTxId =
  Alonzo.txInfoId . toShelleyTxId <$> genTxId

genDatumMap :: Gen [(Plutus.DatumHash, Plutus.Datum)]
genDatumMap =
  map Alonzo.transDataPair <$> Gen.list (Range.linear 0 5) genDatumHashTuple

genDatumHashTuple :: Gen (Alonzo.DataHash StandardCrypto, Alonzo.Data ledgerera)
genDatumHashTuple = do
  sData <- genScriptData
  let ScriptDataHash h = hashScriptData sData
  return (h, toAlonzoData sData)

genPOSIXTimeRange :: Gen Plutus.POSIXTimeRange
genPOSIXTimeRange = do
  ptime <- Plutus.POSIXTime <$> Gen.integral (Range.linear 0 10)
  Gen.element [ Plutus.to ptime
              , Plutus.always
              , Plutus.never
              , Plutus.singleton ptime
              ]
