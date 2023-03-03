{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Helpers.Tx where

import Cardano.Api (QueryConvenienceError)
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (isInfixOf)
import Data.Map qualified as Map
import GHC.Stack qualified as GHC
import Hedgehog (MonadTest)
import Hedgehog.Extras.Test qualified as HE
import Hedgehog.Extras.Test.Base qualified as H
import Helpers.Common (toEraInCardanoMode)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))

deriving instance Show QueryConvenienceError

-- | Check whether the auto-balancing txbody build (constructBalancedTx) resulted in an error
isTxBodyScriptExecutionError :: String -> Either C.TxBodyErrorAutoBalance r -> Bool
isTxBodyScriptExecutionError expectedError (Left (C.TxBodyScriptExecutionError m)) = expectedError `isInfixOf` (show m)
isTxBodyScriptExecutionError _ _                                                   = False

-- | Any CardanoEra to one that supports tokens. Used for building TxOut.
multiAssetSupportedInEra :: C.CardanoEra era -> C.MultiAssetSupportedInEra era
multiAssetSupportedInEra era = fromEither $ C.multiAssetSupportedInEra era
  where
    fromEither (Left _)  = error $ "Era must support MA"
    fromEither (Right m) = m

-- | Treat CardanoEra as ShelleyBased to satisfy constraint on constructBalancedTx.
withIsShelleyBasedEra :: C.CardanoEra era -> (C.IsShelleyBasedEra era => r) -> r
withIsShelleyBasedEra era r =
  case era of
    C.AlonzoEra  -> r
    C.BabbageEra -> r
    _            -> error "Must use Alonzo or Babbage era"

-- | Build TxOut for spending or minting with no datum or reference script present
txOut ::
  C.CardanoEra era ->
  C.Value ->
  C.Address C.ShelleyAddr ->
  C.TxOut C.CtxTx era
txOut era value address =
  C.TxOut
    (maybeAnyAddressInEra $ C.anyAddressInEra era $ C.toAddressAny address)
    (C.TxOutValue (multiAssetSupportedInEra era) value)
    C.TxOutDatumNone
    C.ReferenceScriptNone
  where
    maybeAnyAddressInEra Nothing    = error $ "Era must be ShelleyBased"
    maybeAnyAddressInEra (Just aie) = aie

-- | Build TxOut with a reference script
txOutWithRefScript ::
  C.CardanoEra era ->
  C.Value ->
  C.Address C.ShelleyAddr ->
  C.Script lang ->
  C.TxOut C.CtxTx era
txOutWithRefScript era value address script = withRefScript era script $ txOut era value address

txOutWithInlineDatum,
  txOutWithDatumHash,
  txOutWithDatumInTx ::
    C.CardanoEra era ->
    C.Value ->
    C.Address C.ShelleyAddr ->
    C.ScriptData ->
    C.TxOut C.CtxTx era
-- | Build TxOut with inline datum
txOutWithInlineDatum era value address datum = withInlineDatum era datum $ txOut era value address
-- | Build TxOut with datum hash
txOutWithDatumHash era value address datum = withDatumHash era datum $ txOut era value address
-- | Build TxOut with datum hash whilst including datum value in txbody
txOutWithDatumInTx era value address datum = withDatumInTx era datum $ txOut era value address

-- | Add reference script to TxOut
withRefScript ::
  C.CardanoEra era ->
  C.Script lang ->
  C.TxOut C.CtxTx era ->
  C.TxOut C.CtxTx era
withRefScript era script (C.TxOut e v d _) =
  C.TxOut e v d (C.ReferenceScript (refInsScriptsAndInlineDatsSupportedInEra era) (C.toScriptInAnyLang script))

withInlineDatum,
  withDatumHash,
  withDatumInTx ::
    C.CardanoEra era ->
    C.ScriptData ->
    C.TxOut C.CtxTx era ->
    C.TxOut C.CtxTx era
-- | Add inline datum to TxOut
withInlineDatum era datum (C.TxOut e v _ rs) =
  C.TxOut e v (C.TxOutDatumInline (refInsScriptsAndInlineDatsSupportedInEra era) datum) rs
-- | Add datum hash to TxOut
withDatumHash era datum (C.TxOut e v _ rs) =
  C.TxOut e v (C.TxOutDatumHash (scriptDataSupportedInEra era) (C.hashScriptData datum)) rs
-- | Add datum hash to TxOut whilst including datum value in txbody
withDatumInTx era datum (C.TxOut e v _ rs) =
  C.TxOut e v (C.TxOutDatumInTx (scriptDataSupportedInEra era) datum) rs

refInsScriptsAndInlineDatsSupportedInEra :: C.CardanoEra era -> C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
refInsScriptsAndInlineDatsSupportedInEra = fromMaybe . C.refInsScriptsAndInlineDatsSupportedInEra
  where
    fromMaybe Nothing  = error "Era must support reference inputs"
    fromMaybe (Just e) = e

scriptDataSupportedInEra :: C.CardanoEra era -> C.ScriptDataSupportedInEra era
scriptDataSupportedInEra = fromMaybe . C.scriptDataSupportedInEra
  where
    fromMaybe Nothing  = error "Era must support script data"
    fromMaybe (Just e) = e



-- | Empty transaction body to begin building from.
emptyTxBodyContent :: C.CardanoEra era -> C.ProtocolParameters -> C.TxBodyContent C.BuildTx era
emptyTxBodyContent era pparams =
  C.TxBodyContent
    { C.txIns = [],
      C.txInsCollateral = C.TxInsCollateralNone,
      C.txInsReference = C.TxInsReferenceNone,
      C.txOuts = [],
      C.txTotalCollateral = C.TxTotalCollateralNone,
      C.txReturnCollateral = C.TxReturnCollateralNone,
      C.txFee = C.TxFeeExplicit (fromTxFeesExplicit $ C.txFeesExplicitInEra era) 0,
      C.txValidityRange =
        ( C.TxValidityNoLowerBound,
          C.TxValidityNoUpperBound $
              fromNoUpperBoundMaybe $ C.validityNoUpperBoundSupportedInEra era
        ),
      C.txMetadata = C.TxMetadataNone,
      C.txAuxScripts = C.TxAuxScriptsNone,
      C.txExtraKeyWits = C.TxExtraKeyWitnessesNone,
      C.txProtocolParams = C.BuildTxWith $ Just pparams,
      C.txWithdrawals = C.TxWithdrawalsNone,
      C.txCertificates = C.TxCertificatesNone,
      C.txUpdateProposal = C.TxUpdateProposalNone,
      C.txMintValue = C.TxMintNone,
      C.txScriptValidity = C.TxScriptValidityNone
    }
  where
    fromNoUpperBoundMaybe Nothing    = error "Era must support no upper bound"
    fromNoUpperBoundMaybe (Just nub) = nub

txFee :: C.CardanoEra era -> C.Lovelace -> C.TxFee era
txFee era = C.TxFeeExplicit (fromTxFeesExplicit $ C.txFeesExplicitInEra era)

fromTxFeesExplicit :: Either imp exp -> exp
fromTxFeesExplicit (Left _)    = error "Era must support explicit fees"
fromTxFeesExplicit (Right tfe) = tfe

txExtraKeyWits :: C.CardanoEra era -> [C.VerificationKey C.PaymentKey] -> C.TxExtraKeyWitnesses era
txExtraKeyWits era pk = case C.extraKeyWitnessesSupportedInEra era of
    Nothing        -> error "era supporting extra key witnesses only"
    Just supported -> C.TxExtraKeyWitnesses supported $ C.verificationKeyHash <$> pk

-- | Produce collateral inputs if era supports it. Used for building txbody.
txInsCollateral :: C.CardanoEra era -> [C.TxIn] -> C.TxInsCollateral era
txInsCollateral era txIns = case C.collateralSupportedInEra era of
  Nothing        -> error "era supporting collateral only"
  Just supported -> C.TxInsCollateral supported txIns

txValidityRange :: C.CardanoEra era -> C.SlotNo -> C.SlotNo -> (C.TxValidityLowerBound era, C.TxValidityUpperBound era)
txValidityRange era lowerSlot upperSlot =
  (C.TxValidityLowerBound validityLowerBoundSupportedInEra lowerSlot,
   C.TxValidityUpperBound validityUpperBoundSupportedInEra upperSlot)
  where
    validityLowerBoundSupportedInEra = case C.validityLowerBoundSupportedInEra era of
        Nothing   -> error "era must support lower bound"
        (Just lb) -> lb
    validityUpperBoundSupportedInEra = case C.validityUpperBoundSupportedInEra era of
        Nothing   -> error "era must support upper bound"
        (Just ub) -> ub

-- | Get TxId from a signed transaction.
--  Useful for producing TxIn for building subsequant transaction.
txId :: C.Tx era -> C.TxId
txId = C.getTxId . C.getTxBody

-- | Build TxIn from TxId and index. Useful for waiting for or asserting expected TxOut is
--   onchain after submitting transaction.
txIn :: C.TxId -> Int -> C.TxIn
txIn txId txIx = C.TxIn txId (C.TxIx $ fromIntegral txIx)

pubkeyTxIns  :: [C.TxIn] -> [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))]
pubkeyTxIns = map (\txIn -> txInWitness txIn $ C.KeyWitness C.KeyWitnessForSpending)

txInWitness :: C.TxIn -> (C.Witness C.WitCtxTxIn era) -> (C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn era))
txInWitness txIn wit = (txIn, C.BuildTxWith wit)

txInsReference ::
  C.CardanoEra era ->
  [C.TxIn] ->
  C.TxInsReference build era
txInsReference era txIns = case era of
  C.BabbageEra -> C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra txIns

txMintValue ::
  C.CardanoEra era ->
  C.Value ->
  Map.Map C.PolicyId (C.ScriptWitness C.WitCtxMint era) ->
  C.TxMintValue C.BuildTx era
txMintValue era tv m = C.TxMintValue (multiAssetSupportedInEra era) tv (C.BuildTxWith m)

buildTx ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.TxBodyContent C.BuildTx era ->
  C.Address C.ShelleyAddr ->
  C.SigningKey C.PaymentKey ->
  C.NetworkId ->
  m (C.Tx era)
buildTx era txBody changeAddress sKey networkId = do
  eitherTx <- buildTx' era txBody changeAddress sKey networkId
  return $ fromEither eitherTx
  where
    fromEither (Left e)   = error $ show e
    fromEither (Right tx) = tx

-- | Maybe build signed transaction using convenience functions for calculating fees and exunits.
--   Useful for asserting for error.
buildTx' ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.TxBodyContent C.BuildTx era ->
  C.Address C.ShelleyAddr ->
  C.SigningKey C.PaymentKey ->
  C.NetworkId ->
  m (Either C.TxBodyErrorAutoBalance (C.Tx era))
buildTx' era txBody changeAddress sKey networkId = do
  (nodeEraUtxo, pparams, eraHistory, systemStart, stakePools) <-
    H.leftFailM . liftIO $
      C.queryStateForBalancedTx era networkId allInputs

  return $
    withIsShelleyBasedEra era $
      C.constructBalancedTx
        (toEraInCardanoMode era)
        txBody
        (C.shelleyAddressInEra changeAddress)
        Nothing -- Override key witnesses
        nodeEraUtxo -- tx inputs
        pparams
        eraHistory
        systemStart
        stakePools
        [C.WitnessPaymentKey sKey]
  where
    allInputs :: [C.TxIn]
    allInputs = do
      let txIns = (fst <$> C.txIns txBody)
      case C.txInsReference txBody of
        C.TxInsReferenceNone        -> txIns
        C.TxInsReference _ refTxIns -> txIns ++ refTxIns

-- | Build txbody with no calculated change, fees or execution unit
buildRawTx :: (MonadTest m) =>
  C.CardanoEra era ->
  C.TxBodyContent C.BuildTx era ->
  m (C.TxBody era)
buildRawTx era = withIsShelleyBasedEra era $ HE.leftFail . C.makeTransactionBody -- TODO: handle error

-- | Witness txbody with signing key when not using convenience build function
signTx :: (MonadIO m) =>
  C.CardanoEra  era ->
  C.TxBody era ->
  C.SigningKey C.PaymentKey ->
  m (C.KeyWitness era)
signTx era txbody skey = return $
  withIsShelleyBasedEra era $ C.makeShelleyKeyWitness txbody (C.WitnessPaymentKey skey)

submitTx ::
  (MonadIO m, MonadTest m) =>
  C.CardanoEra era ->
  C.LocalNodeConnectInfo C.CardanoMode ->
  C.Tx era ->
  m ()
submitTx era localNodeConnectInfo tx = do
  submitResult :: SubmitResult (C.TxValidationErrorInMode C.CardanoMode) <-
    liftIO $ C.submitTxToNodeLocal localNodeConnectInfo $ C.TxInMode tx (toEraInCardanoMode era)
  failOnTxSubmitFail submitResult
  where
    failOnTxSubmitFail :: (Show a, MonadTest m) => SubmitResult a -> m ()
    failOnTxSubmitFail = \case
      SubmitFail reason -> H.failMessage GHC.callStack $ "Transaction failed: " <> show reason
      SubmitSuccess     -> pure ()
