{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE EmptyCase                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
{-# OPTIONS_GHC -Wno-missing-import-lists #-}
module Ledger.Tx.Constraints.OffChain(
    -- * Lookups
    P.ScriptLookups(..)
    , P.typedValidatorLookups
    , P.generalise
    , P.unspentOutputs
    , P.plutusV1MintingPolicy
    , P.plutusV2MintingPolicy
    , P.plutusV1OtherScript
    , P.plutusV2OtherScript
    , P.otherData
    , P.ownPaymentPubKeyHash
    , P.ownStakePubKeyHash
    , P.paymentPubKey
    -- * Constraints resolution
    , P.SomeLookupsAndConstraints(..)
    , UnbalancedTx(..)
    , unBalancedTxTx
    , tx
    , txValidityRange
    , txOuts
    , P.requiredSignatories
    , P.utxoIndex
    , P.validityTimeRange
    , emptyUnbalancedTx
    , P.adjustUnbalancedTx
    , MkTxError(..)
    , mkTx
    , mkSomeTx
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (Lens', Traversal', coerced, iso, makeLensesFor, use, (.=), (<>=))
import Control.Monad.Except (Except, MonadError, mapExcept, runExcept, throwError)
import Control.Monad.Reader (ReaderT (runReaderT), mapReaderT)
import Control.Monad.State (MonadState, StateT, execStateT, gets, mapStateT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Ledger (POSIXTimeRange, Params (..), networkIdL)
import Ledger.Address (pubKeyHashAddress, scriptValidatorHashAddress)
import Ledger.Constraints qualified as P
import Ledger.Constraints.OffChain (UnbalancedTx (..), cpsUnbalancedTx, unBalancedTxTx, unbalancedTx)
import Ledger.Constraints.OffChain qualified as P
import Ledger.Constraints.TxConstraints (ScriptOutputConstraint, TxConstraint,
                                         TxConstraints (TxConstraints, txConstraints, txOwnOutputs))
import Ledger.Interval ()
import Ledger.Orphans ()
import Ledger.Scripts (ScriptHash, getDatum, getRedeemer, getValidator)
import Ledger.TimeSlot (posixTimeRangeToContainedSlotRange)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (toCardanoScriptInAnyLang)
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (ValidatorTypes (DatumType, RedeemerType))
import PlutusTx (FromData, ToData)
import PlutusTx.Lattice (BoundedMeetSemiLattice (top), MeetSemiLattice ((/\)))
import Prettyprinter (Pretty (pretty), colon, (<+>))

makeLensesFor
    [ ("txIns", "txIns'")
    , ("txInsCollateral", "txInsCollateral'")
    , ("txInsReference", "txInsReference'")
    , ("txOuts", "txOuts'")
    , ("txValidityRange", "txValidityRange'")
    ] ''C.TxBodyContent

txIns :: Lens' C.CardanoBuildTx [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.BabbageEra))]
txIns = coerced . txIns'

txInsCollateral :: Lens' C.CardanoBuildTx [C.TxIn]
txInsCollateral = coerced . txInsCollateral' . iso toList fromList
    where
        toList C.TxInsCollateralNone       = []
        toList (C.TxInsCollateral _ txins) = txins
        fromList []    = C.TxInsCollateralNone
        fromList txins = C.TxInsCollateral C.CollateralInBabbageEra txins

txInsReference :: Lens' C.CardanoBuildTx [C.TxIn]
txInsReference = coerced . txInsReference' . iso toList fromList
    where
        toList C.TxInsReferenceNone       = []
        toList (C.TxInsReference _ txins) = txins
        fromList []    = C.TxInsReferenceNone
        fromList txins = C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra txins

txOuts :: Lens' C.CardanoBuildTx [C.TxOut C.CtxTx C.BabbageEra]
txOuts = coerced . txOuts'

txValidityRange :: Lens' C.CardanoBuildTx (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
txValidityRange = coerced . txValidityRange'

tx :: Traversal' UnbalancedTx C.CardanoBuildTx
tx = P.cardanoTx

emptyCardanoBuildTx :: Params -> C.CardanoBuildTx
emptyCardanoBuildTx Params { pProtocolParams }= C.CardanoBuildTx $ C.TxBodyContent
    { C.txIns = mempty
    , C.txInsCollateral = C.TxInsCollateral C.CollateralInBabbageEra mempty
    , C.txInsReference = C.TxInsReferenceNone
    , C.txOuts = mempty
    , C.txTotalCollateral = C.TxTotalCollateralNone
    , C.txReturnCollateral = C.TxReturnCollateralNone
    , C.txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra mempty
    , C.txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
    , C.txMintValue = C.TxMintNone
    , C.txProtocolParams = C.BuildTxWith $ Just pProtocolParams
    , C.txScriptValidity = C.TxScriptValidityNone
    , C.txExtraKeyWits = C.TxExtraKeyWitnessesNone
    , C.txMetadata = C.TxMetadataNone
    , C.txAuxScripts = C.TxAuxScriptsNone
    , C.txWithdrawals = C.TxWithdrawalsNone
    , C.txCertificates = C.TxCertificatesNone
    , C.txUpdateProposal = C.TxUpdateProposalNone
    }

emptyUnbalancedTx :: Params -> UnbalancedTx
emptyUnbalancedTx params = UnbalancedCardanoTx (emptyCardanoBuildTx params) mempty mempty

initialState :: Params -> P.ConstraintProcessingState
initialState params = P.ConstraintProcessingState
    { P.cpsUnbalancedTx = emptyUnbalancedTx params
    , P.cpsMintRedeemers = mempty
    , P.cpsValueSpentBalancesInputs = P.ValueSpentBalances mempty mempty
    , P.cpsValueSpentBalancesOutputs = P.ValueSpentBalances mempty mempty
    , P.cpsParams = params
    }

data MkTxError
    = ToCardanoError C.ToCardanoError
    | LedgerMkTxError P.MkTxError
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty MkTxError where
    pretty = \case
        ToCardanoError err  -> "ToCardanoError" <> colon <+> pretty err
        LedgerMkTxError err -> pretty err

-- | Given a list of 'SomeLookupsAndConstraints' describing the constraints
--   for several scripts, build a single transaction that runs all the scripts.
mkSomeTx
    :: Params
    -> [P.SomeLookupsAndConstraints]
    -> Either MkTxError UnbalancedTx
mkSomeTx params xs =
    let process = \case
            P.SomeLookupsAndConstraints lookups constraints ->
                processLookupsAndConstraints lookups constraints
    in  fmap cpsUnbalancedTx
        $ runExcept
        $ execStateT (traverse process xs) (initialState params)

-- | Resolve some 'TxConstraints' by modifying the 'UnbalancedTx' in the
--   'ConstraintProcessingState'
processLookupsAndConstraints
    :: ( -- FromData (DatumType a)
         ToData (DatumType a)
    --    , ToData (RedeemerType a)
       )
    => P.ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> StateT P.ConstraintProcessingState (Except MkTxError) ()
processLookupsAndConstraints lookups TxConstraints{txConstraints, txOwnOutputs} =
    let
      extractPosixTimeRange = \case
        P.MustValidateIn range -> Left range
        other                  -> Right other
      (ranges, otherConstraints) = partitionEithers $ extractPosixTimeRange <$> txConstraints
    in do
        flip runReaderT lookups $ do
            ownOutputConstraints <- traverse addOwnOutput txOwnOutputs
            traverse_ processConstraint (otherConstraints <> ownOutputConstraints)
            -- traverse_ P.processConstraintFun txCnsFuns
            -- traverse_ P.addOwnInput txOwnInputs
            -- P.addMintingRedeemers
            -- P.addMissingValueSpent
            P.updateUtxoIndex
        setValidityRange ranges

-- | Reinject the validityRange inside the unbalanced Tx.
--   As the Tx is a Caradano transaction, and as we have access to the SlotConfig,
--   we can already internalize the constraints for the test
setValidityRange
    :: [POSIXTimeRange] -> StateT P.ConstraintProcessingState (Except MkTxError) ()
setValidityRange ranges = do
  slotConfig <- gets (pSlotConfig . P.cpsParams)
  let slotRange = foldl (/\) top $ posixTimeRangeToContainedSlotRange slotConfig <$> ranges
  cTxTR <- throwLeft ToCardanoError $ C.toCardanoValidityRange slotRange
  unbalancedTx . tx . txValidityRange .= cTxTR

-- | Turn a 'TxConstraints' value into an unbalanced transaction that satisfies
--   the constraints. To use this in a contract, see
--   'Plutus.Contract.submitTxConstraints'
--   and related functions.
mkTx
    :: ( FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       )
    => Params
    -> P.ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> Either MkTxError UnbalancedTx
mkTx params lookups txc = mkSomeTx params [P.SomeLookupsAndConstraints lookups txc]

throwLeft :: (MonadState s m, MonadError err m) => (b -> err) -> Either b r -> m r
throwLeft f = either (throwError . f) pure

-- | Modify the 'UnbalancedTx' so that it satisfies the constraints, if
--   possible. Fails if a hash is missing from the lookups, or if an output
--   of the wrong type is spent.
processConstraint
    :: TxConstraint
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) ()
processConstraint = \case
    P.MustSpendPubKeyOutput txo -> do
        txout <- lookupTxOutRef txo
        case txout of
          Tx.PublicKeyChainIndexTxOut {} -> do
              txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
              unbalancedTx . tx . txIns <>= [(txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))]
          _ -> throwError (LedgerMkTxError $ P.TxOutRefWrongType txo)

    P.MustSpendScriptOutput txo redeemer -> do
        txout <- lookupTxOutRef txo
        mscriptTXO <- mapReaderT (mapStateT (mapExcept (first LedgerMkTxError))) $ P.resolveScriptTxOut txout
        case mscriptTXO of
            Just ((_, validator, lang), (_, datum), _) -> do
                txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
                witness <-
                    throwLeft ToCardanoError $ C.ScriptWitness C.ScriptWitnessForSpending <$>
                    case lang of
                        Tx.PlutusV1 ->
                            C.PlutusScriptWitness C.PlutusScriptV1InBabbage C.PlutusScriptV1
                                <$> fmap C.PScript (C.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV1) (getValidator validator))
                                <*> pure (C.ScriptDatumForTxIn $ C.toCardanoScriptData (getDatum datum))
                                <*> pure (C.toCardanoScriptData (getRedeemer redeemer))
                                <*> pure C.zeroExecutionUnits
                        Tx.PlutusV2 ->
                            C.PlutusScriptWitness C.PlutusScriptV2InBabbage C.PlutusScriptV2
                                <$> fmap C.PScript (C.toCardanoPlutusScript (C.AsPlutusScript C.AsPlutusScriptV2) (getValidator validator))
                                <*> pure (C.ScriptDatumForTxIn $ C.toCardanoScriptData (getDatum datum))
                                <*> pure (C.toCardanoScriptData (getRedeemer redeemer))
                                <*> pure C.zeroExecutionUnits

                unbalancedTx . tx . txIns <>= [(txIn, C.BuildTxWith witness)]

            _ -> throwError (LedgerMkTxError $ P.TxOutRefWrongType txo)

    P.MustUseOutputAsCollateral txo -> do
        txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
        unbalancedTx . tx . txInsCollateral <>= [ txIn ]

    P.MustReferenceOutput txo -> do
        txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
        unbalancedTx . tx . txInsReference <>= [ txIn ]

    P.MustPayToPubKeyAddress pk mskh md refScriptHashM vl -> do
        networkId <- use (P.paramsL . networkIdL)
        refScript <- lookupScriptAsReferenceScript refScriptHashM
        out <- throwLeft ToCardanoError $ C.TxOut
            <$> C.toCardanoAddressInEra networkId (pubKeyHashAddress pk mskh)
            <*> C.toCardanoTxOutValue vl
            <*> pure (maybe C.TxOutDatumNone (C.TxOutDatumInTx C.ScriptDataInBabbageEra . C.toCardanoScriptData . getDatum) md)
            <*> pure refScript

        unbalancedTx . tx . txOuts <>= [ out ]

    P.MustPayToOtherScript vlh svhM dv refScriptHashM vl -> do
        networkId <- use (P.paramsL . networkIdL)
        refScript <- lookupScriptAsReferenceScript refScriptHashM
        out <- throwLeft ToCardanoError $ C.TxOut
            <$> C.toCardanoAddressInEra networkId (scriptValidatorHashAddress vlh svhM)
            <*> C.toCardanoTxOutValue vl
            <*> pure (C.TxOutDatumInTx C.ScriptDataInBabbageEra (C.toCardanoScriptData (getDatum dv)))
            <*> pure refScript
        unbalancedTx . tx . txOuts <>= [ out ]

    c -> error $ "Ledger.Tx.Constraints.OffChain: " ++ show c ++ " not implemented yet"

lookupTxOutRef
    :: Tx.TxOutRef
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) Tx.ChainIndexTxOut
lookupTxOutRef txo = mapReaderT (mapStateT (mapExcept (first LedgerMkTxError))) $ P.lookupTxOutRef txo

lookupScriptAsReferenceScript
    :: Maybe ScriptHash
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) (C.ReferenceScript C.BabbageEra)
lookupScriptAsReferenceScript Nothing = pure C.ReferenceScriptNone
lookupScriptAsReferenceScript (Just sh) = do
    (script, language) <- mapReaderT (mapStateT (mapExcept (first LedgerMkTxError))) $ P.lookupScript sh
    scriptInAnyLang <- either (throwError . ToCardanoError) pure $ toCardanoScriptInAnyLang script language
    pure $ C.ReferenceScript C.ReferenceTxInsScriptsInlineDatumsInBabbageEra scriptInAnyLang

addOwnOutput
    :: ToData (DatumType a)
    => ScriptOutputConstraint (DatumType a)
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) TxConstraint
addOwnOutput soc = mapReaderT (mapStateT (mapExcept (first LedgerMkTxError))) $ P.addOwnOutput soc
