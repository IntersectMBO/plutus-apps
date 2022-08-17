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
    , tx
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
import Control.Lens (Lens', Traversal', _Left, coerced, iso, makeLensesFor, use, (<>=))
import Control.Monad.Except (Except, mapExcept, runExcept, throwError)
import Control.Monad.Reader (ReaderT (runReaderT), mapReaderT)
import Control.Monad.State (StateT, execStateT, mapStateT)

import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), colon, (<+>))

import PlutusTx (FromData, ToData)
import PlutusTx.Lattice (BoundedMeetSemiLattice (top))

import Ledger.Address (pubKeyHashAddress, scriptValidatorHashAddress)
import Ledger.Constraints qualified as P
import Ledger.Constraints.OffChain (UnbalancedTx (..), cpsUnbalancedTx, unbalancedTx)
import Ledger.Constraints.OffChain qualified as P
import Ledger.Constraints.TxConstraints (TxConstraint, TxConstraints (TxConstraints, txConstraints))
import Ledger.Orphans ()
import Ledger.Params (Params (..), networkIdL)
import Ledger.Scripts (getDatum, getRedeemer, getValidator)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (ValidatorTypes (DatumType, RedeemerType))

makeLensesFor
    [ ("txIns", "txIns'")
    , ("txInsCollateral", "txInsCollateral'")
    , ("txInsReference", "txInsReference'")
    , ("txOuts", "txOuts'")
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

tx :: Traversal' UnbalancedTx C.CardanoBuildTx
tx = P.cardanoTx . _Left

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
emptyUnbalancedTx params = UnbalancedTx (Left $ emptyCardanoBuildTx params) mempty mempty top

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
    in fmap cpsUnbalancedTx
        $ runExcept
        $ execStateT (traverse process xs) (initialState params)

-- | Resolve some 'TxConstraints' by modifying the 'UnbalancedTx' in the
--   'ConstraintProcessingState'
processLookupsAndConstraints
    :: ( -- FromData (DatumType a)
    --    , ToData (DatumType a)
    --    , ToData (RedeemerType a)
       )
    => P.ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> StateT P.ConstraintProcessingState (Except MkTxError) ()
processLookupsAndConstraints lookups TxConstraints{txConstraints} =
        flip runReaderT lookups $ do
            traverse_ processConstraint txConstraints
            -- traverse_ P.processConstraintFun txCnsFuns
            -- traverse_ P.addOwnInput txOwnInputs
            -- traverse_ P.addOwnOutput txOwnOutputs
            -- P.addMintingRedeemers
            -- P.addMissingValueSpent
            P.updateUtxoIndex

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

throwLeft :: (b -> MkTxError) -> Either b r -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) r
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

    P.MustPayToPubKeyAddress pk mskh md vl -> do
        networkId <- use (P.paramsL . networkIdL)
        out <- throwLeft ToCardanoError $ C.TxOut
            <$> C.toCardanoAddressInEra networkId (pubKeyHashAddress pk mskh)
            <*> C.toCardanoTxOutValue vl
            <*> pure (maybe C.TxOutDatumNone (C.TxOutDatumInTx C.ScriptDataInBabbageEra . C.toCardanoScriptData . getDatum) md)
            <*> pure C.ReferenceScriptNone

        unbalancedTx . tx . txOuts <>= [ out ]

    P.MustPayToOtherScript vlh svhM dv vl -> do
        networkId <- use (P.paramsL . networkIdL)
        out <- throwLeft ToCardanoError $ C.TxOut
            <$> C.toCardanoAddressInEra networkId (scriptValidatorHashAddress vlh svhM)
            <*> C.toCardanoTxOutValue vl
            <*> pure (C.TxOutDatumInTx C.ScriptDataInBabbageEra (C.toCardanoScriptData (getDatum dv)))
            <*> pure C.ReferenceScriptNone
        unbalancedTx . tx . txOuts <>= [ out ]

    c -> error $ "Ledger.Tx.Constraints.OffChain: " ++ show c ++ " not implemented yet"

lookupTxOutRef
    :: Tx.TxOutRef
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) Tx.ChainIndexTxOut
lookupTxOutRef txo = mapReaderT (mapStateT (mapExcept (first LedgerMkTxError))) $ P.lookupTxOutRef txo
