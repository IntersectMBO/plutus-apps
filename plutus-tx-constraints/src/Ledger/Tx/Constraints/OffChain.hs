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
    , P.mintingPolicy
    , P.otherScript
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
    -- * Internals exposed for testing
    , P.ValueSpentBalances(..)
    , P.provided
    , P.required
    , P.missingValueSpent
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Lens (Lens', Traversal', _Left, coerced, makeLensesFor, use, (<>=))
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

import Ledger (Params (..), networkIdL)
import Ledger.Address (pubKeyHashAddress)
import Ledger.Constraints.TxConstraints (TxConstraint, TxConstraints (TxConstraints, txConstraints))
import Ledger.Orphans ()
import Ledger.Scripts (getDatum)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (ValidatorTypes (DatumType, RedeemerType))

import Ledger.Constraints qualified as P
import Ledger.Constraints.OffChain (UnbalancedTx (..), cpsUnbalancedTx, unbalancedTx)
import Ledger.Constraints.OffChain qualified as P

makeLensesFor
    [ ("txIns", "txIns'")
    , ("txOuts", "txOuts'")
    ] ''C.TxBodyContent

txIns :: Lens' C.CardanoBuildTx [(C.TxIn, C.BuildTxWith C.BuildTx (C.Witness C.WitCtxTxIn C.AlonzoEra))]
txIns = coerced . txIns'

txOuts :: Lens' C.CardanoBuildTx [C.TxOut C.CtxTx C.AlonzoEra]
txOuts = coerced . txOuts'

tx :: Traversal' UnbalancedTx C.CardanoBuildTx
tx = P.cardanoTx . _Left

emptyCardanoBuildTx :: Params -> C.CardanoBuildTx
emptyCardanoBuildTx Params { pProtocolParams }= C.CardanoBuildTx $ C.TxBodyContent
    { C.txIns = mempty
    , C.txInsCollateral = C.TxInsCollateral C.CollateralInAlonzoEra mempty
    , C.txInsReference = C.TxInsReferenceNone
    , C.txOuts = mempty
    , C.txTotalCollateral = C.TxTotalCollateralNone
    , C.txReturnCollateral = C.TxReturnCollateralNone
    , C.txFee = C.TxFeeExplicit C.TxFeesExplicitInAlonzoEra mempty
    , C.txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInAlonzoEra)
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
            -- P.updateUtxoIndex

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
        txout <- mapReaderT (mapStateT (mapExcept (first LedgerMkTxError))) $ P.lookupTxOutRef txo
        case txout of
          Tx.PublicKeyOffChainTxOut {} -> do
              txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
              unbalancedTx . tx . txIns <>= [(txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))]
          Tx.ScriptOffChainTxOut {} ->
            throwError (LedgerMkTxError $ P.TxOutRefWrongType txo)

    P.MustPayToPubKeyAddress pk mskh md vl -> do

        -- valueSpentOutputs <>= P.provided vl

        networkId <- use (P.paramsL . networkIdL)
        out <- throwLeft ToCardanoError $ C.TxOut
            <$> C.toCardanoAddressInEra networkId (pubKeyHashAddress pk mskh)
            <*> C.toCardanoTxOutValue vl
            <*> pure (maybe C.TxOutDatumNone (C.TxOutDatumInTx C.ScriptDataInAlonzoEra . C.toCardanoScriptData . getDatum) md)
            <*> pure C.ReferenceScriptNone

        unbalancedTx . tx . txOuts <>= [ out ]

    _ -> pure ()
