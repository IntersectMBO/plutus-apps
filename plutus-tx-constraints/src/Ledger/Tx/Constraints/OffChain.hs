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
    , P.plutusV1MintingPolicy
    , P.plutusV2MintingPolicy
    , P.otherScript
    , P.plutusV1OtherScript
    , P.plutusV2OtherScript
    , P.otherData
    , P.ownPaymentPubKeyHash
    , P.ownStakingCredential
    , P.paymentPubKey
    , P.paymentPubKeyHash
    -- * Constraints resolution
    , P.SomeLookupsAndConstraints(..)
    , UnbalancedTx(..)
    , unBalancedTxTx
    , tx
    , txValidityRange
    , txOuts
    , P.utxoIndex
    , emptyUnbalancedTx
    , P.adjustUnbalancedTx
    , MkTxError(..)
    , mkTx
    , mkSomeTx
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator.Params (Params (..), networkIdL, pProtocolParams)
import Cardano.Node.Emulator.TimeSlot (posixTimeRangeToContainedSlotRange)
import Control.Lens (Lens', Traversal', _2, coerced, iso, makeLensesFor, use, uses, (.=), (<>=), (^.), (^?))
import Control.Lens.Extras (is)
import Control.Monad.Except (Except, MonadError, guard, lift, mapExcept, runExcept, throwError, unless, withExcept)
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), ask, mapReaderT)
import Control.Monad.State (MonadState, StateT, execStateT, gets, mapStateT)
import Data.Aeson (FromJSON, ToJSON)
import Data.Bifunctor (first)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Map qualified as Map
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger (Datum, Language (PlutusV2), MintingPolicy, MintingPolicyHash, POSIXTimeRange, Versioned,
               decoratedTxOutReferenceScript)
import Ledger.Constraints qualified as P
import Ledger.Constraints.OffChain (UnbalancedTx (..), cpsUnbalancedTx, unBalancedTxTx, unbalancedTx)
import Ledger.Constraints.OffChain qualified as P
import Ledger.Constraints.TxConstraints (ScriptOutputConstraint, TxConstraint,
                                         TxConstraints (TxConstraints, txConstraints, txOwnInputs, txOwnOutputs),
                                         TxOutDatum (TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline))
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Typed
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Tx qualified as PV2

import Ledger.Interval ()
import Ledger.Orphans ()
import Ledger.Scripts (ScriptHash, getRedeemer, getValidator)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (CardanoBuildTx (CardanoBuildTx), toCardanoMintWitness, toCardanoPolicyId)
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (ConnectionError (UnknownRef), ValidatorTypes (DatumType, RedeemerType))
import PlutusTx (FromData, ToData (toBuiltinData))
import PlutusTx.Lattice (BoundedMeetSemiLattice (top), MeetSemiLattice ((/\)))
import Prettyprinter (Pretty (pretty), colon, (<+>))

makeLensesFor
    [ ("txIns", "txIns'")
    , ("txInsCollateral", "txInsCollateral'")
    , ("txInsReference", "txInsReference'")
    , ("txExtraKeyWits", "txExtraKeyWits'")
    , ("txOuts", "txOuts'")
    , ("txValidityRange", "txValidityRange'")
    , ("txMintValue", "txMintValue'")
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

txExtraKeyWits :: Lens' C.CardanoBuildTx (Set.Set (C.Hash C.PaymentKey))
txExtraKeyWits = coerced . txExtraKeyWits' . iso toSet fromSet
    where
        toSet C.TxExtraKeyWitnessesNone        = mempty
        toSet (C.TxExtraKeyWitnesses _ txwits) = Set.fromList txwits
        fromSet s | null s    = C.TxExtraKeyWitnessesNone
                  | otherwise = C.TxExtraKeyWitnesses C.ExtraKeyWitnessesInBabbageEra $ Set.toList s

txInsReference :: Lens' C.CardanoBuildTx [C.TxIn]
txInsReference = coerced . txInsReference' . iso toList fromList
    where
        toList C.TxInsReferenceNone       = []
        toList (C.TxInsReference _ txins) = txins
        fromList []    = C.TxInsReferenceNone
        fromList txins = C.TxInsReference C.ReferenceTxInsScriptsInlineDatumsInBabbageEra txins

txMintValue :: Lens' C.CardanoBuildTx
                 (C.Value, Map.Map C.PolicyId (C.ScriptWitness C.WitCtxMint C.BabbageEra))
txMintValue = coerced . txMintValue' . iso toMaybe fromMaybe
    where
        toMaybe :: C.TxMintValue C.BuildTx C.BabbageEra -> (C.Value, Map.Map C.PolicyId (C.ScriptWitness C.WitCtxMint C.BabbageEra))
        toMaybe (C.TxMintValue _ v (C.BuildTxWith msc)) = (v, msc)
        toMaybe _                                       = (mempty, mempty)
        fromMaybe ::  (C.Value, Map.Map C.PolicyId (C.ScriptWitness C.WitCtxMint C.BabbageEra)) -> C.TxMintValue C.BuildTx C.BabbageEra
        fromMaybe (c, msc) = C.TxMintValue C.MultiAssetInBabbageEra c (C.BuildTxWith msc)

txOuts :: Lens' C.CardanoBuildTx [C.TxOut C.CtxTx C.BabbageEra]
txOuts = coerced . txOuts'

txValidityRange :: Lens' C.CardanoBuildTx (C.TxValidityLowerBound C.BabbageEra, C.TxValidityUpperBound C.BabbageEra)
txValidityRange = coerced . txValidityRange'

tx :: Traversal' UnbalancedTx C.CardanoBuildTx
tx = P.cardanoTx

emptyCardanoBuildTx :: Params -> C.CardanoBuildTx
emptyCardanoBuildTx p = C.CardanoBuildTx $ C.TxBodyContent
    { C.txIns = mempty
    , C.txInsCollateral = C.TxInsCollateral C.CollateralInBabbageEra mempty
    , C.txInsReference = C.TxInsReferenceNone
    , C.txOuts = mempty
    , C.txTotalCollateral = C.TxTotalCollateralNone
    , C.txReturnCollateral = C.TxReturnCollateralNone
    , C.txFee = C.TxFeeExplicit C.TxFeesExplicitInBabbageEra mempty
    , C.txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
    , C.txMintValue = C.TxMintNone
    , C.txProtocolParams = C.BuildTxWith $ Just $ pProtocolParams p
    , C.txScriptValidity = C.TxScriptValidityNone
    , C.txExtraKeyWits = C.TxExtraKeyWitnessesNone
    , C.txMetadata = C.TxMetadataNone
    , C.txAuxScripts = C.TxAuxScriptsNone
    , C.txWithdrawals = C.TxWithdrawalsNone
    , C.txCertificates = C.TxCertificatesNone
    , C.txUpdateProposal = C.TxUpdateProposalNone
    }

emptyUnbalancedTx :: Params -> UnbalancedTx
emptyUnbalancedTx params = UnbalancedCardanoTx (emptyCardanoBuildTx params) mempty

initialState :: Params -> P.ConstraintProcessingState
initialState params = P.ConstraintProcessingState
    { P.cpsUnbalancedTx = emptyUnbalancedTx params
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

data SortedConstraints
   = MkSortedConstraints
   { rangeConstraints :: [POSIXTimeRange]
   , otherConstraints :: [TxConstraint]
   }

prepareConstraints
    :: ToData (DatumType a)
    => [ScriptOutputConstraint (DatumType a)]
    -> [TxConstraint]
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) SortedConstraints
prepareConstraints ownOutputs constraints = do
    let
      extractPosixTimeRange = \case
        P.MustValidateIn range -> Left range
        other                  -> Right other
      (ranges, nonRangeConstraints) = partitionEithers $ extractPosixTimeRange <$> constraints
    other <- mapLedgerMkTxError $ P.prepareConstraints ownOutputs nonRangeConstraints
    pure $ MkSortedConstraints ranges other


-- | Resolve some 'TxConstraints' by modifying the 'UnbalancedTx' in the
--   'ConstraintProcessingState'
processLookupsAndConstraints
    ::
    ( FromData (DatumType a)
    , ToData (DatumType a)
    , ToData (RedeemerType a)
    )
    => P.ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> StateT P.ConstraintProcessingState (Except MkTxError) ()
processLookupsAndConstraints lookups TxConstraints{txConstraints, txOwnInputs, txOwnOutputs} = do
        flip runReaderT lookups $ do
            sortedConstraints <- prepareConstraints txOwnOutputs txConstraints
            traverse_ processConstraint (otherConstraints sortedConstraints)
            -- traverse_ P.processConstraintFun txCnsFuns
            traverse_ addOwnInput txOwnInputs
            -- P.addMintingRedeemers
            checkValueSpent
            mapReaderT (mapStateT (withExcept LedgerMkTxError)) P.updateUtxoIndex
            lift $ setValidityRange (rangeConstraints sortedConstraints)

checkValueSpent
    :: ( MonadReader (P.ScriptLookups a) m
       , MonadState P.ConstraintProcessingState m
       , MonadError MkTxError m
       )
    => m ()
checkValueSpent = do
    missingInputs <- uses P.valueSpentInputs P.missingValueSpent
    unless (Value.isZero missingInputs) $ throwError $ LedgerMkTxError $ P.DeclaredInputMismatch missingInputs
    missingOutputs <- uses P.valueSpentOutputs P.missingValueSpent
    unless (Value.isZero missingOutputs) $ throwError $ LedgerMkTxError $ P.DeclaredOutputMismatch missingOutputs

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
    P.MustIncludeDatumInTxWithHash _ _ -> pure () -- always succeeds
    P.MustIncludeDatumInTx _ -> pure () -- always succeeds
    P.MustSpendPubKeyOutput txo -> do
        txout <- lookupTxOutRef txo
        value <- maybe (throwError (LedgerMkTxError $ P.TxOutRefWrongType txo)) pure $ do
            guard $ is Tx._PublicKeyDecoratedTxOut txout
            pure $ txout ^. Tx.decoratedTxOutValue
        txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
        unbalancedTx . tx . txIns <>= [(txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))]
        P.valueSpentInputs <>= P.provided value

    P.MustBeSignedBy pk -> do
        ekw <-  either (throwError . ToCardanoError) pure $ C.toCardanoPaymentKeyHash pk
        unbalancedTx . tx . txExtraKeyWits <>= Set.singleton ekw
    P.MustSpendScriptOutput txo redeemer mref -> do
        txout <- lookupTxOutRef txo
        mkWitness <- case mref of
          Just ref -> do
            refTxOut <- lookupTxOutRef ref
            case refTxOut ^. Tx.decoratedTxOutReferenceScript of
                Just (Tx.Versioned _ lang) -> do
                    txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn ref
                    unbalancedTx . tx . txInsReference <>= [ txIn ]
                    throwLeft ToCardanoError $ C.toCardanoTxInReferenceWitnessHeader (Tx.Versioned ref lang)
                _ -> throwError (LedgerMkTxError $ P.TxOutRefNoReferenceScript ref)
          Nothing -> do
            mscriptTXO <- mapLedgerMkTxError $ P.resolveScriptTxOutValidator txout
            case mscriptTXO of
                Just validator ->
                    throwLeft ToCardanoError $ C.toCardanoTxInScriptWitnessHeader (getValidator <$> validator)
                _ -> throwError (LedgerMkTxError $ P.TxOutRefWrongType txo)
        mscriptTXO <- mapLedgerMkTxError $ P.resolveScriptTxOutDatumAndValue txout
        case mscriptTXO of
            Just (datum, value) -> do
                txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
                let witness
                        = C.ScriptWitness C.ScriptWitnessForSpending $
                            mkWitness
                            (C.toCardanoDatumWitness $ P.datumWitness datum)
                            (C.toCardanoScriptData (getRedeemer redeemer))
                            C.zeroExecutionUnits

                unbalancedTx . tx . txIns <>= [(txIn, C.BuildTxWith witness)]

                P.valueSpentInputs <>= P.provided value

            _ -> throwError (LedgerMkTxError $ P.TxOutRefWrongType txo)

    P.MustUseOutputAsCollateral txo -> do
        txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
        unbalancedTx . tx . txInsCollateral <>= [ txIn ]

    P.MustReferenceOutput txo -> do
        txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
        unbalancedTx . tx . txInsReference <>= [ txIn ]

    P.MustMintValue mpsHash red tn i mref -> do
        let value = Value.singleton (Value.mpsSymbol mpsHash) tn

        -- If i is negative we are burning tokens. The tokens burned must
        -- be provided as an input. So we add the value burnt to
        -- 'valueSpentInputs'. If i is positive then new tokens are created
        -- which must be added to 'valueSpentOutputs'.
        if i < 0
            then P.valueSpentInputs <>= P.provided (value (negate i))
            else P.valueSpentOutputs <>= P.provided (value i)

        v <- either undefined pure $ C.toCardanoValue $ value i
        pId <- either undefined pure $ toCardanoPolicyId mpsHash
        witness <- case mref of
            Just ref -> do
                refTxOut <- lookupTxOutRef ref
                case refTxOut ^? decoratedTxOutReferenceScript of
                    Just _ -> do
                      txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn . Tx.txInputRef . Tx.pubKeyTxInput $ ref
                      unbalancedTx . tx . txInsReference <>= [txIn]
                      throwLeft ToCardanoError
                        $ toCardanoMintWitness red (flip Tx.Versioned PlutusV2 <$> mref) Nothing
                    _      -> throwError (LedgerMkTxError $ P.TxOutRefNoReferenceScript ref)
            Nothing -> do
                mintingPolicyScript <- lookupMintingPolicy mpsHash
                throwLeft ToCardanoError
                  $ toCardanoMintWitness red Nothing (Just mintingPolicyScript)
        unbalancedTx . tx . txMintValue <>= (v, Map.singleton pId witness)

    P.MustPayToAddress addr md refScriptHashM vl -> do
        networkId <- use (P.paramsL . networkIdL)
        refScript <- lookupScriptAsReferenceScript refScriptHashM
        out <- throwLeft ToCardanoError $ C.TxOut
            <$> C.toCardanoAddressInEra networkId addr
            <*> C.toCardanoTxOutValue vl
            <*> pure (toTxOutDatum md)
            <*> pure refScript
        unbalancedTx . tx . txOuts <>= [ out ]

        P.valueSpentOutputs <>= P.provided vl

    P.MustSpendAtLeast vl -> P.valueSpentInputs <>= P.required vl
    P.MustProduceAtLeast vl -> P.valueSpentOutputs <>= P.required vl

    c -> error $ "Ledger.Tx.Constraints.OffChain: " ++ show c ++ " not implemented yet"

lookupTxOutRef
    :: Tx.TxOutRef
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) Tx.DecoratedTxOut
lookupTxOutRef txo = mapLedgerMkTxError $ P.lookupTxOutRef txo

lookupMintingPolicy
    :: MintingPolicyHash
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) (Versioned MintingPolicy)
lookupMintingPolicy mph = mapLedgerMkTxError $ P.lookupMintingPolicy mph

lookupScriptAsReferenceScript
    :: Maybe ScriptHash
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) (C.ReferenceScript C.BabbageEra)
lookupScriptAsReferenceScript msh = mapLedgerMkTxError $ P.lookupScriptAsReferenceScript msh

mapLedgerMkTxError
    :: ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except P.MkTxError)) b
    -> ReaderT (P.ScriptLookups a) (StateT P.ConstraintProcessingState (Except MkTxError)) b
mapLedgerMkTxError = mapReaderT (mapStateT (mapExcept (first LedgerMkTxError)))

toTxOutDatum :: Maybe (TxOutDatum Datum) -> C.TxOutDatum C.CtxTx C.BabbageEra
toTxOutDatum = \case
    Nothing                   -> C.toCardanoTxOutNoDatum
    Just (TxOutDatumHash d)   -> C.toCardanoTxOutDatumHashFromDatum d
    Just (TxOutDatumInTx d)   -> C.toCardanoTxOutDatumInTx d
    Just (TxOutDatumInline d) -> C.toCardanoTxOutDatumInline d

-- | Add a typed input, checking the type of the output it spends. Return the value
--   of the spent output.
addOwnInput
    :: ( MonadReader (P.ScriptLookups a) m
       , MonadError MkTxError m
       , MonadState P.ConstraintProcessingState m
       , FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       )
    => P.ScriptInputConstraint (RedeemerType a)
    -> m ()
addOwnInput P.ScriptInputConstraint{P.icRedeemer, P.icTxOutRef} = do
    P.ScriptLookups{P.slTxOutputs, P.slTypedValidator} <- ask
    inst <- maybe (throwError $ LedgerMkTxError P.TypedValidatorMissing) pure slTypedValidator
    Typed.TypedScriptTxOutRef{Typed.tyTxOutRefRef, Typed.tyTxOutRefOut} <-
      either (throwError . LedgerMkTxError . P.TypeCheckFailed) pure
      $ runExcept @Typed.ConnectionError
      $ do
          (txOut, datum) <- maybe (throwError $ UnknownRef icTxOutRef) pure $ do
                                ciTxOut <- Map.lookup icTxOutRef slTxOutputs
                                datum <- ciTxOut ^? Tx.decoratedTxOutDatum . _2 . Tx.datumInDatumFromQuery
                                pure (Tx.toTxInfoTxOut ciTxOut, datum)
          Typed.typeScriptTxOutRef inst icTxOutRef txOut datum
    let vl = PV2.txOutValue $ Typed.tyTxOutTxOut tyTxOutRefOut
    P.valueSpentInputs <>= P.provided vl
    let datum = C.ScriptDatumForTxIn $ C.toCardanoScriptData $ toBuiltinData $ Typed.tyTxOutData tyTxOutRefOut
    txIn <- either (throwError . ToCardanoError) pure $ C.toCardanoTxIn tyTxOutRefRef
    mkWitness <- either (throwError . ToCardanoError) pure
                     $ C.toCardanoTxInScriptWitnessHeader $ fmap getValidator $ Typed.vValidatorScript inst
    let witIn = C.ScriptWitness
                    C.ScriptWitnessForSpending
                    $ mkWitness datum (C.toCardanoScriptData $ toBuiltinData icRedeemer) C.zeroExecutionUnits
    unbalancedTx . tx .txIns <>= [(txIn, C.BuildTxWith witIn)]
    pure ()
