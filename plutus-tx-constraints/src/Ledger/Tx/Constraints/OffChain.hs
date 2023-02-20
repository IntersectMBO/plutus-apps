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
    ScriptLookups(..)
    , typedValidatorLookups
    , generalise
    , unspentOutputs
    , mintingPolicy
    , plutusV1MintingPolicy
    , plutusV2MintingPolicy
    , otherScript
    , plutusV1OtherScript
    , plutusV2OtherScript
    , otherData
    , paymentPubKey
    , paymentPubKeyHash
    -- * Constraints resolution
    , SomeLookupsAndConstraints(..)
    , UnbalancedTx(..)
    , tx
    , txInsCollateral
    , txValidityRange
    , txOuts
    , utxoIndex
    , emptyUnbalancedTx
    , adjustUnbalancedTx
    , mkTx
    , mkTxWithParams
    , mkSomeTx
    , MkTxError(..)
    , _TypeCheckFailed
    , _ToCardanoError
    , _TxOutRefNotFound
    , _TxOutRefWrongType
    , _TxOutRefNoReferenceScript
    , _DatumNotFound
    , _DeclaredInputMismatch
    , _MintingPolicyNotFound
    , _ScriptHashNotFound
    , _TypedValidatorMissing
    , _DatumWrongHash
    , _CannotSatisfyAny
    , _NoMatchingOutputFound
    , _MultipleMatchingOutputsFound
    -- * Internals exposed for testing
    , ValueSpentBalances(..)
    , provided
    , required
    , missingValueSpent
    , ConstraintProcessingState(..)
    , unbalancedTx
    , valueSpentInputs
    , valueSpentOutputs
    , paramsL
    , processConstraintFun
    , addOwnInput
    , addOwnOutput
    , updateUtxoIndex
    , lookupTxOutRef
    , lookupMintingPolicy
    , lookupScript
    , lookupScriptAsReferenceScript
    , prepareConstraints
    , resolveScriptTxOut
    , resolveScriptTxOutValidator
    , resolveScriptTxOutDatumAndValue
    , DatumWithOrigin(..)
    , datumWitness
    , checkValueSpent
    , SortedConstraints(..)
    , initialState
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Node.Emulator.Params (PParams, Params (..), networkIdL, pProtocolParams)
import Cardano.Node.Emulator.TimeSlot (posixTimeRangeToContainedSlotRange, slotRangeToPOSIXTimeRange)
import Control.Lens
import Control.Lens.Extras (is)
import Control.Monad.Except (Except, MonadError (catchError), guard, lift, runExcept, throwError, unless)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get, put), StateT, execStateT, gets)
import Data.Aeson (FromJSON, ToJSON)
import Data.Either (partitionEithers)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose (Compose))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Semigroup (First (First, getFirst))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger (Datum, Language (PlutusV1, PlutusV2), MintingPolicy, MintingPolicyHash, POSIXTimeRange,
               Redeemer (Redeemer), Versioned, adjustCardanoTxOut, decoratedTxOutReferenceScript)
import Ledger.Address (PaymentPubKey (PaymentPubKey), PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Crypto (pubKeyHash)
import Ledger.Interval ()
import Ledger.Orphans ()
import Ledger.Scripts (ScriptHash, getRedeemer, getValidator)
import Ledger.Tx (DecoratedTxOut, TxOut, TxOutRef)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (CardanoBuildTx (CardanoBuildTx), toCardanoMintWitness, toCardanoPolicyId)
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Tx.Constraints.TxConstraints
import Ledger.Tx.Constraints.ValidityInterval (toPlutusInterval)
import Ledger.Typed.Scripts (Any, ConnectionError (UnknownRef), TypedValidator (tvValidator, tvValidatorHash),
                             ValidatorTypes (DatumType, RedeemerType), validatorAddress)
import Plutus.Script.Utils.Scripts (datumHash, scriptHash)
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Typed
import Plutus.Script.Utils.Value qualified as Value
import Plutus.V1.Ledger.Api (Datum (Datum), DatumHash, StakingCredential, Validator, Value, getMintingPolicy)
import Plutus.V1.Ledger.Scripts (MintingPolicy (MintingPolicy), MintingPolicyHash (MintingPolicyHash), Script,
                                 ScriptHash (ScriptHash), Validator (Validator), ValidatorHash (ValidatorHash))
import PlutusTx (FromData, ToData (toBuiltinData))
import PlutusTx.Lattice (BoundedMeetSemiLattice (top), JoinSemiLattice ((\/)), MeetSemiLattice ((/\)))
import PlutusTx.Numeric qualified as N
import Prettyprinter (Pretty (pretty), colon, hang, viaShow, vsep, (<+>))


data ScriptLookups a =
    ScriptLookups
        { slTxOutputs            :: Map TxOutRef DecoratedTxOut
        -- ^ Unspent outputs that the script may want to spend
        , slOtherScripts         :: Map ScriptHash (Versioned Script)
        -- ^ Scripts other than "our script"
        , slOtherData            :: Map DatumHash Datum
        -- ^ Datums that we might need
        , slPaymentPubKeyHashes  :: Set PaymentPubKeyHash
        -- ^ Public keys that we might need
        , slTypedValidator       :: Maybe (TypedValidator a)
        -- ^ The script instance with the typed validator hash & actual compiled program
        , slOwnPaymentPubKeyHash :: Maybe PaymentPubKeyHash
        -- ^ The contract's payment public key hash, used for depositing tokens etc.
        , slOwnStakingCredential :: Maybe StakingCredential
        -- ^ The contract's staking credentials (optional)
        } deriving stock (Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

generalise :: ScriptLookups a -> ScriptLookups Any
generalise sl =
    let validator = fmap Typed.generalise (slTypedValidator sl)
    in sl{slTypedValidator = validator}

instance Semigroup (ScriptLookups a) where
    l <> r =
        ScriptLookups
            { slTxOutputs = slTxOutputs l <> slTxOutputs r
            , slOtherScripts = slOtherScripts l <> slOtherScripts r
            , slOtherData = slOtherData l <> slOtherData r
            , slPaymentPubKeyHashes = slPaymentPubKeyHashes l <> slPaymentPubKeyHashes r
            -- 'First' to match the semigroup instance of Map (left-biased)
            , slTypedValidator = fmap getFirst $ (First <$> slTypedValidator l) <> (First <$> slTypedValidator r)
            , slOwnPaymentPubKeyHash =
                fmap getFirst $ (First <$> slOwnPaymentPubKeyHash l)
                             <> (First <$> slOwnPaymentPubKeyHash r)
            , slOwnStakingCredential =
                fmap getFirst $ (First <$> slOwnStakingCredential l)
                             <> (First <$> slOwnStakingCredential r)
            }

instance Monoid (ScriptLookups a) where
    mappend = (<>)
    mempty  = ScriptLookups mempty mempty mempty mempty Nothing Nothing Nothing

-- | A script lookups value with a script instance. For convenience this also
--   includes the minting policy script that forwards all checks to the
--   instance's validator.
--
-- If called multiple times, only the first typed validator is kept:
--
-- @
-- typedValidatorLookups tv1 <> typedValidatorLookups tv2 <> ...
--     == typedValidatorLookups tv1
-- @
typedValidatorLookups :: TypedValidator a -> ScriptLookups a
typedValidatorLookups inst =
    let (ValidatorHash vh, v) = (tvValidatorHash inst, tvValidator inst)
        (MintingPolicyHash mph, mp) = (Typed.forwardingMintingPolicyHash inst, Typed.vForwardingMintingPolicy inst)
    in mempty
        { slOtherScripts =
            Map.fromList [ (ScriptHash vh, fmap getValidator v)
                         , (ScriptHash mph, fmap getMintingPolicy mp)
                         ]
        , slTypedValidator = Just inst
        }


-- | A script lookups value that uses the map of unspent outputs to resolve
--   input constraints.
unspentOutputs :: Map TxOutRef DecoratedTxOut -> ScriptLookups a
unspentOutputs mp = mempty { slTxOutputs = mp }

-- | A script lookups value with a versioned minting policy script.
mintingPolicy :: Versioned MintingPolicy -> ScriptLookups a
mintingPolicy (fmap getMintingPolicy -> script) = mempty { slOtherScripts = Map.singleton (scriptHash script) script }

-- | A script lookups value with a PlutusV1 minting policy script.
plutusV1MintingPolicy :: MintingPolicy -> ScriptLookups a
plutusV1MintingPolicy pl = mintingPolicy (Tx.Versioned pl PlutusV1)

-- | A script lookups value with a PlutusV2 minting policy script.
plutusV2MintingPolicy :: MintingPolicy -> ScriptLookups a
plutusV2MintingPolicy pl = mintingPolicy (Tx.Versioned pl PlutusV2)

-- | A script lookups value with a versioned validator script.
otherScript :: Versioned Validator -> ScriptLookups a
otherScript (fmap getValidator -> script) = mempty { slOtherScripts = Map.singleton (scriptHash script) script }

-- | A script lookups value with a PlutusV1 validator script.
plutusV1OtherScript :: Validator -> ScriptLookups a
plutusV1OtherScript vl = otherScript (Tx.Versioned vl PlutusV1)

-- | A script lookups value with a PlutusV2 validator script.
plutusV2OtherScript :: Validator -> ScriptLookups a
plutusV2OtherScript vl = otherScript (Tx.Versioned vl PlutusV2)

-- | A script lookups value with a datum.
otherData :: Datum -> ScriptLookups a
otherData dt =
    let dh = datumHash dt in
    mempty { slOtherData = Map.singleton dh dt }

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

-- | A script lookups value with a payment public key
paymentPubKey :: PaymentPubKey -> ScriptLookups a
paymentPubKey (PaymentPubKey pk) =
   paymentPubKeyHash (PaymentPubKeyHash $ pubKeyHash pk)

-- | A script lookups value with a payment public key
paymentPubKeyHash :: PaymentPubKeyHash -> ScriptLookups a
paymentPubKeyHash pkh =
    mempty { slPaymentPubKeyHashes = Set.singleton pkh }


-- | An unbalanced transaction. It needs to be balanced and signed before it
--   can be submitted to the ledger. See note [Submitting transactions from
--   Plutus contracts] in 'Plutus.Contract.Wallet'.
data UnbalancedTx
    = UnbalancedCardanoTx
        { unBalancedCardanoBuildTx :: C.CardanoBuildTx
        , unBalancedTxUtxoIndex    :: Map TxOutRef TxOut
        -- ^ Utxo lookups that are used for adding inputs to the 'UnbalancedTx'.
        -- Simply refers to  'slTxOutputs' of 'ScriptLookups'.
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

makeLensesFor
    [ ("unBalancedCardanoBuildTx", "cardanoTx")
    , ("unBalancedTxUtxoIndex", "utxoIndex")
    ] ''UnbalancedTx

tx :: Traversal' UnbalancedTx C.CardanoBuildTx
tx = cardanoTx

instance Pretty UnbalancedTx where
    pretty (UnbalancedCardanoTx utx utxo) =
        vsep
        [ hang 2 $ vsep ["Tx:", pretty utx]
        , hang 2 $ vsep $ "Requires signatures:" : (viaShow <$> Set.toList (utx ^. txExtraKeyWits))
        , hang 2 $ vsep $ "Utxo index:" : (pretty <$> Map.toList utxo)
        ]

{- Note [Balance of value spent]

To build a transaction that satisfies the 'MustSpendAtLeast' and
'MustProduceAtLeast' constraints, we keep a tally of the required and
actual values we encounter on either side of the transaction. Then we
compute the missing value on both sides, and add an input with the
join of the positive parts [1] of the missing values.

[1] See 'Plutus.V1.Ledger.Value.split'

-}

-- | The balances we track for computing the missing 'Value' (if any)
--   that needs to be added to the transaction.
--   See note [Balance of value spent].
data ValueSpentBalances =
    ValueSpentBalances
        { vbsRequired :: Value
        -- ^ Required value spent by the transaction.
        , vbsProvided :: Value
        -- ^ Value provided by an input or output of the transaction.
        } deriving (Show, Generic)

instance Semigroup ValueSpentBalances where
    l <> r =
        ValueSpentBalances
            { vbsRequired = vbsRequired l \/ vbsRequired r
            , vbsProvided = vbsProvided l \/ vbsProvided r
            }

-- No @Monoid ValueSpentBalances@ because @max@ (used by 'convexUnion') is only
-- a semigroup. In this module we only use @Value@s with non-negative amounts,
-- so @mempty :: Value@ technically is the identity, but I'd rather not
-- define the instance. Maybe we need a type for non-negative @Value@s.

data ConstraintProcessingState =
    ConstraintProcessingState
        { cpsUnbalancedTx              :: UnbalancedTx
        -- ^ The unbalanced transaction that we're building
        , cpsValueSpentBalancesInputs  :: ValueSpentBalances
        -- ^ Balance of the values given and required for the transaction's
        --   inputs
        , cpsValueSpentBalancesOutputs :: ValueSpentBalances
        -- ^ Balance of the values produced and required for the transaction's
        --   outputs
        , cpsParams                    :: Params
        }

missingValueSpent :: ValueSpentBalances -> Value
missingValueSpent ValueSpentBalances{vbsRequired, vbsProvided} =
    let
        difference = vbsRequired <> N.negate vbsProvided
        (_, missing) = Value.split difference
    in missing

makeLensesFor
    [ ("cpsUnbalancedTx", "unbalancedTx")
    , ("cpsMintRedeemers", "mintRedeemers")
    , ("cpsValueSpentBalancesInputs", "valueSpentInputs")
    , ("cpsValueSpentBalancesOutputs", "valueSpentOutputs")
    , ("cpsParams", "paramsL")
    ] ''ConstraintProcessingState

initialState :: Params -> ConstraintProcessingState
initialState params = ConstraintProcessingState
    { cpsUnbalancedTx = emptyUnbalancedTx params
    , cpsValueSpentBalancesInputs = ValueSpentBalances mempty mempty
    , cpsValueSpentBalancesOutputs = ValueSpentBalances mempty mempty
    , cpsParams = params
    }

provided :: Value -> ValueSpentBalances
provided v = ValueSpentBalances { vbsProvided = v, vbsRequired = mempty }

required :: Value -> ValueSpentBalances
required v = ValueSpentBalances { vbsRequired = v, vbsProvided = mempty }

-- | Some typed 'TxConstraints' and the 'ScriptLookups' needed to turn them
--   into an 'UnbalancedTx'.
data SomeLookupsAndConstraints where
    SomeLookupsAndConstraints
        :: forall a. (FromData (DatumType a), ToData (DatumType a), ToData (RedeemerType a))
        => ScriptLookups a
        -> TxConstraints (RedeemerType a) (DatumType a)
        -> SomeLookupsAndConstraints

data MkTxError =
      TypeCheckFailed Typed.ConnectionError
    | ToCardanoError C.ToCardanoError
    | TxOutRefNotFound TxOutRef
    | TxOutRefWrongType TxOutRef
    | TxOutRefNoReferenceScript TxOutRef
    | DatumNotFound DatumHash
    | DeclaredInputMismatch Value
    | DeclaredOutputMismatch Value
    | MintingPolicyNotFound MintingPolicyHash
    | ScriptHashNotFound ScriptHash
    | TypedValidatorMissing
    | DatumWrongHash DatumHash Datum
    | CannotSatisfyAny
    | NoMatchingOutputFound ValidatorHash
    | MultipleMatchingOutputsFound ValidatorHash
    | AmbiguousRedeemer TxOutRef [Redeemer]
    | AmbiguousReferenceScript TxOutRef [TxOutRef]
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
makeClassyPrisms ''MkTxError

instance Pretty MkTxError where
    pretty = \case
        TypeCheckFailed e              -> "Type check failed:" <+> pretty e
        ToCardanoError e               -> "Cardano conversion error:" <+> pretty e
        TxOutRefNotFound t             -> "Tx out reference not found:" <+> pretty t
        TxOutRefWrongType t            -> "Tx out reference wrong type:" <+> pretty t
        TxOutRefNoReferenceScript t    -> "Tx out reference does not contain a reference script:" <+> pretty t
        DatumNotFound h                -> "No datum with hash" <+> pretty h <+> "was found in lookups value"
        DeclaredInputMismatch v        -> "Discrepancy of" <+> pretty v <+> "inputs"
        DeclaredOutputMismatch v       -> "Discrepancy of" <+> pretty v <+> "outputs"
        MintingPolicyNotFound h        -> "No minting policy with hash" <+> pretty h <+> "was found"
        ScriptHashNotFound h           -> "No script with hash" <+> pretty h <+> "was found"
        TypedValidatorMissing          -> "Script instance is missing"
        DatumWrongHash h d             -> "Wrong hash for datum" <+> pretty d <> colon <+> pretty h
        CannotSatisfyAny               -> "Cannot satisfy any of the required constraints"
        NoMatchingOutputFound h        -> "No matching output found for validator hash" <+> pretty h
        MultipleMatchingOutputsFound h -> "Multiple matching outputs found for validator hash" <+> pretty h
        AmbiguousRedeemer t rs         -> "Try to spend a script output" <+> pretty t
                                       <+> "with different redeemers:" <+> pretty rs
        AmbiguousReferenceScript t rss -> "Try to spend a script output" <+> pretty t
                                       <+> "with different referenceScript:" <+> pretty rss

-- | Given a list of 'SomeLookupsAndConstraints' describing the constraints
--   for several scripts, build a single transaction that runs all the scripts.
mkSomeTx
    :: Params
    -> [SomeLookupsAndConstraints]
    -> Either MkTxError UnbalancedTx
mkSomeTx params xs =
    let process = \case
            SomeLookupsAndConstraints lookups constraints ->
                processLookupsAndConstraints lookups constraints
    in  fmap cpsUnbalancedTx
        $ runExcept
        $ execStateT (traverse process xs) (initialState params)

data SortedConstraints
   = MkSortedConstraints
   { rangeConstraints :: [POSIXTimeRange]
   , otherConstraints :: [TxConstraint]
   } deriving (Eq, Show)

-- | Filtering MustSpend constraints to ensure their consistency and check that we do not try to spend them
-- with different redeemer or reference scripts.
--
-- When:
--     - 2 or more MustSpendPubkeyOutput are defined for the same output, we only keep the first one
--     - 2 or more MustSpendScriptOutpt are defined for the same output:
--          - if they have different redeemer, we throw an 'AmbiguousRedeemer' error;
--          - if they provide more than one reference script we throw an 'AmbiguousReferenceScript' error;
--          - if only one define a reference script, we use that reference script.
cleaningMustSpendConstraints :: MonadError MkTxError m => [TxConstraint] -> m [TxConstraint]
cleaningMustSpendConstraints (x@(MustSpendScriptOutput t _ _):xs) = do
    let
        spendSame (MustSpendScriptOutput t' _ _) = t == t'
        spendSame _                              = False
        getRedeemer (MustSpendScriptOutput _ r _) = Just r
        getRedeemer _                             = Nothing
        getReferenceScript (MustSpendScriptOutput _ _ rs) = rs
        getReferenceScript _                              = Nothing
        (mustSpendSame, otherConstraints) = List.partition spendSame xs
        redeemers = Set.fromList $ mapMaybe getRedeemer (x:mustSpendSame)
        referenceScripts = Set.fromList $ mapMaybe getReferenceScript (x:mustSpendSame)
    red <- case Set.toList redeemers of
                []    -> throwError $ AmbiguousRedeemer t [] -- Can't happen as x must have a redeemer
                [red] -> pure red
                rs    -> throwError $ AmbiguousRedeemer t rs
    rs  <- case Set.toList referenceScripts of
                []  -> pure Nothing
                [r] -> pure $ Just r
                rs  -> throwError $ AmbiguousReferenceScript t rs
    (MustSpendScriptOutput t red rs:) <$> cleaningMustSpendConstraints otherConstraints
cleaningMustSpendConstraints (x@(MustSpendPubKeyOutput _):xs) =
    (x :) <$> cleaningMustSpendConstraints (filter (x /=) xs)
cleaningMustSpendConstraints [] = pure []
cleaningMustSpendConstraints (x:xs) = (x :) <$> cleaningMustSpendConstraints xs

prepareConstraints
    ::
    ( FromData (DatumType a)
    , ToData (DatumType a)
    , ToData (RedeemerType a)
    )
    => [ScriptInputConstraint (RedeemerType a)]
    -> [ScriptOutputConstraint (DatumType a)]
    -> [TxConstraint]
    -> ReaderT (ScriptLookups a) (StateT ConstraintProcessingState (Except MkTxError)) SortedConstraints
prepareConstraints ownInputs ownOutputs constraints = do
    let
      extractPosixTimeRange = \case
        MustValidateInTimeRange range -> Left $ toPlutusInterval range
        other                         -> Right other
      (ranges, _nonRangeConstraints) = partitionEithers $ extractPosixTimeRange <$> constraints
    ownInputConstraints <- traverse addOwnInput ownInputs
    ownOutputConstraints <- traverse addOwnOutput ownOutputs
    cleanedConstraints <- cleaningMustSpendConstraints constraints
    pure $ MkSortedConstraints ranges (cleanedConstraints <> ownOutputConstraints <> ownInputConstraints)


-- | Resolve some 'TxConstraints' by modifying the 'UnbalancedTx' in the
--   'ConstraintProcessingState'
processLookupsAndConstraints
    ::
    ( FromData (DatumType a)
    , ToData (DatumType a)
    , ToData (RedeemerType a)
    )
    => ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> StateT ConstraintProcessingState (Except MkTxError) ()
processLookupsAndConstraints lookups TxConstraints{txConstraints, txOwnInputs, txConstraintFuns = TxConstraintFuns txCnsFuns, txOwnOutputs} = do
        flip runReaderT lookups $ do
            sortedConstraints <- prepareConstraints txOwnInputs txOwnOutputs txConstraints
            traverse_ processConstraint (otherConstraints sortedConstraints)
            traverse_ processConstraintFun txCnsFuns
            checkValueSpent
            updateUtxoIndex
            lift $ setValidityRange (rangeConstraints sortedConstraints)

processConstraintFun
    :: TxConstraintFun
    -> ReaderT (ScriptLookups a) (StateT ConstraintProcessingState (Except MkTxError)) ()
processConstraintFun = \case
    MustSpendScriptOutputWithMatchingDatumAndValue vh datumPred valuePred red -> do
        ScriptLookups{slTxOutputs} <- ask
        -- TODO: Need to precalculate the validator hash or else this won't work
        -- with PlutusV2 validator. This means changing `DecoratedTxOut` to
        -- include the hash.
        let matches (Just (_, d, value)) = datumPred (getDatum d) && valuePred value
            matches Nothing              = False

        opts <- fmap (Map.toList . Map.filter matches)
                $ traverse resolveScriptTxOut
                $ Map.filter ((== Just vh) . preview Tx.decoratedTxOutValidatorHash) slTxOutputs
        case opts of
            [] -> throwError $ NoMatchingOutputFound vh
            [(ref, Just (validator, datum, value))] -> do
                mkWitness <- throwLeft ToCardanoError $ C.toCardanoTxInScriptWitnessHeader (getValidator <$> validator)
                txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn ref
                let witness
                        = C.ScriptWitness C.ScriptWitnessForSpending $
                            mkWitness
                            (C.toCardanoDatumWitness $ datumWitness datum)
                            (C.toCardanoScriptData (getRedeemer red))
                            C.zeroExecutionUnits

                unbalancedTx . tx . txIns <>= [(txIn, C.BuildTxWith witness)]

                valueSpentInputs <>= provided value
            _ -> throwError $ MultipleMatchingOutputsFound vh

data DatumWithOrigin
    = DatumInTx { getDatum :: Datum }
    | DatumInline { getDatum :: Datum }

datumWitness :: DatumWithOrigin -> Maybe Datum
datumWitness (DatumInTx d)   = Just d
datumWitness (DatumInline _) = Nothing

checkValueSpent
    :: ( MonadReader (ScriptLookups a) m
       , MonadState ConstraintProcessingState m
       , MonadError MkTxError m
       )
    => m ()
checkValueSpent = do
    missingInputs <- uses valueSpentInputs missingValueSpent
    unless (Value.isZero missingInputs) $ throwError $ DeclaredInputMismatch missingInputs
    missingOutputs <- uses valueSpentOutputs missingValueSpent
    unless (Value.isZero missingOutputs) $ throwError $ DeclaredOutputMismatch missingOutputs

-- | Reinject the validityRange inside the unbalanced Tx.
--   As the Tx is a Caradano transaction, and as we have access to the SlotConfig,
--   we can already internalize the constraints for the test
setValidityRange
    :: [POSIXTimeRange] -> StateT ConstraintProcessingState (Except MkTxError) ()
setValidityRange ranges = do
  slotConfig <- gets (pSlotConfig . cpsParams)
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
    -> ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> Either MkTxError UnbalancedTx
mkTx params lookups txc = mkSomeTx params [SomeLookupsAndConstraints lookups txc]

throwLeft :: (MonadState s m, MonadError err m) => (b -> err) -> Either b r -> m r
throwLeft f = either (throwError . f) pure

-- | Modify the 'UnbalancedTx' so that it satisfies the constraints, if
--   possible. Fails if a hash is missing from the lookups, or if an output
--   of the wrong type is spent.
processConstraint
    :: TxConstraint
    -> ReaderT (ScriptLookups a) (StateT ConstraintProcessingState (Except MkTxError)) ()
processConstraint = \case
    MustIncludeDatumInTxWithHash _ _ -> pure () -- always succeeds
    MustIncludeDatumInTx _ -> pure () -- always succeeds
    MustSpendPubKeyOutput txo -> do
        txout <- lookupTxOutRef txo
        value <- maybe (throwError (TxOutRefWrongType txo)) pure $ do
            guard $ is Tx._PublicKeyDecoratedTxOut txout
            pure $ txout ^. Tx.decoratedTxOutValue
        txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
        unbalancedTx . tx . txIns <>= [(txIn, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending))]
        valueSpentInputs <>= provided (C.fromCardanoValue value)

    MustBeSignedBy pk -> do
        ekw <-  either (throwError . ToCardanoError) pure $ C.toCardanoPaymentKeyHash pk
        unbalancedTx . tx . txExtraKeyWits <>= Set.singleton ekw
    MustSpendScriptOutput txo redeemer mref -> do
        txout <- lookupTxOutRef txo
        mkWitness <- case mref of
          Just ref -> do
            refTxOut <- lookupTxOutRef ref
            case refTxOut ^. Tx.decoratedTxOutReferenceScript of
                Just (Tx.Versioned _ lang) -> do
                    txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn ref
                    unbalancedTx . tx . txInsReference <>= [ txIn ]
                    throwLeft ToCardanoError $ C.toCardanoTxInReferenceWitnessHeader (Tx.Versioned ref lang)
                _ -> throwError (TxOutRefNoReferenceScript ref)
          Nothing -> do
            mscriptTXO <- resolveScriptTxOutValidator txout
            case mscriptTXO of
                Just validator ->
                    throwLeft ToCardanoError $ C.toCardanoTxInScriptWitnessHeader (getValidator <$> validator)
                _ -> throwError (TxOutRefWrongType txo)
        mscriptTXO <- resolveScriptTxOutDatumAndValue txout
        case mscriptTXO of
            Just (datum, value) -> do
                txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
                let witness
                        = C.ScriptWitness C.ScriptWitnessForSpending $
                            mkWitness
                            (C.toCardanoDatumWitness $ datumWitness datum)
                            (C.toCardanoScriptData (getRedeemer redeemer))
                            C.zeroExecutionUnits

                unbalancedTx . tx . txIns <>= [(txIn, C.BuildTxWith witness)]

                valueSpentInputs <>= provided value

            _ -> throwError (TxOutRefWrongType txo)

    MustUseOutputAsCollateral txo -> do
        txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
        unbalancedTx . tx . txInsCollateral <>= [ txIn ]

    MustReferenceOutput txo -> do
        txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn txo
        unbalancedTx . tx . txInsReference <>= [ txIn ]

    MustMintValue mpsHash red tn i mref -> do
        let value = Value.singleton (Value.mpsSymbol mpsHash) tn

        -- If i is negative we are burning tokens. The tokens burned must
        -- be provided as an input. So we add the value burnt to
        -- 'valueSpentInputs'. If i is positive then new tokens are created
        -- which must be added to 'valueSpentOutputs'.
        if i < 0
            then valueSpentInputs <>= provided (value (negate i))
            else valueSpentOutputs <>= provided (value i)

        v <- throwLeft ToCardanoError $ C.toCardanoValue $ value i
        pId <- throwLeft ToCardanoError $ toCardanoPolicyId mpsHash
        witness <- case mref of
            Just ref -> do
                refTxOut <- lookupTxOutRef ref
                case refTxOut ^? decoratedTxOutReferenceScript of
                    Just _ -> do
                      txIn <- throwLeft ToCardanoError $ C.toCardanoTxIn . Tx.txInputRef . Tx.pubKeyTxInput $ ref
                      unbalancedTx . tx . txInsReference <>= [txIn]
                      throwLeft ToCardanoError
                        $ toCardanoMintWitness red (flip Tx.Versioned PlutusV2 <$> mref) Nothing
                    _      -> throwError (TxOutRefNoReferenceScript ref)
            Nothing -> do
                mintingPolicyScript <- lookupMintingPolicy mpsHash
                throwLeft ToCardanoError
                  $ toCardanoMintWitness red Nothing (Just mintingPolicyScript)
        unbalancedTx . tx . txMintValue <>= (v, Map.singleton pId witness)

    MustPayToAddress addr md refScriptHashM vl -> do
        networkId <- use (paramsL . networkIdL)
        refScript <- lookupScriptAsReferenceScript refScriptHashM
        out <- throwLeft ToCardanoError $ C.TxOut
            <$> C.toCardanoAddressInEra networkId addr
            <*> fmap C.toCardanoTxOutValue (C.toCardanoValue vl)
            <*> pure (toTxOutDatum md)
            <*> pure refScript
        unbalancedTx . tx . txOuts <>= [ out ]

        valueSpentOutputs <>= provided vl

    MustSpendAtLeast vl -> valueSpentInputs <>= required vl
    MustProduceAtLeast vl -> valueSpentOutputs <>= required vl

    MustSatisfyAnyOf xs -> do
        s <- get
        let tryNext [] =
                throwError CannotSatisfyAny
            tryNext (hs:qs) = do
                traverse_ processConstraint hs `catchError` const (put s >> tryNext qs)
        tryNext xs

    MustValidateInTimeRange timeRange -> do
        slotConfig <- gets (pSlotConfig . cpsParams)
        unbTx <- use unbalancedTx
        let currentValRange = unbTx ^? tx . txValidityRange
        let currentTimeRange = maybe top (slotRangeToPOSIXTimeRange slotConfig . C.fromCardanoValidityRange) currentValRange
        let newRange = posixTimeRangeToContainedSlotRange slotConfig $ currentTimeRange /\ toPlutusInterval timeRange
        cTxTR <- throwLeft ToCardanoError $ C.toCardanoValidityRange newRange
        unbalancedTx . tx . txValidityRange .= cTxTR

-- | Add a typed input, checking the type of the output it spends. Return the value
--   of the spent output.
addOwnInput
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       , FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       )
    => ScriptInputConstraint (RedeemerType a)
    -> m TxConstraint
addOwnInput ScriptInputConstraint{icRedeemer, icTxOutRef, icReferenceTxOutRef} = do
    ScriptLookups{slTxOutputs, slTypedValidator} <- ask
    inst <- maybe (throwError TypedValidatorMissing) pure slTypedValidator
    typedOutRef <-
      either (throwError . TypeCheckFailed) pure
      $ runExcept @Typed.ConnectionError
      $ do
          (txOut, datum) <- maybe (throwError $ UnknownRef icTxOutRef) pure $ do
                                ciTxOut <- Map.lookup icTxOutRef slTxOutputs
                                datum <- ciTxOut ^? Tx.decoratedTxOutDatum . _2 . Tx.datumInDatumFromQuery
                                pure (Tx.toTxInfoTxOut ciTxOut, datum)
          Typed.typeScriptTxOutRef inst icTxOutRef txOut datum
    let red = Redeemer $ toBuiltinData icRedeemer
    pure $ MustSpendScriptOutput (Typed.tyTxOutRefRef typedOutRef) red icReferenceTxOutRef

-- | Convert a @ScriptOutputConstraint@ into a @TxConstraint@.
addOwnOutput
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       , ToData (DatumType a)
       )
    => ScriptOutputConstraint (DatumType a)
    -> m TxConstraint
addOwnOutput ScriptOutputConstraint{ocDatum, ocValue, ocReferenceScriptHash} = do
    ScriptLookups{slTypedValidator} <- ask
    inst <- maybe (throwError TypedValidatorMissing) pure slTypedValidator
    let dsV = fmap (Datum . toBuiltinData) ocDatum
    pure $ MustPayToAddress (validatorAddress inst) (Just dsV) ocReferenceScriptHash ocValue

lookupDatum
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => DatumHash
    -> m Datum
lookupDatum dvh =
    let err = throwError (DatumNotFound dvh) in
    asks slOtherData >>= maybe err pure . view (at dvh)

lookupValidator
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => ValidatorHash
    -> m (Versioned Validator)
lookupValidator (ValidatorHash vh) = fmap Validator <$> lookupScript (ScriptHash vh)

lookupScript
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => ScriptHash
    -> m (Versioned Script)
lookupScript sh =
    let err = throwError (ScriptHashNotFound sh) in
    asks slOtherScripts >>= maybe err pure . view (at sh)

lookupTxOutRef
    :: Tx.TxOutRef
    -> ReaderT (ScriptLookups a) (StateT ConstraintProcessingState (Except MkTxError)) Tx.DecoratedTxOut
lookupTxOutRef outRef =
    let err = throwError (TxOutRefNotFound outRef) in
    asks slTxOutputs >>= maybe err pure . view (at outRef)

lookupMintingPolicy
    :: MintingPolicyHash
    -> ReaderT (ScriptLookups a) (StateT ConstraintProcessingState (Except MkTxError)) (Versioned MintingPolicy)
lookupMintingPolicy (MintingPolicyHash mph) = fmap MintingPolicy <$> lookupScript (ScriptHash mph)

lookupScriptAsReferenceScript
    :: Maybe ScriptHash
    -> ReaderT (ScriptLookups a) (StateT ConstraintProcessingState (Except MkTxError)) (C.ReferenceScript C.BabbageEra)
lookupScriptAsReferenceScript msh = do
    mscript <- traverse lookupScript msh
    throwToCardanoError $ C.toCardanoReferenceScript mscript

resolveScriptTxOut
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => DecoratedTxOut -> m (Maybe (Versioned Validator, DatumWithOrigin, Value))
resolveScriptTxOut txo = do
    mv <- resolveScriptTxOutValidator txo
    mdv <- resolveScriptTxOutDatumAndValue txo
    pure $ (\v (d, value) -> (v, d, value)) <$> mv <*> mdv

resolveScriptTxOutValidator
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => DecoratedTxOut -> m (Maybe (Versioned Validator))
resolveScriptTxOutValidator
        Tx.ScriptDecoratedTxOut
            { Tx._decoratedTxOutValidator = v
            , Tx._decoratedTxOutValidatorHash = vh
            } = do
    -- first check in the 'DecoratedTxOut' for the validator, then
    -- look for it in the 'slOtherScripts' map.
    validator <- maybe (lookupValidator vh) pure v
    pure $ Just validator
resolveScriptTxOutValidator _ = pure Nothing

resolveScriptTxOutDatumAndValue
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => DecoratedTxOut -> m (Maybe (DatumWithOrigin, Value))
resolveScriptTxOutDatumAndValue
        Tx.ScriptDecoratedTxOut
            { Tx._decoratedTxOutScriptDatum = (dh, d)
            , Tx._decoratedTxOutValue
            } = do

    -- first check in the 'DecoratedTxOut' for the datum, then
    -- look for it in the 'slOtherData' map.
    datum <- case d of
        Tx.DatumUnknown      -> DatumInTx <$> lookupDatum dh
        Tx.DatumInBody datum -> pure (DatumInTx datum)
        Tx.DatumInline datum -> pure (DatumInline datum)
    pure $ Just (datum, C.fromCardanoValue _decoratedTxOutValue)
resolveScriptTxOutDatumAndValue _ = pure Nothing

throwToCardanoError :: MonadError MkTxError m => Either C.ToCardanoError a -> m a
throwToCardanoError (Left err) = throwError $ ToCardanoError err
throwToCardanoError (Right a)  = pure a

toTxOutDatum :: Maybe (TxOutDatum Datum) -> C.TxOutDatum C.CtxTx C.BabbageEra
toTxOutDatum = \case
    Nothing                   -> C.toCardanoTxOutNoDatum
    Just (TxOutDatumHash d)   -> C.toCardanoTxOutDatumHashFromDatum d
    Just (TxOutDatumInTx d)   -> C.toCardanoTxOutDatumInTx d
    Just (TxOutDatumInline d) -> C.toCardanoTxOutDatumInline d

-- | Turn a 'TxConstraints' value into an unbalanced transaction that satisfies
--   the constraints. To use this in a contract, see
--   'Plutus.Contract.submitTxConstraints'
--   and related functions.
mkTxWithParams
    :: ( FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       )
    => Params
    -> ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> Either MkTxError UnbalancedTx
mkTxWithParams params lookups txc = mkSomeTx params [SomeLookupsAndConstraints lookups txc]

-- | Each transaction output should contain a minimum amount of Ada (this is a
-- restriction on the real Cardano network).
adjustUnbalancedTx :: PParams -> UnbalancedTx -> Either Tx.ToCardanoError ([C.Lovelace], UnbalancedTx)
adjustUnbalancedTx params = alaf Compose (tx . txOuts . traverse) (fmap (\(l,out) -> (l, Tx.getTxOut out)) . adjustCardanoTxOut params . Tx.TxOut)

updateUtxoIndex
    :: ( MonadReader (ScriptLookups a) m
       , MonadState ConstraintProcessingState m
       , MonadError MkTxError m
       )
    => m ()
updateUtxoIndex = do
    ScriptLookups{slTxOutputs} <- ask
    networkId <- gets $ pNetworkId . cpsParams
    slUtxos <- traverse (throwToCardanoError . Tx.toTxOut networkId) slTxOutputs
    unbalancedTx . utxoIndex <>= slUtxos
