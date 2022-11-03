{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
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

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ledger.Constraints.OffChain(
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
    , ownPaymentPubKeyHash
    , ownStakePubKeyHash
    , paymentPubKey
    -- * Constraints resolution
    , SomeLookupsAndConstraints(..)
    , UnbalancedTx(..)
    , unBalancedTxTx
    , cardanoTx
    , tx
    , requiredSignatories
    , utxoIndex
    , emptyUnbalancedTx
    , adjustUnbalancedTx
    , adjustTxOut
    , MkTxError(..)
    , _TypeCheckFailed
    , _ToCardanoError
    , _TxOutRefNotFound
    , _TxOutRefWrongType
    , _TxOutRefNoReferenceScript
    , _DatumNotFound
    , _DatumNotFoundInTx
    , _MintingPolicyNotFound
    , _ScriptHashNotFound
    , _OwnPubKeyMissing
    , _TypedValidatorMissing
    , _DatumWrongHash
    , _CannotSatisfyAny
    , _NoMatchingOutputFound
    , _MultipleMatchingOutputsFound
    , mkTx
    , mkTxWithParams
    , mkSomeTx
    -- * Internals exposed for testing
    , ValueSpentBalances(..)
    , provided
    , required
    , missingValueSpent
    , ConstraintProcessingState(..)
    , unbalancedTx
    , valueSpentOutputs
    , paramsL
    , processConstraintFun
    , addOwnInput
    , addOwnOutput
    , addMissingValueSpent
    , updateUtxoIndex
    , lookupTxOutRef
    , lookupScript
    , lookupScriptAsReferenceScript
    , prepareConstraints
    , resolveScriptTxOut
    , resolveScriptTxOutValidator
    , resolveScriptTxOutDatumAndValue
    , DatumWithOrigin(..)
    , datumWitness
    ) where

import Cardano.Api qualified as C
import Control.Lens (_2, alaf, at, makeClassyPrisms, makeLensesFor, preview, view, (%=), (&), (.=), (.~), (<>=), (^.),
                     (^?))
import Control.Lens.Extras (is)
import Control.Monad (forM_, guard)
import Control.Monad.Except (MonadError (catchError, throwError), runExcept, unless)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get, put), execStateT, gets)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Functor.Compose (Compose (Compose))
import Data.List qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi.Schema qualified as OpenApi
import Data.Semigroup (First (First, getFirst))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Ledger (Redeemer (Redeemer), decoratedTxOutReferenceScript, outValue)
import Ledger.Ada qualified as Ada
import Ledger.Address (Address, PaymentPubKey (PaymentPubKey), PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash,
                       pubKeyHashAddress)
import Ledger.Address qualified as Address
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icRedeemer, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocReferenceScriptHash, ocValue),
                                         TxConstraint (MustBeSignedBy, MustIncludeDatumInTx, MustIncludeDatumInTxWithHash, MustMintValue, MustPayToOtherScript, MustPayToPubKeyAddress, MustProduceAtLeast, MustReferenceOutput, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustUseOutputAsCollateral, MustValidateIn),
                                         TxConstraintFun (MustSpendScriptOutputWithMatchingDatumAndValue),
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs),
                                         TxOutDatum (TxOutDatumHash, TxOutDatumInTx, TxOutDatumInline))
import Ledger.Crypto (pubKeyHash)
import Ledger.Index (minAdaTxOut)
import Ledger.Orphans ()
import Ledger.Params (Params (pNetworkId, pSlotConfig))
import Ledger.TimeSlot (posixTimeRangeToContainedSlotRange)
import Ledger.Tx (DecoratedTxOut, Language (PlutusV1, PlutusV2), ReferenceScript, TxOut (TxOut), TxOutRef,
                  Versioned (Versioned), txOutValue)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (Any, ConnectionError (UnknownRef), TypedValidator (tvValidator, tvValidatorHash),
                             ValidatorTypes (DatumType, RedeemerType))
import Ledger.Validation (evaluateMinLovelaceOutput, fromPlutusTxOut)
import Plutus.Script.Utils.Scripts qualified as P
import Plutus.Script.Utils.V2.Typed.Scripts qualified as Typed
import Plutus.V1.Ledger.Api (Datum (Datum), DatumHash, Validator (getValidator), Value, getMintingPolicy)
import Plutus.V1.Ledger.Scripts (MintingPolicy (MintingPolicy), MintingPolicyHash (MintingPolicyHash), Script,
                                 ScriptHash (ScriptHash), Validator (Validator), ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Value qualified as Value
import Plutus.V2.Ledger.Tx qualified as PV2
import PlutusTx (FromData, ToData (toBuiltinData))
import PlutusTx.Lattice (JoinSemiLattice ((\/)), MeetSemiLattice ((/\)))
import PlutusTx.Numeric qualified as N
import Prettyprinter (Pretty (pretty), colon, hang, vsep, (<+>))

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
        , slOwnStakePubKeyHash   :: Maybe StakePubKeyHash
        -- ^ The contract's stake public key hash (optional)
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
            , slOwnStakePubKeyHash =
                fmap getFirst $ (First <$> slOwnStakePubKeyHash l)
                             <> (First <$> slOwnStakePubKeyHash r)
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
mintingPolicy (fmap getMintingPolicy -> script) = mempty { slOtherScripts = Map.singleton (P.scriptHash script) script }

-- | A script lookups value with a PlutusV1 minting policy script.
plutusV1MintingPolicy :: MintingPolicy -> ScriptLookups a
plutusV1MintingPolicy pl = mintingPolicy (Versioned pl PlutusV1)

-- | A script lookups value with a PlutusV2 minting policy script.
plutusV2MintingPolicy :: MintingPolicy -> ScriptLookups a
plutusV2MintingPolicy pl = mintingPolicy (Versioned pl PlutusV2)

-- | A script lookups value with a versioned validator script.
otherScript :: Versioned Validator -> ScriptLookups a
otherScript (fmap getValidator -> script) = mempty { slOtherScripts = Map.singleton (P.scriptHash script) script }

-- | A script lookups value with a PlutusV1 validator script.
plutusV1OtherScript :: Validator -> ScriptLookups a
plutusV1OtherScript vl = otherScript (Versioned vl PlutusV1)

-- | A script lookups value with a PlutusV2 validator script.
plutusV2OtherScript :: Validator -> ScriptLookups a
plutusV2OtherScript vl = otherScript (Versioned vl PlutusV2)

-- | A script lookups value with a datum.
otherData :: Datum -> ScriptLookups a
otherData dt =
    let dh = P.datumHash dt in
    mempty { slOtherData = Map.singleton dh dt }

-- | A script lookups value with a payment public key
paymentPubKey :: PaymentPubKey -> ScriptLookups a
paymentPubKey (PaymentPubKey pk) =
    mempty { slPaymentPubKeyHashes = Set.singleton (PaymentPubKeyHash $ pubKeyHash pk) }

-- | A script lookups value with a payment public key hash.
--
-- If called multiple times, only the payment public key hash is kept:
--
-- @
-- ownPaymentPubKeyHash pkh1 <> ownPaymentPubKeyHash pkh2 <> ...
--     == ownPaymentPubKeyHash pkh1
-- @
ownPaymentPubKeyHash :: PaymentPubKeyHash -> ScriptLookups a
ownPaymentPubKeyHash pkh = mempty { slOwnPaymentPubKeyHash = Just pkh }

-- | A script lookups value with a stake public key hash.
--
-- If called multiple times, only the stake public key hash is kept:
--
-- @
-- ownStakePubKeyHash skh1 <> ownStakePubKeyHash skh2 <> ...
--     == ownStakePubKeyHash skh1
-- @
ownStakePubKeyHash :: StakePubKeyHash -> ScriptLookups a
ownStakePubKeyHash skh = mempty { slOwnStakePubKeyHash = Just skh }

-- | An unbalanced transaction. It needs to be balanced and signed before it
--   can be submitted to the ledger. See note [Submitting transactions from
--   Plutus contracts] in 'Plutus.Contract.Wallet'.
data UnbalancedTx
    = UnbalancedEmulatorTx
        { unBalancedEmulatorTx            :: Tx.Tx
        , unBalancedTxRequiredSignatories :: Set PaymentPubKeyHash
        -- ^ These are all the payment public keys that should be used to request the
        -- signatories from the user's wallet. The signatories are what is required to
        -- sign the transaction before submitting it to the blockchain. Transaction
        -- validation will fail if the transaction is not signed by the required wallet.
        , unBalancedTxUtxoIndex           :: Map TxOutRef TxOut
        -- ^ Utxo lookups that are used for adding inputs to the 'UnbalancedTx'.
        -- Simply refers to  'slTxOutputs' of 'ScriptLookups'.
        }
    | UnbalancedCardanoTx
        { unBalancedCardanoBuildTx        :: C.CardanoBuildTx
        , unBalancedTxRequiredSignatories :: Set PaymentPubKeyHash
        -- ^ These are all the payment public keys that should be used to request the
        -- signatories from the user's wallet. The signatories are what is required to
        -- sign the transaction before submitting it to the blockchain. Transaction
        -- validation will fail if the transaction is not signed by the required wallet.
        , unBalancedTxUtxoIndex           :: Map TxOutRef TxOut
        -- ^ Utxo lookups that are used for adding inputs to the 'UnbalancedTx'.
        -- Simply refers to  'slTxOutputs' of 'ScriptLookups'.
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

makeLensesFor
    [ ("unBalancedEmulatorTx", "tx")
    , ("unBalancedCardanoBuildTx", "cardanoTx")
    , ("unBalancedTxRequiredSignatories", "requiredSignatories")
    , ("unBalancedTxUtxoIndex", "utxoIndex")
    ] ''UnbalancedTx

unBalancedTxTx :: UnbalancedTx -> Either C.CardanoBuildTx Tx.Tx
unBalancedTxTx UnbalancedEmulatorTx{unBalancedEmulatorTx}    = Right unBalancedEmulatorTx
unBalancedTxTx UnbalancedCardanoTx{unBalancedCardanoBuildTx} = Left unBalancedCardanoBuildTx

emptyUnbalancedTx :: UnbalancedTx
emptyUnbalancedTx = UnbalancedEmulatorTx mempty mempty mempty

instance Pretty UnbalancedTx where
    pretty (UnbalancedEmulatorTx utx rs utxo) =
        vsep
        [ hang 2 $ vsep ["Tx:", pretty utx]
        , hang 2 $ vsep $ "Requires signatures:" : (pretty <$> Set.toList rs)
        , hang 2 $ vsep $ "Utxo index:" : (pretty <$> Map.toList utxo)
        ]
    pretty (UnbalancedCardanoTx utx rs utxo) =
        vsep
        [ hang 2 $ vsep ["Tx (cardano-api Representation):", pretty utx]
        , hang 2 $ vsep $ "Requires signatures:" : (pretty <$> Set.toList rs)
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

totalMissingValue :: ConstraintProcessingState -> Value
totalMissingValue ConstraintProcessingState{cpsValueSpentBalancesInputs, cpsValueSpentBalancesOutputs} =
        missingValueSpent cpsValueSpentBalancesInputs \/
        missingValueSpent cpsValueSpentBalancesOutputs

makeLensesFor
    [ ("cpsUnbalancedTx", "unbalancedTx")
    , ("cpsMintRedeemers", "mintRedeemers")
    , ("cpsValueSpentBalancesInputs", "valueSpentInputs")
    , ("cpsValueSpentBalancesOutputs", "valueSpentOutputs")
    , ("cpsParams", "paramsL")
    ] ''ConstraintProcessingState

initialState :: Params -> ConstraintProcessingState
initialState params = ConstraintProcessingState
    { cpsUnbalancedTx = emptyUnbalancedTx
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
    in fmap cpsUnbalancedTx
        $ runExcept
        $ execStateT (traverse process xs) (initialState params)

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
    :: ( ToData (DatumType a)
       , MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => [ScriptOutputConstraint (DatumType a)]
    -> [TxConstraint]
    -> m ([TxConstraint], [TxConstraint])
prepareConstraints ownOutputs constraints = do
    let
      -- This is done so that the 'MustIncludeDatumInTxWithHash' and
      -- 'MustIncludeDatumInTx' are not sensitive to the order of the
      -- constraints. @mustPayToOtherScript ... <> mustIncludeDatumInTx ...@
      -- and @mustIncludeDatumInTx ... <> mustPayToOtherScript ...@
      -- must yield the same behavior.
      isVerificationConstraints = \case
        MustIncludeDatumInTxWithHash {} -> True
        MustIncludeDatumInTx {}         -> True
        _                               -> False
      (verificationConstraints, otherConstraints) =
          List.partition isVerificationConstraints constraints
    ownOutputConstraints <- concat <$> traverse addOwnOutput ownOutputs
    cleantConstraints <- cleaningMustSpendConstraints otherConstraints
    pure (cleantConstraints <> ownOutputConstraints, verificationConstraints)

-- | Resolve some 'TxConstraints' by modifying the 'UnbalancedTx' in the
--   'ConstraintProcessingState'
processLookupsAndConstraints
    :: ( FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       , MonadState ConstraintProcessingState m
       , MonadError MkTxError m
       )
    => ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> m ()
processLookupsAndConstraints lookups TxConstraints{txConstraints, txOwnInputs, txOwnOutputs, txConstraintFuns = TxConstraintFuns txCnsFuns } =
    flip runReaderT lookups $ do
         (constraints, verificationConstraints) <- prepareConstraints txOwnOutputs txConstraints
         traverse_ processConstraint constraints
         traverse_ processConstraintFun txCnsFuns
         traverse_ addOwnInput txOwnInputs
         traverse_ processConstraint verificationConstraints
         addMissingValueSpent
         updateUtxoIndex

-- | Turn a 'TxConstraints' value into an unbalanced transaction that satisfies
--   the constraints. To use this in a contract, see
--   'Plutus.Contract.submitTxConstraints'
--   and related functions.
--   Uses default 'Params' which is probably not what you want, use 'mkTxWithParams' instead.
{-# DEPRECATED mkTx "Use mkTxWithParams instead" #-}
mkTx
    :: ( FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       )
    => ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> Either MkTxError UnbalancedTx
mkTx = mkTxWithParams def

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
adjustUnbalancedTx :: Params -> UnbalancedTx -> Either Tx.ToCardanoError ([Ada.Ada], UnbalancedTx)
adjustUnbalancedTx params = alaf Compose (tx . Tx.outputs . traverse) (adjustTxOut params)

-- | Adjust a single transaction output so it contains at least the minimum amount of Ada
-- and return the adjustment (if any) and the updated TxOut.
adjustTxOut :: Params -> TxOut -> Either Tx.ToCardanoError ([Ada.Ada], TxOut)
adjustTxOut params txOut = do
    -- Increasing the ada amount can also increase the size in bytes, so start with a rough estimated amount of ada
    withMinAdaValue <- C.toCardanoTxOutValue $ txOutValue txOut <> Ada.toValue minAdaTxOut
    let txOutEstimate = txOut & outValue .~ withMinAdaValue
        minAdaTxOut' = evaluateMinLovelaceOutput params (fromPlutusTxOut txOutEstimate)
        missingLovelace = minAdaTxOut' - Ada.fromValue (txOutValue txOut)
    if missingLovelace > 0
    then do
      adjustedLovelace <- C.toCardanoTxOutValue $ txOutValue txOut <> Ada.toValue missingLovelace
      pure ([missingLovelace], txOut & outValue .~ adjustedLovelace)
    else pure ([], txOut)


-- | Add the remaining balance of the total value that the tx must spend.
--   See note [Balance of value spent]
addMissingValueSpent
    :: ( MonadReader (ScriptLookups a) m
       , MonadState ConstraintProcessingState m
       , MonadError MkTxError m
       )
    => m ()
addMissingValueSpent = do
    missing <- gets totalMissingValue
    if Value.isZero missing
        then pure ()
        else do
            -- add 'missing' to the transaction's outputs. This ensures that the
            -- wallet will add a corresponding input when balancing the
            -- transaction.
            -- Step 4 of the process described in [Balance of value spent]
            pkh <- asks slOwnPaymentPubKeyHash >>= maybe (throwError OwnPubKeyMissing) pure
            skh <- asks slOwnStakePubKeyHash
            let pv2TxOut = PV2.TxOut { PV2.txOutAddress = pubKeyHashAddress pkh skh
                                     , PV2.txOutValue = missing
                                     , PV2.txOutDatum = PV2.NoOutputDatum
                                     , PV2.txOutReferenceScript = Nothing
                                     }
            txOut <- toCardanoTxOutWithOutputDatum pv2TxOut
            unbalancedTx . tx . Tx.outputs %= (txOut:)

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

-- | Add a typed input, checking the type of the output it spends. Return the value
--   of the spent output.
addOwnInput
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       , MonadState ConstraintProcessingState m
       , FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       )
    => ScriptInputConstraint (RedeemerType a)
    -> m ()
addOwnInput ScriptInputConstraint{icRedeemer, icTxOutRef} = do
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
    let vl = PV2.txOutValue $ Typed.tyTxOutTxOut $ Typed.tyTxOutRefOut typedOutRef
    valueSpentInputs <>= provided vl
    case typedOutRef of
        Typed.TypedScriptTxOutRef{Typed.tyTxOutRefRef, Typed.tyTxOutRefOut} -> do
            let datum = Datum $ toBuiltinData $ Typed.tyTxOutData tyTxOutRefOut
            unbalancedTx . tx %= Tx.addScriptTxInput
                                      tyTxOutRefRef
                                      (Typed.vValidatorScript inst)
                                      (Redeemer $ toBuiltinData icRedeemer)
                                      (Just datum)

-- | Convert a @ScriptOutputConstraint@ into a @TxConstraint@.
addOwnOutput
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       , ToData (DatumType a)
       )
    => ScriptOutputConstraint (DatumType a)
    -> m [TxConstraint]
addOwnOutput ScriptOutputConstraint{ocDatum, ocValue, ocReferenceScriptHash} = do
    ScriptLookups{slTypedValidator} <- ask
    inst <- maybe (throwError TypedValidatorMissing) pure slTypedValidator
    let dsV = fmap (Datum . toBuiltinData) ocDatum
    pure [ MustPayToOtherScript (tvValidatorHash inst) Nothing dsV ocReferenceScriptHash ocValue ]

lookupTxOutRef
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => TxOutRef
    -> m DecoratedTxOut
lookupTxOutRef outRef =
    let err = throwError (TxOutRefNotFound outRef) in
    asks slTxOutputs >>= maybe err pure . view (at outRef)

lookupDatum
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => DatumHash
    -> m Datum
lookupDatum dvh =
    let err = throwError (DatumNotFound dvh) in
    asks slOtherData >>= maybe err pure . view (at dvh)

lookupMintingPolicy
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => MintingPolicyHash
    -> m (Versioned MintingPolicy)
lookupMintingPolicy (MintingPolicyHash mph) = fmap MintingPolicy <$> lookupScript (ScriptHash mph)

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

lookupScriptAsReferenceScript
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => Maybe ScriptHash
    -> m ReferenceScript
lookupScriptAsReferenceScript msh = do
    mscript <- traverse lookupScript msh
    throwToCardanoError $ C.toCardanoReferenceScript mscript

-- | Modify the 'UnbalancedTx' so that it satisfies the constraints, if
--   possible. Fails if a hash is missing from the lookups, or if an output
--   of the wrong type is spent.
processConstraint
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       , MonadState ConstraintProcessingState m
       )
    => TxConstraint
    -> m ()
processConstraint = \case
    MustIncludeDatumInTxWithHash dvh dv -> do
        let dvHash = P.datumHash dv
        unless (dvHash == dvh)
            (throwError $ DatumWrongHash dvh dv)
        datums <- gets $ view (unbalancedTx . tx . Tx.datumWitnesses)
        unless (dvHash `elem` Map.keys datums)
            (throwError $ DatumNotFoundInTx dvHash)
    MustIncludeDatumInTx dv -> do
        datums <- gets $ view (unbalancedTx . tx . Tx.datumWitnesses)
        let dvHash = P.datumHash dv
        unless (dvHash `elem` Map.keys datums)
            (throwError $ DatumNotFoundInTx dvHash)
    MustValidateIn timeRange -> do
        slotRange <-
            gets ( flip posixTimeRangeToContainedSlotRange timeRange
                 . pSlotConfig
                 . cpsParams
                 )
        unbalancedTx . tx . Tx.validRange %= (slotRange /\)
    MustBeSignedBy pk ->
        unbalancedTx . requiredSignatories <>= Set.singleton pk
    MustSpendAtLeast vl -> valueSpentInputs <>= required vl
    MustProduceAtLeast vl -> valueSpentOutputs <>= required vl

    MustSpendPubKeyOutput txo -> do
        txout <- lookupTxOutRef txo
        value <- maybe (throwError (TxOutRefWrongType txo)) pure $ do
                       guard $ is Tx._PublicKeyDecoratedTxOut txout
                       pure $ txout ^. Tx.decoratedTxOutValue
        -- TODO: Add the optional datum in the witness set for the pub key output
        unbalancedTx . tx . Tx.inputs %= (Tx.pubKeyTxInput txo :)
        valueSpentInputs <>= provided value

    MustSpendScriptOutput txo red mref -> do
        txout <- lookupTxOutRef txo
        mDatumAndValue <- resolveScriptTxOutDatumAndValue txout
        (datum, value) <- maybe (throwError (TxOutRefWrongType txo)) pure mDatumAndValue
        valueSpentInputs <>= provided value
        case mref of
          Just ref -> do
            refTxOut <- lookupTxOutRef ref
            case refTxOut ^. Tx.decoratedTxOutReferenceScript  of
                Just val -> do
                    unbalancedTx . tx %= Tx.addReferenceTxInput txo (ref <$ val) red (datumWitness datum)
                    unbalancedTx . tx . Tx.referenceInputs <>= [Tx.pubKeyTxInput ref]
                _        -> throwError (TxOutRefNoReferenceScript ref)
          Nothing -> do
            mscriptTXO <- resolveScriptTxOutValidator txout
            case mscriptTXO of
                Just val -> do
                    unbalancedTx . tx %= Tx.addScriptTxInput txo val red (datumWitness datum)
                _             -> throwError (TxOutRefWrongType txo)

    MustUseOutputAsCollateral txo -> do
        unbalancedTx . tx . Tx.collateralInputs <>= [Tx.pubKeyTxInput txo]
    MustReferenceOutput txo -> do
        unbalancedTx . tx . Tx.referenceInputs <>= [Tx.pubKeyTxInput txo]

    MustMintValue mpsHash@(MintingPolicyHash mpsHashBytes) red tn i mref -> do
        -- See note [Mint and Fee fields must have ada symbol].
        let value = (<>) (Ada.lovelaceValueOf 0) . Value.singleton (Value.mpsSymbol mpsHash) tn
        -- If i is negative we are burning tokens. The tokens burned must
        -- be provided as an input. So we add the value burnt to
        -- 'valueSpentInputs'. If i is positive then new tokens are created
        -- which must be added to 'valueSpentOutputs'.
        if i < 0
            then valueSpentInputs <>= provided (value (negate i))
            else valueSpentOutputs <>= provided (value i)

        unbalancedTx . tx . Tx.mintScripts %= Map.insert mpsHash (red, flip Versioned PlutusV2 <$> mref)
        unbalancedTx . tx . Tx.mint <>= value i

        case mref of
            Just ref -> do
                refTxOut <- lookupTxOutRef ref
                case refTxOut ^? decoratedTxOutReferenceScript of
                    Just _ -> unbalancedTx . tx . Tx.referenceInputs <>= [Tx.pubKeyTxInput ref]
                    _      -> throwError (TxOutRefNoReferenceScript ref)
            Nothing -> do
                mintingPolicyScript <- lookupMintingPolicy mpsHash
                unbalancedTx . tx . Tx.scriptWitnesses %= Map.insert (ScriptHash mpsHashBytes) (fmap getMintingPolicy mintingPolicyScript)


    MustPayToPubKeyAddress pk skhM mdv refScriptHashM vl -> do
        forM_ mdv $ \case
            TxOutDatumInTx d -> do
                let theHash = P.datumHash d
                unbalancedTx . tx . Tx.datumWitnesses . at theHash .= Just d
            _ -> pure ()

        let addr = pubKeyHashAddress pk skhM
        refScript <- lookupScriptAsReferenceScript refScriptHashM
        txOut <- mkCardanoTxOut addr vl mdv refScript
        unbalancedTx . tx . Tx.outputs <>= [txOut]

        valueSpentOutputs <>= provided vl

    MustPayToOtherScript vlh svhM dv refScriptHashM vl -> do
        case dv of
            TxOutDatumInTx d -> do
                let theHash = P.datumHash d
                unbalancedTx . tx . Tx.datumWitnesses . at theHash .= Just d
            _ -> pure ()

        let addr = Address.scriptValidatorHashAddress vlh svhM
        refScript <- lookupScriptAsReferenceScript refScriptHashM
        txOut <- mkCardanoTxOut addr vl (Just dv) refScript
        unbalancedTx . tx . Tx.outputs <>= [txOut]

        valueSpentOutputs <>= provided vl

    MustSatisfyAnyOf xs -> do
        s <- get
        let tryNext [] =
                throwError CannotSatisfyAny
            tryNext (hs:qs) = do
                traverse_ processConstraint hs `catchError` const (put s >> tryNext qs)
        tryNext xs

processConstraintFun
    :: ( MonadReader (ScriptLookups a) m
        , MonadError MkTxError m
        , MonadState ConstraintProcessingState m
        )
    => TxConstraintFun
    -> m ()
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
                unbalancedTx . tx %= Tx.addScriptTxInput ref validator red (datumWitness datum)
                valueSpentInputs <>= provided value
            _ -> throwError $ MultipleMatchingOutputsFound vh

data DatumWithOrigin
    = DatumInTx { getDatum :: Datum }
    | DatumInline { getDatum :: Datum }

datumWitness :: DatumWithOrigin -> Maybe Datum
datumWitness (DatumInTx d)   = Just d
datumWitness (DatumInline _) = Nothing

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
    pure $ Just (datum, _decoratedTxOutValue)
resolveScriptTxOutDatumAndValue _ = pure Nothing

toCardanoTxOutWithOutputDatum ::
    ( MonadState ConstraintProcessingState m
    , MonadError MkTxError m
    )
    => PV2.TxOut
    -> m TxOut
toCardanoTxOutWithOutputDatum txout = do
  networkId <- gets $ pNetworkId . cpsParams
  throwToCardanoError $ TxOut <$> C.toCardanoTxOut networkId C.toCardanoTxOutDatum txout

throwToCardanoError :: MonadError MkTxError m => Either C.ToCardanoError a -> m a
throwToCardanoError (Left err) = throwError $ ToCardanoError err
throwToCardanoError (Right a)  = pure a

mkCardanoTxOut ::
    ( MonadState ConstraintProcessingState m
    , MonadError MkTxError m
    )
    => Address
    -> Value
    -> Maybe (TxOutDatum Datum)
    -> ReferenceScript
    -> m TxOut
mkCardanoTxOut addr value mTxOutDatum refScript = do
  networkId <- gets $ pNetworkId . cpsParams
  let cardanoTxOut =
          fmap TxOut $
              C.TxOut <$> C.toCardanoAddressInEra networkId addr
                      <*> C.toCardanoTxOutValue value
                      <*> pure (toTxOutDatum mTxOutDatum)
                      <*> pure refScript

  case cardanoTxOut of
    Left err     -> throwError $ ToCardanoError err
    Right cTxOut -> pure cTxOut

toTxOutDatum :: Maybe (TxOutDatum Datum) -> C.TxOutDatum C.CtxTx C.BabbageEra
toTxOutDatum = \case
    Nothing                   -> C.toCardanoTxOutNoDatum
    Just (TxOutDatumHash d)   -> C.toCardanoTxOutDatumHashFromDatum d
    Just (TxOutDatumInTx d)   -> C.toCardanoTxOutDatumInTx d
    Just (TxOutDatumInline d) -> C.toCardanoTxOutDatumInline d

data MkTxError =
    TypeCheckFailed Typed.ConnectionError
    | ToCardanoError C.ToCardanoError
    | TxOutRefNotFound TxOutRef
    | TxOutRefWrongType TxOutRef
    | TxOutRefNoReferenceScript TxOutRef
    | DatumNotFound DatumHash
    | DatumNotFoundInTx DatumHash
    | MintingPolicyNotFound MintingPolicyHash
    | ScriptHashNotFound ScriptHash
    | OwnPubKeyMissing
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
        DatumNotFoundInTx h            -> "No datum with hash" <+> pretty h <+> "was found in the transaction body"
        MintingPolicyNotFound h        -> "No minting policy with hash" <+> pretty h <+> "was found"
        ScriptHashNotFound h           -> "No script with hash" <+> pretty h <+> "was found"
        OwnPubKeyMissing               -> "Own public key is missing"
        TypedValidatorMissing          -> "Script instance is missing"
        DatumWrongHash h d             -> "Wrong hash for datum" <+> pretty d <> colon <+> pretty h
        CannotSatisfyAny               -> "Cannot satisfy any of the required constraints"
        NoMatchingOutputFound h        -> "No matching output found for validator hash" <+> pretty h
        MultipleMatchingOutputsFound h -> "Multiple matching outputs found for validator hash" <+> pretty h
        AmbiguousRedeemer t rs         -> "Try to spend a script output" <+> pretty t <+> "with different redeemers:" <+> pretty rs
        AmbiguousReferenceScript t rss -> "Try to spend a script output" <+> pretty t <+> "with different referenceScript:" <+> pretty rss

