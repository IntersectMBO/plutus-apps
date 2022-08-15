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
module Ledger.Constraints.OffChain(
    -- * Lookups
    ScriptLookups(..)
    , plutusV1TypedValidatorLookups
    , generalise
    , unspentOutputs
    , plutusV1MintingPolicy
    , plutusV2MintingPolicy
    , plutusV1OtherScript
    , plutusV2OtherScript
    , otherData
    , ownPaymentPubKeyHash
    , ownStakePubKeyHash
    , paymentPubKey
    -- * Constraints resolution
    , SomeLookupsAndConstraints(..)
    , UnbalancedTx(..)
    , cardanoTx
    , tx
    , requiredSignatories
    , utxoIndex
    , validityTimeRange
    , emptyUnbalancedTx
    , adjustUnbalancedTx
    , adjustTxOut
    , MkTxError(..)
    , mkTx
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
    , addMintingRedeemers
    , addMissingValueSpent
    , updateUtxoIndex
    , lookupTxOutRef
    , resolveScriptTxOut
    ) where

import Control.Lens (Traversal', _2, _Just, _Right, alaf, at, iforM_, makeLensesFor, use, view, (%=), (.=), (<>=), (^?))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (catchError, throwError), runExcept, unless)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get, put), execStateT, gets)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default (def)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Functor.Compose (Compose (Compose))
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.OpenApi.Schema qualified as OpenApi
import Data.Semigroup (First (First, getFirst))
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), colon, hang, vsep, (<+>))

import Data.Maybe (fromJust)
import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPubKey (PaymentPubKey), PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash,
                       pubKeyHashAddress)
import Ledger.Address qualified as Address
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icRedeemer, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocValue),
                                         TxConstraint (MustBeSignedBy, MustHashDatum, MustIncludeDatum, MustMintValue, MustPayToOtherScript, MustPayToPubKeyAddress, MustProduceAtLeast, MustReferenceOutput, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustUseOutputAsCollateral, MustValidateIn),
                                         TxConstraintFun (MustSpendScriptOutputWithMatchingDatumAndValue),
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs))
import Ledger.Crypto (pubKeyHash)
import Ledger.Index (minAdaTxOut)
import Ledger.Orphans ()
import Ledger.Params (Params)
import Ledger.Tx (ChainIndexTxOut, Language (PlutusV1, PlutusV2), RedeemerPtr (RedeemerPtr), ScriptTag (Mint, Spend),
                  Tx, TxOut (txOutAddress, txOutDatumHash, txOutValue), TxOutRef)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (Any, ConnectionError (UnknownRef), TypedValidator,
                             ValidatorTypes (DatumType, RedeemerType))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Typed.Scripts qualified as Typed
import Ledger.Validation (evaluateMinLovelaceOutput, fromPlutusTxOutUnsafe)
import Plutus.Script.Utils.Scripts qualified as P
import Plutus.Script.Utils.V1.Scripts qualified as PV1
import Plutus.Script.Utils.V1.Tx (scriptAddressTxOut)
import Plutus.Script.Utils.V2.Scripts qualified as PV2
import Plutus.V1.Ledger.Api (Datum (Datum), DatumHash, MintingPolicy, MintingPolicyHash, POSIXTimeRange, Redeemer,
                             Validator, ValidatorHash, Value)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx (FromData, ToData (toBuiltinData))
import PlutusTx.Lattice (BoundedMeetSemiLattice (top), JoinSemiLattice ((\/)), MeetSemiLattice ((/\)))
import PlutusTx.Numeric qualified as N

data ScriptLookups a =
    ScriptLookups
        { slMPS                    :: Map MintingPolicyHash MintingPolicy
        -- ^ Minting policies that the script interacts with
        , slTxOutputs              :: Map TxOutRef ChainIndexTxOut
        -- ^ Unspent outputs that the script may want to spend
        , slOtherScripts           :: Map ValidatorHash (Validator, Language)
        -- ^ Validators of scripts other than "our script"
        , slOtherData              :: Map DatumHash Datum
        -- ^ Datums that we might need
        , slPaymentPubKeyHashes    :: Set PaymentPubKeyHash
        -- ^ Public keys that we might need
        , slTypedPlutusV1Validator :: Maybe (TypedValidator a)
        -- ^ The script instance with the typed validator hash & actual compiled program
        , slOwnPaymentPubKeyHash   :: Maybe PaymentPubKeyHash
        -- ^ The contract's payment public key hash, used for depositing tokens etc.
        , slOwnStakePubKeyHash     :: Maybe StakePubKeyHash
        -- ^ The contract's stake public key hash (optional)
        } deriving stock (Show, Generic)
          deriving anyclass (ToJSON, FromJSON)

generalise :: ScriptLookups a -> ScriptLookups Any
generalise sl =
    let validator = fmap Scripts.generalise (slTypedPlutusV1Validator sl)
    in sl{slTypedPlutusV1Validator = validator}

instance Semigroup (ScriptLookups a) where
    l <> r =
        ScriptLookups
            { slMPS = slMPS l <> slMPS r
            , slTxOutputs = slTxOutputs l <> slTxOutputs r
            , slOtherScripts = slOtherScripts l <> slOtherScripts r
            , slOtherData = slOtherData l <> slOtherData r
            , slPaymentPubKeyHashes = slPaymentPubKeyHashes l <> slPaymentPubKeyHashes r
            -- 'First' to match the semigroup instance of Map (left-biased)
            , slTypedPlutusV1Validator = fmap getFirst $ (First <$> slTypedPlutusV1Validator l) <> (First <$> slTypedPlutusV1Validator r)
            , slOwnPaymentPubKeyHash =
                fmap getFirst $ (First <$> slOwnPaymentPubKeyHash l)
                             <> (First <$> slOwnPaymentPubKeyHash r)
            , slOwnStakePubKeyHash =
                fmap getFirst $ (First <$> slOwnStakePubKeyHash l)
                             <> (First <$> slOwnStakePubKeyHash r)
            }

instance Monoid (ScriptLookups a) where
    mappend = (<>)
    mempty  = ScriptLookups mempty mempty mempty mempty mempty Nothing Nothing Nothing

-- | A script lookups value with a script instance. For convenience this also
--   includes the minting policy script that forwards all checks to the
--   instance's validator.
--
-- If called multiple times, only the first typed validator is kept:
--
-- @
-- plutusV1TypedValidatorLookups tv1 <> plutusV1TypedValidatorLookups tv2 <> ...
--     == plutusV1TypedValidatorLookups tv1
-- @
plutusV1TypedValidatorLookups :: TypedValidator a -> ScriptLookups a
plutusV1TypedValidatorLookups inst =
    mempty
        { slMPS = Map.singleton (Scripts.forwardingMintingPolicyHash inst) (Scripts.forwardingMintingPolicy inst)
        , slTypedPlutusV1Validator = Just inst
        }

-- | A script lookups value that uses the map of unspent outputs to resolve
--   input constraints.
unspentOutputs :: Map TxOutRef ChainIndexTxOut -> ScriptLookups a
unspentOutputs mp = mempty { slTxOutputs = mp }

-- | A script lookups value with a minting policy script.
plutusV1MintingPolicy :: MintingPolicy -> ScriptLookups a
plutusV1MintingPolicy pl =
    let hsh = PV1.mintingPolicyHash pl in
    mempty { slMPS = Map.singleton hsh pl }

-- | A script lookups value with a minting policy script.
plutusV2MintingPolicy :: MintingPolicy -> ScriptLookups a
plutusV2MintingPolicy pl =
    let hsh = PV2.mintingPolicyHash pl in
    mempty { slMPS = Map.singleton hsh pl }

-- | A script lookups value with a PlutusV1 validator script.
plutusV1OtherScript :: Validator -> ScriptLookups a
plutusV1OtherScript vl =
    let vh = PV1.validatorHash vl in
    mempty { slOtherScripts = Map.singleton vh (vl, PlutusV1) }

-- | A script lookups value with a PlutusV2 validator script.
plutusV2OtherScript :: Validator -> ScriptLookups a
plutusV2OtherScript vl =
    let vh = PV2.validatorHash vl in
    mempty { slOtherScripts = Map.singleton vh (vl, PlutusV2) }

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
data UnbalancedTx =
    UnbalancedTx
        { unBalancedTxTx                  :: Either C.CardanoBuildTx Tx.Tx
        , unBalancedTxRequiredSignatories :: Set PaymentPubKeyHash
        -- ^ These are all the payment public keys that should be used to request the
        -- signatories from the user's wallet. The signatories are what is required to
        -- sign the transaction before submitting it to the blockchain. Transaction
        -- validation will fail if the transaction is not signed by the required wallet.
        , unBalancedTxUtxoIndex           :: Map TxOutRef TxOut
        -- ^ Utxo lookups that are used for adding inputs to the 'UnbalancedTx'.
        -- Simply refers to  'slTxOutputs' of 'ScriptLookups'.
        , unBalancedTxValidityTimeRange   :: POSIXTimeRange
        -- ^ The reason this is a separate field instead of setting the
        -- 'Plutus.txValidRange' of 'Plutus.Tx' is because the 'Plutus.txValidRange' is
        -- specified as a 'SlotRange', but the user must specify the validity
        -- range in terms of 'POSIXTimeRange' instead. Thus, before submitting
        -- this transaction to the blockchain, we must convert this
        -- 'POSIXTimeRange' to 'SlotRange' using a 'SlotConfig'. See
        -- 'Plutus.Contract.Wallet.finalize'.
        }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON, OpenApi.ToSchema)

makeLensesFor
    [ ("unBalancedTxTx", "cardanoTx")
    , ("unBalancedTxRequiredSignatories", "requiredSignatories")
    , ("unBalancedTxUtxoIndex", "utxoIndex")
    , ("unBalancedTxValidityTimeRange", "validityTimeRange")
    ] ''UnbalancedTx

tx :: Traversal' UnbalancedTx Tx
tx = cardanoTx . _Right

emptyUnbalancedTx :: UnbalancedTx
emptyUnbalancedTx = UnbalancedTx (Right mempty) mempty mempty top

instance Pretty UnbalancedTx where
    pretty (UnbalancedTx utx rs utxo vr) =
        vsep
        [ hang 2 $ vsep ["Tx:", either pretty pretty utx]
        , hang 2 $ vsep $ "Requires signatures:" : (pretty <$> Set.toList rs)
        , hang 2 $ vsep $ "Utxo index:" : (pretty <$> Map.toList utxo)
        , hang 2 $ vsep ["Validity range:", pretty vr]
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
        , cpsMintRedeemers             :: Map.Map MintingPolicyHash Redeemer
        -- ^ Redeemers for minting policies.
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

initialState :: ConstraintProcessingState
initialState = ConstraintProcessingState
    { cpsUnbalancedTx = emptyUnbalancedTx
    , cpsMintRedeemers = mempty
    , cpsValueSpentBalancesInputs = ValueSpentBalances mempty mempty
    , cpsValueSpentBalancesOutputs = ValueSpentBalances mempty mempty
    , cpsParams = def -- cpsParams is not used here, only in plutus-tx-constraints
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
    :: [SomeLookupsAndConstraints]
    -> Either MkTxError UnbalancedTx
mkSomeTx xs =
    let process = \case
            SomeLookupsAndConstraints lookups constraints ->
                processLookupsAndConstraints lookups constraints
    in fmap cpsUnbalancedTx
        $ runExcept
        $ execStateT (traverse process xs) initialState

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
            traverse_ processConstraint txConstraints
            traverse_ processConstraintFun txCnsFuns
            traverse_ addOwnInput txOwnInputs
            traverse_ addOwnOutput txOwnOutputs
            addMintingRedeemers
            addMissingValueSpent
            updateUtxoIndex

-- | Turn a 'TxConstraints' value into an unbalanced transaction that satisfies
--   the constraints. To use this in a contract, see
--   'Plutus.Contract.submitTxConstraints'
--   and related functions.
mkTx
    :: ( FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       )
    => ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> Either MkTxError UnbalancedTx
mkTx lookups txc = mkSomeTx [SomeLookupsAndConstraints lookups txc]

-- | Each transaction output should contain a minimum amount of Ada (this is a
-- restriction on the real Cardano network).
adjustUnbalancedTx :: Params -> UnbalancedTx -> Either Tx.ToCardanoError ([Ada.Ada], UnbalancedTx)
adjustUnbalancedTx params = alaf Compose (tx . Tx.outputs . traverse) (adjustTxOut params)

-- | Adjust a single transaction output so it contains at least the minimum amount of Ada
-- and return the adjustment (if any) and the updated TxOut.
adjustTxOut :: Params -> TxOut -> Either Tx.ToCardanoError ([Ada.Ada], TxOut)
adjustTxOut params txOut =
    -- Increasing the ada amount can also increase the size in bytes, so start with a rough estimated amount of ada
    let txOutEstimate = txOut { txOutValue = txOutValue txOut <> Ada.toValue minAdaTxOut }
     in fromPlutusTxOutUnsafe params txOutEstimate <&> \txOut' ->
         let minAdaTxOut' = evaluateMinLovelaceOutput params txOut'
             missingLovelace = minAdaTxOut' - Ada.fromValue (txOutValue txOut)
         in if missingLovelace > 0
             then ([missingLovelace], txOut { txOutValue = txOutValue txOut <> Ada.toValue missingLovelace })
             else ([], txOut)

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
            unbalancedTx . tx . Tx.outputs %= (Tx.TxOut { txOutAddress=pubKeyHashAddress pkh skh
                                                        , txOutValue=missing
                                                        , txOutDatumHash=Nothing
                                                        } :)

addMintingRedeemers
    :: ( MonadState ConstraintProcessingState m
       , MonadError MkTxError m
       )
    => m ()
addMintingRedeemers = do
    reds <- use mintRedeemers
    txSoFar <- use (unbalancedTx . tx)
    iforM_ reds $ \mpsHash red -> do
        let err = throwError (MintingPolicyNotFound mpsHash)
        ptr <-
            maybe
                err
                (pure . RedeemerPtr Mint . fromIntegral)
                $ elemIndex mpsHash (Map.keys $ Tx.txMintScripts txSoFar)
        unbalancedTx . tx . Tx.redeemers . at ptr .= Just red

updateUtxoIndex
    :: ( MonadReader (ScriptLookups a) m
       , MonadState ConstraintProcessingState m
       )
    => m ()
updateUtxoIndex = do
    ScriptLookups{slTxOutputs} <- ask
    unbalancedTx . utxoIndex <>= fmap Tx.toTxOut slTxOutputs

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
    ScriptLookups{slTxOutputs, slTypedPlutusV1Validator} <- ask
    inst <- maybe (throwError TypedValidatorMissing) pure slTypedPlutusV1Validator
    typedOutRef <-
      either (throwError . TypeCheckFailed) pure
      $ runExcept @Typed.ConnectionError
      $ do
          (txOut, datum) <- maybe (throwError UnknownRef) pure $ do
                                ciTxOut <- Map.lookup icTxOutRef slTxOutputs
                                datum <- ciTxOut ^? Tx.ciTxOutScriptDatum . _2 . _Just
                                pure (Tx.toTxOut ciTxOut, datum)
          Typed.typeScriptTxOutRef inst icTxOutRef txOut datum
    -- TODO Needs to work with PlutusV1 AND PlutusV2.
    let txIn = Scripts.makeTypedScriptTxIn PlutusV1 inst icRedeemer typedOutRef
        vl   = Tx.txOutValue $ Typed.tyTxOutTxOut $ Typed.tyTxOutRefOut typedOutRef
    unbalancedTx . tx . Tx.inputs %= (Typed.tyTxInTxIn txIn :)
    valueSpentInputs <>= provided vl

-- | Add a typed output and return its value.
addOwnOutput
    :: ( MonadReader (ScriptLookups a) m
        , MonadState ConstraintProcessingState m
        , FromData (DatumType a)
        , ToData (DatumType a)
        , MonadError MkTxError m
        )
    => ScriptOutputConstraint (DatumType a)
    -> m ()
addOwnOutput ScriptOutputConstraint{ocDatum, ocValue} = do
    ScriptLookups{slTypedPlutusV1Validator} <- ask
    inst <- maybe (throwError TypedValidatorMissing) pure slTypedPlutusV1Validator
    let txOut = Typed.makeTypedScriptTxOut inst ocDatum ocValue
        dsV   = Datum (toBuiltinData ocDatum)
    unbalancedTx . tx . Tx.outputs %= (Typed.tyTxOutTxOut txOut :)
    unbalancedTx . tx . Tx.datumWitnesses . at (P.datumHash dsV) .= Just dsV
    valueSpentOutputs <>= provided ocValue

data MkTxError =
    TypeCheckFailed Typed.ConnectionError
    | TxOutRefNotFound TxOutRef
    | TxOutRefWrongType TxOutRef
    | DatumNotFound DatumHash
    | MintingPolicyNotFound MintingPolicyHash
    | ValidatorHashNotFound ValidatorHash
    | OwnPubKeyMissing
    | TypedValidatorMissing
    | DatumWrongHash DatumHash Datum
    | CannotSatisfyAny
    | NoMatchingOutputFound ValidatorHash
    | MultipleMatchingOutputsFound ValidatorHash
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty MkTxError where
    pretty = \case
        TypeCheckFailed e              -> "Type check failed:" <+> pretty e
        TxOutRefNotFound t             -> "Tx out reference not found:" <+> pretty t
        TxOutRefWrongType t            -> "Tx out reference wrong type:" <+> pretty t
        DatumNotFound h                -> "No datum with hash" <+> pretty h <+> "was found"
        MintingPolicyNotFound h        -> "No minting policy with hash" <+> pretty h <+> "was found"
        ValidatorHashNotFound h        -> "No validator with hash" <+> pretty h <+> "was found"
        OwnPubKeyMissing               -> "Own public key is missing"
        TypedValidatorMissing          -> "Script instance is missing"
        DatumWrongHash h d             -> "Wrong hash for datum" <+> pretty d <> colon <+> pretty h
        CannotSatisfyAny               -> "Cannot satisfy any of the required constraints"
        NoMatchingOutputFound h        -> "No matching output found for validator hash" <+> pretty h
        MultipleMatchingOutputsFound h -> "Multiple matching outputs found for validator hash" <+> pretty h

lookupTxOutRef
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => TxOutRef
    -> m ChainIndexTxOut
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
    -> m MintingPolicy
lookupMintingPolicy mph =
    let err = throwError (MintingPolicyNotFound mph) in
    asks slMPS >>= maybe err pure . view (at mph)

lookupValidator
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => ValidatorHash
    -> m (Validator, Language)
lookupValidator vh =
    let err = throwError (ValidatorHashNotFound vh) in
    asks slOtherScripts >>= maybe err pure . view (at vh)

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
    MustIncludeDatum dv ->
        let theHash = P.datumHash dv in
        unbalancedTx . tx . Tx.datumWitnesses . at theHash .= Just dv
    MustValidateIn timeRange ->
        unbalancedTx . validityTimeRange %= (timeRange /\)
    MustBeSignedBy pk ->
        unbalancedTx . requiredSignatories <>= Set.singleton pk
    MustSpendAtLeast vl -> valueSpentInputs <>= required vl
    MustProduceAtLeast vl -> valueSpentOutputs <>= required vl
    MustSpendPubKeyOutput txo -> do
        txout <- lookupTxOutRef txo
        case txout of
          Tx.PublicKeyChainIndexTxOut { Tx._ciTxOutValue } -> do
              -- TODO: Add the optional datum in the witness set for the pub key output
              unbalancedTx . tx . Tx.inputs %= (Tx.pubKeyTxIn txo :)
              valueSpentInputs <>= provided _ciTxOutValue
          _ -> throwError (TxOutRefWrongType txo)
    MustSpendScriptOutput txo red -> do
        txout <- lookupTxOutRef txo
        mscriptTXO <- resolveScriptTxOut txout
        case mscriptTXO of
          Just ((_, validator, pv), (dvh, datum), value) -> do
            -- TODO: When witnesses are properly segregated we can
            --       probably get rid of the 'slOtherData' map and of
            --       'lookupDatum'
            let input = Tx.scriptTxIn txo pv validator red datum
            unbalancedTx . tx . Tx.inputs %= (input :)
            inputs <- use (unbalancedTx . tx . Tx.inputs)
            -- We use fromJust because we can garanty that it will always be Just.
            let idx = fromJust $ elemIndex input inputs
            unbalancedTx . tx . Tx.datumWitnesses . at dvh .= Just datum
            unbalancedTx . tx . Tx.redeemers . at (RedeemerPtr Spend (fromIntegral idx)) .= Just red
            valueSpentInputs <>= provided value
          _ -> throwError (TxOutRefWrongType txo)
    MustUseOutputAsCollateral _ -> do
        pure () -- TODO
    MustReferenceOutput txo -> do
        unbalancedTx . tx . Tx.referenceInputs %= (Tx.pubKeyTxIn txo :)
    MustMintValue mpsHash red tn i -> do
        mintingPolicyScript <- lookupMintingPolicy mpsHash
        -- See note [Mint and Fee fields must have ada symbol].
        let value = (<>) (Ada.lovelaceValueOf 0) . Value.singleton (Value.mpsSymbol mpsHash) tn
        -- If i is negative we are burning tokens. The tokens burned must
        -- be provided as an input. So we add the value burnt to
        -- 'valueSpentInputs'. If i is positive then new tokens are created
        -- which must be added to 'valueSpentOutputs'.
        if i < 0
            then valueSpentInputs <>= provided (value (negate i))
            else valueSpentOutputs <>= provided (value i)

        unbalancedTx . tx . Tx.mintScripts %= Map.insert mpsHash mintingPolicyScript
        unbalancedTx . tx . Tx.mint <>= value i
        mintRedeemers . at mpsHash .= Just red
    MustPayToPubKeyAddress pk skhM mdv vl -> do
        -- if datum is presented, add it to 'datumWitnesses'
        forM_ mdv $ \dv -> do
            unbalancedTx . tx . Tx.datumWitnesses . at (P.datumHash dv) .= Just dv
        let hash = P.datumHash <$> mdv
        unbalancedTx . tx . Tx.outputs %= (Tx.TxOut{ txOutAddress=pubKeyHashAddress pk skhM
                                                   , txOutValue=vl
                                                   , txOutDatumHash=hash
                                                   } :)
        valueSpentOutputs <>= provided vl
    MustPayToOtherScript vlh svhM dv vl -> do
        let addr = Address.scriptValidatorHashAddress vlh svhM
            theHash = P.datumHash dv
        unbalancedTx . tx . Tx.datumWitnesses . at theHash .= Just dv
        unbalancedTx . tx . Tx.outputs %= (scriptAddressTxOut addr vl dv :)
        valueSpentOutputs <>= provided vl
    MustHashDatum dvh dv -> do
        unless (P.datumHash dv == dvh)
            (throwError $ DatumWrongHash dvh dv)
        unbalancedTx . tx . Tx.datumWitnesses . at dvh .= Just dv
    MustSatisfyAnyOf xs -> do
        s <- get
        let tryNext [] =
                throwError CannotSatisfyAny
            tryNext (hs:qs) = do
                traverse_ processConstraint hs `catchError` \_ -> put s >> tryNext qs
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
        -- with PlutusV2 validator. This means changing `ChainIndexTxOut` to
        -- include the hash.
        let matches (Just ((validatorHash, _, _), (_, datum), value)) =
                validatorHash == vh && datumPred datum && valuePred value
            matches Nothing = False
        opts <- filter (matches . snd)
            <$> traverse (\(ref, txo) -> (ref,) <$> resolveScriptTxOut txo)
                         (Map.toList slTxOutputs)
        case opts of
            [] -> throwError $ NoMatchingOutputFound vh
            [(ref, Just ((_, validator, pv), (dvh, datum), value))] -> do
                let input = Tx.scriptTxIn ref pv validator red datum
                unbalancedTx . tx . Tx.inputs %= (input :)
                unbalancedTx . tx . Tx.datumWitnesses . at dvh .= Just datum
                valueSpentInputs <>= provided value
            _ -> throwError $ MultipleMatchingOutputsFound vh

resolveScriptTxOut
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => ChainIndexTxOut -> m (Maybe ((ValidatorHash, Validator, Language), (DatumHash, Datum), Value))
resolveScriptTxOut
        Tx.ScriptChainIndexTxOut
            { Tx._ciTxOutValidator = (vh, v)
            , Tx._ciTxOutScriptDatum = (dh, d)
            , Tx._ciTxOutValue
            } = do
    -- first check in the 'ChainIndexTx' for the validator, then
    -- look for it in the 'slOtherScripts map.
    (validator, pv) <- maybe (lookupValidator vh) (pure . (, PlutusV1)) v

    -- first check in the 'ChainIndexTxOut' for the datum, then
    -- look for it in the 'slOtherData' map.
    dataValue <- maybe (lookupDatum dh) pure d

    pure $ Just ((vh, validator, pv), (dh, dataValue), _ciTxOutValue)
resolveScriptTxOut _ = pure Nothing
