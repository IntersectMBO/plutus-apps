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
    , typedValidatorLookups
    , generalise
    , unspentOutputs
    , mintingPolicy
    , otherScript
    , otherData
    , ownPaymentPubKeyHash
    , ownStakePubKeyHash
    , paymentPubKey
    -- * Constraints resolution
    , SomeLookupsAndConstraints(..)
    , UnbalancedTx(..)
    , tx
    , requiredSignatories
    , utxoIndex
    , validityTimeRange
    , emptyUnbalancedTx
    , adjustUnbalancedTx
    , MkTxError(..)
    , mkTx
    , mkSomeTx
    -- * Internals exposed for testing
    , ValueSpentBalances(..)
    , provided
    , required
    , missingValueSpent
    ) where

import Control.Lens (At (at), iforM_, makeLensesFor, mapAccumLOf, use, view, (%=), (.=), (<>=))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (catchError, throwError), runExcept, unless)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get, put), execStateT, gets)

import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.List (elemIndex)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.OpenApi.Schema qualified as OpenApi
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), colon, hang, vsep, (<+>))

import PlutusTx (FromData, ToData (toBuiltinData))
import PlutusTx.Lattice (BoundedMeetSemiLattice (top), JoinSemiLattice ((\/)), MeetSemiLattice ((/\)))
import PlutusTx.Numeric qualified as N

import Data.Semigroup (First (First, getFirst))
import Data.Set (Set)
import Ledger.Address (PaymentPubKey (PaymentPubKey), PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash,
                       pubKeyHashAddress)
import Ledger.Address qualified as Address
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icRedeemer, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocValue),
                                         TxConstraint (MustBeSignedBy, MustHashDatum, MustIncludeDatum, MustMintValue, MustPayToOtherScript, MustPayToPubKeyAddress, MustProduceAtLeast, MustSatisfyAnyOf, MustSpendAtLeast, MustSpendPubKeyOutput, MustSpendScriptOutput, MustValidateIn),
                                         TxConstraintFun (MustSpendScriptOutputWithMatchingDatumAndValue),
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs))
import Ledger.Crypto (pubKeyHash)
import Ledger.Orphans ()
import Ledger.Params (Params)
import Ledger.Tx (ChainIndexTxOut, RedeemerPtr (RedeemerPtr), ScriptTag (Mint), Tx,
                  TxOut (txOutAddress, txOutDatumHash, txOutValue), TxOutRef)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts (Any, TypedValidator, ValidatorTypes (DatumType, RedeemerType))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Typed.Tx (ConnectionError)
import Ledger.Typed.Tx qualified as Typed
import Ledger.Validation (evaluateMinLovelaceOutput, fromPlutusTxOutUnsafe)
import Plutus.Script.Utils.V1.Scripts (datumHash, mintingPolicyHash, validatorHash)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, Validator,
                                 ValidatorHash)
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Value (Value)
import Plutus.V1.Ledger.Value qualified as Value

data ScriptLookups a =
    ScriptLookups
        { slMPS                  :: Map MintingPolicyHash MintingPolicy
        -- ^ Minting policies that the script interacts with
        , slTxOutputs            :: Map TxOutRef ChainIndexTxOut
        -- ^ Unspent outputs that the script may want to spend
        , slOtherScripts         :: Map ValidatorHash Validator
        -- ^ Validators of scripts other than "our script"
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
    let validator = fmap Scripts.generalise (slTypedValidator sl)
    in sl{slTypedValidator = validator}

instance Semigroup (ScriptLookups a) where
    l <> r =
        ScriptLookups
            { slMPS = slMPS l <> slMPS r
            , slTxOutputs = slTxOutputs l <> slTxOutputs r
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
    mempty  = ScriptLookups mempty mempty mempty mempty mempty Nothing Nothing Nothing

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
    mempty
        { slMPS = Map.singleton (Scripts.forwardingMintingPolicyHash inst) (Scripts.forwardingMintingPolicy inst)
        , slTypedValidator = Just inst
        }

-- | A script lookups value that uses the map of unspent outputs to resolve
--   input constraints.
unspentOutputs :: Map TxOutRef ChainIndexTxOut -> ScriptLookups a
unspentOutputs mp = mempty { slTxOutputs = mp }

-- | A script lookups value with a minting policy script.
mintingPolicy :: MintingPolicy -> ScriptLookups a
mintingPolicy pl =
    let hsh = mintingPolicyHash pl in
    mempty { slMPS = Map.singleton hsh pl }

-- | A script lookups value with a validator script.
otherScript :: Validator -> ScriptLookups a
otherScript vl =
    let vh = validatorHash vl in
    mempty { slOtherScripts = Map.singleton vh vl }

-- | A script lookups value with a datum.
otherData :: Datum -> ScriptLookups a
otherData dt =
    let dh = datumHash dt in
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
        { unBalancedTxTx                  :: Tx
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
    [ ("unBalancedTxTx", "tx")
    , ("unBalancedTxRequiredSignatories", "requiredSignatories")
    , ("unBalancedTxUtxoIndex", "utxoIndex")
    , ("unBalancedTxValidityTimeRange", "validityTimeRange")
    ] ''UnbalancedTx

emptyUnbalancedTx :: UnbalancedTx
emptyUnbalancedTx = UnbalancedTx mempty mempty mempty top

instance Pretty UnbalancedTx where
    pretty (UnbalancedTx utx rs utxo vr) =
        vsep
        [ hang 2 $ vsep ["Tx:", pretty utx]
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
    ] ''ConstraintProcessingState

initialState :: ConstraintProcessingState
initialState = ConstraintProcessingState
    { cpsUnbalancedTx = emptyUnbalancedTx
    , cpsMintRedeemers = mempty
    , cpsValueSpentBalancesInputs = ValueSpentBalances mempty mempty
    , cpsValueSpentBalancesOutputs = ValueSpentBalances mempty mempty
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
adjustUnbalancedTx params utx =
    let (acc, res) = mapAccumLOf (tx . Tx.outputs . traverse) step (Right []) utx
    in (flip (,) res) <$> acc
  where
    step acc txOut = case (acc, adjustTxOut txOut) of
        (Left _, _)                       -> (acc, txOut) -- acc is an error, do nothing
        (Right _, Left e)                 -> (Left e, txOut) -- failed somewhere, return error as acc
        (Right acc', Right (ada, txOut')) -> (Right $ ada:acc', txOut')
    adjustTxOut :: TxOut -> Either Tx.ToCardanoError (Ada.Ada, TxOut)
    adjustTxOut txOut = fromPlutusTxOutUnsafe params txOut <&> \txOut' ->
        let minAdaTxOut' = evaluateMinLovelaceOutput params txOut'
            missingLovelace = max 0 (minAdaTxOut' - Ada.fromValue (txOutValue txOut))
        in (missingLovelace, txOut { txOutValue = txOutValue txOut <> Ada.toValue missingLovelace })

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
    let mpss = mintingPolicyHash <$> Set.toList (Tx.txMintScripts txSoFar)
    iforM_ reds $ \mpsHash red -> do
        let err = throwError (MintingPolicyNotFound mpsHash)
        ptr <- maybe err (pure . RedeemerPtr Mint . fromIntegral) $ elemIndex mpsHash mpss
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
    ScriptLookups{slTxOutputs, slTypedValidator} <- ask
    inst <- maybe (throwError TypedValidatorMissing) pure slTypedValidator
    typedOutRef <-
        either (throwError . TypeCheckFailed) pure
        $ runExcept @ConnectionError
        $ Typed.typeScriptTxOutRef (`Map.lookup` slTxOutputs) inst icTxOutRef
    let txIn = Typed.makeTypedScriptTxIn inst icRedeemer typedOutRef
        vl   = Tx.txOutValue $ Typed.tyTxOutTxOut $ Typed.tyTxOutRefOut typedOutRef
    unbalancedTx . tx . Tx.inputs %= Set.insert (Typed.tyTxInTxIn txIn)
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
    ScriptLookups{slTypedValidator} <- ask
    inst <- maybe (throwError TypedValidatorMissing) pure slTypedValidator
    let txOut = Typed.makeTypedScriptTxOut inst ocDatum ocValue
        dsV   = Datum (toBuiltinData ocDatum)
    unbalancedTx . tx . Tx.outputs %= (Typed.tyTxOutTxOut txOut :)
    unbalancedTx . tx . Tx.datumWitnesses . at (datumHash dsV) .= Just dsV
    valueSpentOutputs <>= provided ocValue

data MkTxError =
    TypeCheckFailed ConnectionError
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
    -> m Validator
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
        let theHash = datumHash dv in
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
              unbalancedTx . tx . Tx.inputs %= Set.insert (Tx.pubKeyTxIn txo)
              valueSpentInputs <>= provided _ciTxOutValue
          _ -> throwError (TxOutRefWrongType txo)
    MustSpendScriptOutput txo red -> do
        txout <- lookupTxOutRef txo
        mscriptTXO <- resolveScriptTxOut txout
        case mscriptTXO of
          Just (validator, datum, value) -> do
            let dvh = datumHash datum
            -- TODO: When witnesses are properly segregated we can
            --       probably get rid of the 'slOtherData' map and of
            --       'lookupDatum'
            let input = Tx.scriptTxIn txo validator red datum
            unbalancedTx . tx . Tx.inputs %= Set.insert input
            unbalancedTx . tx . Tx.datumWitnesses . at dvh .= Just datum
            valueSpentInputs <>= provided value
          _ -> throwError (TxOutRefWrongType txo)

    MustMintValue mpsHash red tn i -> do
        mintingPolicyScript <- lookupMintingPolicy mpsHash
        let value = Value.singleton (Value.mpsSymbol mpsHash) tn
        -- If i is negative we are burning tokens. The tokens burned must
        -- be provided as an input. So we add the value burnt to
        -- 'valueSpentInputs'. If i is positive then new tokens are created
        -- which must be added to 'valueSpentOutputs'.
        if i < 0
            then valueSpentInputs <>= provided (value (negate i))
            else valueSpentOutputs <>= provided (value i)

        unbalancedTx . tx . Tx.mintScripts %= Set.insert mintingPolicyScript
        unbalancedTx . tx . Tx.mint <>= value i
        mintRedeemers . at mpsHash .= Just red
    MustPayToPubKeyAddress pk skhM mdv vl -> do
        -- if datum is presented, add it to 'datumWitnesses'
        forM_ mdv $ \dv -> do
            unbalancedTx . tx . Tx.datumWitnesses . at (datumHash dv) .= Just dv
        let hash = datumHash <$> mdv
        unbalancedTx . tx . Tx.outputs %= (Tx.TxOut{ txOutAddress=pubKeyHashAddress pk skhM
                                                   , txOutValue=vl
                                                   , txOutDatumHash=hash
                                                   } :)
        valueSpentOutputs <>= provided vl
    MustPayToOtherScript vlh svhM dv vl -> do
        let addr = Address.scriptValidatorHashAddress vlh svhM
            theHash = datumHash dv
        unbalancedTx . tx . Tx.datumWitnesses . at theHash .= Just dv
        unbalancedTx . tx . Tx.outputs %= (Tx.scriptTxOut' vl addr dv :)
        valueSpentOutputs <>= provided vl
    MustHashDatum dvh dv -> do
        unless (datumHash dv == dvh)
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
        let matches (Just (validator, datum, value)) = validatorHash validator == vh && datumPred datum && valuePred value
            matches Nothing = False
        opts <- filter (matches . snd) <$> traverse (\(ref, txo) -> (ref,) <$> resolveScriptTxOut txo) (Map.toList slTxOutputs)
        case opts of
            [] -> throwError $ NoMatchingOutputFound vh
            [(ref, Just (validator, datum, value))] -> do
                let dvh = datumHash datum
                let input = Tx.scriptTxIn ref validator red datum
                unbalancedTx . tx . Tx.inputs %= Set.insert input
                unbalancedTx . tx . Tx.datumWitnesses . at dvh .= Just datum
                valueSpentInputs <>= provided value
            _ -> throwError $ MultipleMatchingOutputsFound vh

resolveScriptTxOut
    :: ( MonadReader (ScriptLookups a) m
       , MonadError MkTxError m
       )
    => ChainIndexTxOut -> m (Maybe (Validator, Datum, Value))
resolveScriptTxOut Tx.ScriptChainIndexTxOut { Tx._ciTxOutValidator, Tx._ciTxOutDatum, Tx._ciTxOutValue } = do
    -- first check in the 'ChainIndexTx' for the validator, then
    -- look for it in the 'slOtherScripts map.
    validator <- either lookupValidator pure _ciTxOutValidator

    -- first check in the 'ChainIndexTx' for the datum, then
    -- look for it in the 'slOtherData' map.
    dataValue <- either lookupDatum pure _ciTxOutDatum

    pure $ Just (validator, dataValue, _ciTxOutValue)
resolveScriptTxOut _ = pure Nothing
