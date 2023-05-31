{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

-- | Constraints for transactions
module Ledger.Tx.Constraints.TxConstraints where

import Cardano.Node.Emulator.Internal.Node.TimeSlot (slotRangeToPOSIXTimeRange)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (bimap))
import Data.Default (def)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty, prettyList), defaultLayoutOptions, hang, layoutPretty, viaShow, vsep, (<+>))

import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude (Bool (False, True), Eq, Foldable (foldMap), Functor (fmap), Integer, JoinSemiLattice ((\/)),
                         Maybe (Just, Nothing), Monoid (mempty), Semigroup ((<>)), any, concat, foldl, map, mapMaybe,
                         not, null, ($), (.), (==), (>>=), (||))

import Ledger.Address (Address (Address), PaymentPubKeyHash (PaymentPubKeyHash))
import Ledger.Slot (Slot)
import Ledger.Tx (DecoratedTxOut)
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum, DatumHash, MintingPolicyHash,
                             POSIXTime, POSIXTimeRange, Redeemer, StakingCredential, TxOutRef, Validator, ValidatorHash)
import Plutus.V1.Ledger.Interval qualified as I
import Plutus.V1.Ledger.Scripts (MintingPolicyHash (MintingPolicyHash), ScriptHash (ScriptHash),
                                 ValidatorHash (ValidatorHash), unitRedeemer)
import Plutus.V1.Ledger.Value (TokenName, Value, isZero)
import Plutus.V1.Ledger.Value qualified as Value

import Control.Lens (At (at), (^.))
import Data.Function (const, flip)
import Data.Maybe (fromMaybe)
import Prelude qualified as Haskell
import Prettyprinter.Render.String (renderShowS)

import Ledger.Tx.Constraints.ValidityInterval (ValidityInterval, fromPlutusInterval, toPlutusInterval)

-- | How tx outs datum are embedded in a a Tx
--
-- We do not use 'TxOutDatum' from cardano-node to provide easier to handel
-- type (we don't type witnesses) and to have a distinction at the type leve
-- between constraints that require a Datum and constraints (like
-- 'MustPayToOtherScript') with an optional datum (like
-- 'MustPayToPubKeyAddress').
data TxOutDatum datum =
    TxOutDatumHash datum
    -- ^ A datum specified in a transaction output using only it's hash, i.e.
    -- the datum is not inlined nor is it added in the transaction body.
  | TxOutDatumInTx datum
    -- ^ A datum specified in a transaction output using it's hash, while also
    -- adding the actual datum in the transaction body.
  | TxOutDatumInline datum
    -- ^ A datum inlined in a transaction output. It is *not* added in the
    -- transaction body.
    deriving stock (Haskell.Show, Generic, Haskell.Eq, Haskell.Functor)
    deriving anyclass (ToJSON, FromJSON)

instance Eq d => Eq (TxOutDatum d) where
    TxOutDatumHash d1 == TxOutDatumHash d2     = d1 == d2
    TxOutDatumInTx d1 == TxOutDatumInTx d2     = d1 == d2
    TxOutDatumInline d1 == TxOutDatumInline d2 = d1 == d2
    _ == _                                     = False

instance Functor TxOutDatum where
    fmap f (TxOutDatumHash d)   = TxOutDatumHash $ f d
    fmap f (TxOutDatumInTx d)   = TxOutDatumInTx $ f d
    fmap f (TxOutDatumInline d) = TxOutDatumInline $ f d

getTxOutDatum :: TxOutDatum d -> d
getTxOutDatum (TxOutDatumHash d)   = d
getTxOutDatum (TxOutDatumInTx d)   = d
getTxOutDatum (TxOutDatumInline d) = d

isTxOutDatumHash :: TxOutDatum d -> Bool
isTxOutDatumHash (TxOutDatumHash _) = True
isTxOutDatumHash _                  = False

isTxOutDatumInTx :: TxOutDatum d -> Bool
isTxOutDatumInTx (TxOutDatumInTx _) = True
isTxOutDatumInTx _                  = False

isTxOutDatumInline :: TxOutDatum d -> Bool
isTxOutDatumInline (TxOutDatumInline _) = True
isTxOutDatumInline _                    = False

instance Pretty d => Pretty (TxOutDatum d) where
  pretty = \case
    TxOutDatumHash d   -> "hashed datum" <+> pretty d
    TxOutDatumInTx d   -> "datum in tx body" <+> pretty d
    TxOutDatumInline d -> "inline datum" <+> pretty d

-- | Constraints on transactions that want to spend script outputs
data TxConstraint =
      MustIncludeDatumInTxWithHash DatumHash Datum
    -- ^ The provided 'DatumHash' and 'Datum' must be included in the
    -- transaction body. Like 'MustIncludeDatumInTx', but useful when you
    -- already have a 'DatumHash' and want to make sure that is is the actual
    -- hash of the 'Datum'.
    | MustIncludeDatumInTx Datum
    -- ^ Like 'MustHashDatum', but the hash of the 'Datum' is computed automatically.
    | MustValidateInTimeRange !(ValidityInterval POSIXTime)
    -- ^ The transaction's validity range must be set with the given 'POSIXTimeRange'.
    | MustBeSignedBy PaymentPubKeyHash
    -- ^ The transaction must add the given 'PaymentPubKeyHash' in its signatories.
    | MustSpendAtLeast Value
    -- ^ The sum of the transaction's input 'Value's must be at least as much as
    -- the given 'Value'.
    | MustProduceAtLeast Value
    -- ^ The sum of the transaction's output 'Value's must be at least as much as
    -- the given 'Value'.
    | MustSpendPubKeyOutput TxOutRef
    -- ^ The transaction must spend the given unspent transaction public key output.
    | MustSpendScriptOutput TxOutRef Redeemer (Maybe TxOutRef)
    -- ^ The transaction must spend the given unspent transaction script output.
    | MustUseOutputAsCollateral TxOutRef
    -- ^ The transaction must include the utxo as collateral input.
    | MustReferenceOutput TxOutRef
    -- ^ The transaction must reference (not spend) the given unspent transaction output.
    | MustMintValue MintingPolicyHash Redeemer TokenName Integer (Maybe TxOutRef)
    -- ^ The transaction must mint the given token and amount.
    | MustPayToAddress Address (Maybe (TxOutDatum Datum)) (Maybe ScriptHash) Value
    -- ^ The transaction must create a transaction output.
    | MustSatisfyAnyOf [[TxConstraint]]
    -- ^ The transaction must satisfy constraints given as an alternative of conjuctions (DNF),
    -- that is `check (MustSatisfyAnyOf xs) = any (all check) xs`
    deriving stock (Haskell.Show, Generic, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty TxConstraint where
    pretty = \case
        MustIncludeDatumInTxWithHash dvh dv ->
            hang 2 $ vsep ["must include datum in tx with hash:", pretty dvh, pretty dv]
        MustIncludeDatumInTx dv ->
            hang 2 $ vsep ["must include datum in tx:", pretty dv]
        MustValidateInTimeRange range ->
            "must validate in:" <+> viaShow range
        MustBeSignedBy signatory ->
            "must be signed by:" <+> pretty signatory
        MustSpendAtLeast vl ->
            hang 2 $ vsep ["must spend at least:", pretty vl]
        MustProduceAtLeast vl ->
            hang 2 $ vsep ["must produce at least:", pretty vl]
        MustSpendPubKeyOutput ref ->
            hang 2 $ vsep ["must spend pubkey output:", pretty ref]
        MustSpendScriptOutput ref red mref ->
            hang 2 $ vsep ["must spend script output:", pretty ref, pretty red, pretty mref]
        MustReferenceOutput ref ->
            hang 2 $ vsep ["must reference output:", pretty ref]
        MustMintValue mps red tn i mref ->
            hang 2 $ vsep ["must mint value:", pretty mps, pretty red, pretty tn <+> pretty i, pretty mref]
        MustPayToAddress addr datum refScript v ->
            hang 2 $ vsep ["must pay to pubkey address:", pretty addr, pretty datum, pretty refScript, pretty v]
        MustUseOutputAsCollateral ref ->
            hang 2 $ vsep ["must use output as collateral:", pretty ref]
        MustSatisfyAnyOf xs ->
            hang 2 $ vsep ["must satisfy any of:", prettyList xs]


-- | Constraints on transactions that contain functions. These don't support conversion to and from JSON.
data TxConstraintFun =
    MustSpendScriptOutputWithMatchingDatumAndValue ValidatorHash (Datum -> Bool) (Value -> Bool) Redeemer
    -- ^ The transaction must spend a script output from the given script address which matches the @Datum@ and @Value@ predicates.

instance Haskell.Show TxConstraintFun where
    showsPrec _ = renderShowS . layoutPretty defaultLayoutOptions . pretty

instance Pretty TxConstraintFun where
    pretty = \case
        MustSpendScriptOutputWithMatchingDatumAndValue sh _ _ red ->
            hang 2 $ vsep ["must spend script out from script hash: ", pretty sh, pretty red]

newtype TxConstraintFuns = TxConstraintFuns [TxConstraintFun]
    deriving stock (Haskell.Show, Generic)
    deriving newtype (Semigroup, Monoid)

-- We can't convert functons to JSON, so we have a @TxConstraintFuns@ wrapper to provide dummy To/FromJSON instances.
instance ToJSON TxConstraintFuns where
    toJSON _ = Aeson.Array Haskell.mempty

instance FromJSON TxConstraintFuns where
    parseJSON _ = Haskell.pure mempty

-- | Constraint which specifies that the transaction must spend a transaction
-- output from a target script.
data ScriptInputConstraint a =
    ScriptInputConstraint
        { icRedeemer          :: a -- ^ The typed 'Redeemer' to be used with the target script
        , icTxOutRef          :: TxOutRef -- ^ The UTXO to be spent by the target script
        , icReferenceTxOutRef :: Maybe TxOutRef -- ^ Optionally use a reference script as witness
        } deriving stock (Haskell.Show, Generic, Haskell.Functor)

{-# INLINABLE mustSpendOutputFromTheScript #-}
-- | @mustSpendOutputFromTheScript txOutRef red@ spends the transaction output
-- @txOutRef@ with a script address using the redeemer @red@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint spends a script
-- output @txOutRef@ with redeemer @red@.
-- The script address is derived from the typed validator that is provided in
-- the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.typedValidatorLookups'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- spend script transaction output with @red@ is part of the transaction's
-- inputs.
mustSpendOutputFromTheScript :: TxOutRef -> i -> TxConstraints i o
mustSpendOutputFromTheScript txOutRef red =
    mempty { txOwnInputs = [ScriptInputConstraint red txOutRef Nothing] }

{-# INLINABLE mustSpendOutputFromTheReferencedScript #-}
-- | @mustSpendOutputFromTheReferencedScript txOutRef red ref @ spends the transaction
-- output @txOutRef@ with a script address using the redeemer @red@, using the reference script @ref@
-- as a validator.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint spends a script
-- output @txOutRef@ with redeemer @red@.
-- The script address is derived from the typed validator that is provided in
-- the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.typedValidatorLookups'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- spend script transaction output with @red@ is part of the transaction's
-- inputs.
mustSpendOutputFromTheReferencedScript :: TxOutRef -> i -> TxOutRef -> TxConstraints i o
mustSpendOutputFromTheReferencedScript txOutRef red ref =
    mempty { txOwnInputs = [ScriptInputConstraint red txOutRef (Just ref)] }

instance (Pretty a) => Pretty (ScriptInputConstraint a) where
    pretty ScriptInputConstraint{icRedeemer, icTxOutRef, icReferenceTxOutRef} =
        vsep $
            [ "Redeemer:" <+> pretty icRedeemer
            , "TxOutRef:" <+> pretty icTxOutRef
            ] Haskell.++ Haskell.maybe [] (\ref -> ["Reference TxOutRef: " <+> pretty ref]) icReferenceTxOutRef

deriving anyclass instance (ToJSON a) => ToJSON (ScriptInputConstraint a)
deriving anyclass instance (FromJSON a) => FromJSON (ScriptInputConstraint a)
deriving stock instance (Haskell.Eq a) => Haskell.Eq (ScriptInputConstraint a)

-- Constraint which specifies that the transaction must produce a transaction
-- output which pays to a target script.
data ScriptOutputConstraint a =
    ScriptOutputConstraint
        { ocDatum               :: TxOutDatum a -- ^ Typed datum to be used with the target script
        , ocValue               :: Value
        , ocReferenceScriptHash :: Maybe ScriptHash
        } deriving stock (Haskell.Show, Generic, Haskell.Functor)

instance (Pretty a) => Pretty (ScriptOutputConstraint a) where
    pretty ScriptOutputConstraint{ocDatum, ocValue, ocReferenceScriptHash} =
        vsep $
            [ "Datum:" <+> pretty ocDatum
            , "Value:" <+> pretty ocValue
            ] Haskell.++ Haskell.maybe [] (\sh -> ["Reference script hash: " <+> pretty sh]) ocReferenceScriptHash

deriving anyclass instance (ToJSON a) => ToJSON (ScriptOutputConstraint a)
deriving anyclass instance (FromJSON a) => FromJSON (ScriptOutputConstraint a)
deriving stock instance (Haskell.Eq a) => Haskell.Eq (ScriptOutputConstraint a)

-- | Restrictions placed on the allocation of funds to outputs of transactions.
data TxConstraints i o =
    TxConstraints
        { txConstraints    :: [TxConstraint]
        , txConstraintFuns :: TxConstraintFuns
        , txOwnInputs      :: [ScriptInputConstraint i]
        , txOwnOutputs     :: [ScriptOutputConstraint o]
        }
    deriving stock (Haskell.Show, Generic)

instance Bifunctor TxConstraints where
    bimap f g txc =
        txc
            { txOwnInputs = Haskell.fmap (Haskell.fmap f) (txOwnInputs txc)
            , txOwnOutputs = Haskell.fmap (Haskell.fmap g) (txOwnOutputs txc)
            }

type UntypedConstraints = TxConstraints PlutusTx.BuiltinData PlutusTx.BuiltinData

instance Semigroup (TxConstraints i o) where
    l <> r =
        TxConstraints
            { txConstraints = txConstraints l <> txConstraints r
            , txConstraintFuns = txConstraintFuns l <> txConstraintFuns r
            , txOwnInputs = txOwnInputs l <> txOwnInputs r
            , txOwnOutputs = txOwnOutputs l <> txOwnOutputs r
            }

instance Haskell.Semigroup (TxConstraints i o) where
    (<>) = (<>) -- uses PlutusTx.Semigroup instance

instance Monoid (TxConstraints i o) where
    mempty = TxConstraints mempty mempty mempty mempty

instance Haskell.Monoid (TxConstraints i o) where
    mappend = (<>)
    mempty  = mempty

deriving anyclass instance (ToJSON i, ToJSON o) => ToJSON (TxConstraints i o)
deriving anyclass instance (FromJSON i, FromJSON o) => FromJSON (TxConstraints i o)
-- deriving stock instance (Haskell.Eq i, Haskell.Eq o) => Haskell.Eq (TxConstraints i o)

{-# INLINABLE singleton #-}
singleton :: TxConstraint -> TxConstraints i o
singleton a = mempty { txConstraints = [a] }

{-# INLINABLE mustValidateIn #-}
{-# DEPRECATED mustValidateIn "Please use mustValidateInTimeRange or mustValidateInSlotRange instead" #-}
-- | @mustValidateIn r@ requires the transaction's validity time range to be contained
--   in POSIXTimeRange @r@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint sets the
-- transaction's validity time range to @r@.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- time range @r@ is entirely contained in the transaction's validity time range.
mustValidateIn :: forall i o. POSIXTimeRange -> TxConstraints i o
mustValidateIn = singleton . MustValidateInTimeRange . fromPlutusInterval

{-# INLINABLE mustValidateInTimeRange #-}
-- | @mustValidateInTimeRange r@ requires the transaction's validity time range to be contained
--   in POSIXTime range @r@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint sets the
-- transaction's validity time range to @r@.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- time range @r@ is entirely contained in the transaction's validity time range.
mustValidateInTimeRange :: forall i o. ValidityInterval POSIXTime -> TxConstraints i o
mustValidateInTimeRange = singleton . MustValidateInTimeRange

{-# INLINABLE mustValidateInSlotRange #-}
-- | @mustValidateInSlotRange r@ requires the transaction's validity slot range to be contained
--   in Slot range @r@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint sets the
-- transaction's validity slot range to @r@.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- slot range @r@ is entirely contained in the transaction's validity time range.
mustValidateInSlotRange :: forall i o. ValidityInterval Slot -> TxConstraints i o
mustValidateInSlotRange = singleton . MustValidateInTimeRange . fromPlutusInterval . slotRangeToPOSIXTimeRange def . toPlutusInterval

{-# INLINABLE mustBeSignedBy #-}
-- | @mustBeSignedBy pk@ requires the transaction to be signed by the public
-- key @pk@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @pk@ in the
-- transaction's public key witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @pk@
-- is part of the transaction's public key witness set.
mustBeSignedBy :: forall i o. PaymentPubKeyHash -> TxConstraints i o
mustBeSignedBy = singleton . MustBeSignedBy

{-# INLINABLE mustIncludeDatumInTxWithHash #-}
-- | @mustIncludeDatumInTxWithHash dh d@ requires the transaction body to
-- include the datum hash @dh@ and actual datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @dh@ and @d@
-- in the transaction's body.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @dh@
-- and @d@ are part of the transaction's body.
mustIncludeDatumInTxWithHash :: DatumHash -> Datum -> TxConstraints i o
mustIncludeDatumInTxWithHash dvh = singleton . MustIncludeDatumInTxWithHash dvh

{-# INLINABLE mustIncludeDatumInTx #-}
-- | @mustIncludeDatumInTx d@ requires the transaction body to include the
-- datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @d@ in the
-- transaction's body alongside it's hash (which is computed automatically).
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@
-- is part of the transaction's body.
mustIncludeDatumInTx :: forall i o. Datum -> TxConstraints i o
mustIncludeDatumInTx = singleton . MustIncludeDatumInTx

{-# DEPRECATED mustPayToTheScript "Use mustPayToTheScriptWithDatumHash instead" #-}
mustPayToTheScript :: o -> Value -> TxConstraints i o
mustPayToTheScript = mustPayToTheScriptWithDatumHash

{-# INLINABLE mustPayToTheScriptWithDatumHash #-}
-- | @mustPayToTheScriptWithDatumHash d v@ locks the value @v@ with a script alongside a
-- datum @d@ which is included in the transaction body.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @dt@ and @vl@ and adds @dt@ in the transaction's datum witness set.
-- The script address is derived from the typed validator that is provided in
-- the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.typedValidatorLookups'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the new script transaction output with
-- @dt@ and @vt@ is part of the transaction's outputs.
mustPayToTheScriptWithDatumHash :: o -> Value -> TxConstraints i o
mustPayToTheScriptWithDatumHash dt vl =
    mempty { txOwnOutputs = [ScriptOutputConstraint (TxOutDatumHash dt) vl Nothing] }

{-# INLINABLE mustPayToTheScriptWithDatumInTx #-}
mustPayToTheScriptWithDatumInTx :: o -> Value -> TxConstraints i o
mustPayToTheScriptWithDatumInTx dt vl =
    mempty { txOwnOutputs = [ScriptOutputConstraint (TxOutDatumInTx dt) vl Nothing] }

{-# INLINABLE mustPayToTheScriptWithInlineDatum #-}
mustPayToTheScriptWithInlineDatum :: o -> Value -> TxConstraints i o
mustPayToTheScriptWithInlineDatum dt vl =
    mempty { txOwnOutputs = [ScriptOutputConstraint (TxOutDatumInline dt) vl Nothing] }

{-# INLINABLE mustPayToTheScriptWithReferenceScript #-}
mustPayToTheScriptWithReferenceScript :: ScriptHash -> TxOutDatum o -> Value -> TxConstraints i o
mustPayToTheScriptWithReferenceScript sh dt vl =
    mempty { txOwnOutputs = [ScriptOutputConstraint dt vl (Just sh)] }

{-# INLINABLE mustPayToPubKey #-}
-- | @mustPayToPubKey pkh v@ is the same as
-- 'mustPayToPubKeyAddressWithDatumHash', but without any staking key hash and datum.
mustPayToPubKey :: forall i o. PaymentPubKeyHash -> Value -> TxConstraints i o
mustPayToPubKey (PaymentPubKeyHash pkh) vl = singleton (MustPayToAddress (Address (PubKeyCredential pkh) Nothing) Nothing Nothing vl)

{-# INLINABLE mustPayToPubKeyAddress #-}
-- | @mustPayToPubKeyAddress pkh skh v@ is the same as
-- 'mustPayToPubKeyAddressWithDatumHash', but without any datum.
mustPayToPubKeyAddress
    :: forall i o
     . PaymentPubKeyHash
    -> StakingCredential
    -> Value
    -> TxConstraints i o
mustPayToPubKeyAddress (PaymentPubKeyHash pkh) sc vl =
     singleton (MustPayToAddress (Address (PubKeyCredential pkh) (Just sc)) Nothing Nothing vl)

{-# DEPRECATED mustPayWithDatumToPubKey "Use mustPayToPubKeyWithDatumHash instead" #-}
mustPayWithDatumToPubKey
    :: forall i o
     . PaymentPubKeyHash
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayWithDatumToPubKey = mustPayToPubKeyWithDatumHash

{-# INLINABLE mustPayToPubKeyWithDatumHash #-}
-- | @mustPayToPubKeyWithDatumHash pkh d v@ is the same as
-- 'mustPayToPubKeyAddressWithDatumHash', but without the staking key hash.
mustPayToPubKeyWithDatumHash
    :: forall i o
     . PaymentPubKeyHash
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToPubKeyWithDatumHash (PaymentPubKeyHash pkh) datum vl =
    singleton (MustPayToAddress (Address (PubKeyCredential pkh) Nothing) (Just $ TxOutDatumHash datum) Nothing vl)

{-# DEPRECATED mustPayWithDatumInTxToPubKey "Use mustPayToPubKeyWithDatumInTx instead" #-}
mustPayWithDatumInTxToPubKey
    :: forall i o
     . PaymentPubKeyHash
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayWithDatumInTxToPubKey = mustPayToPubKeyWithDatumInTx

{-# INLINABLE mustPayToPubKeyWithDatumInTx #-}
-- | @mustPayToPubKeyWithDatumInTx pkh d v@ is the same as
-- 'mustPayToPubKeyAddressWithDatumHash', but with an inline datum and without the staking key hash.
mustPayToPubKeyWithDatumInTx
    :: forall i o
     . PaymentPubKeyHash
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToPubKeyWithDatumInTx (PaymentPubKeyHash pkh) datum vl =
    singleton (MustPayToAddress (Address (PubKeyCredential pkh) Nothing) (Just $ TxOutDatumInTx datum) Nothing vl)

{-# DEPRECATED mustPayWithInlineDatumToPubKey "Use mustPayToPubKeyWithInlineDatum instead" #-}
mustPayWithInlineDatumToPubKey
    :: forall i o
     . PaymentPubKeyHash
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayWithInlineDatumToPubKey = mustPayToPubKeyWithInlineDatum

{-# INLINABLE mustPayToPubKeyWithInlineDatum #-}
-- | @mustPayToPubKeyWithInlineDatum pkh d v@ is the same as
-- 'mustPayToPubKeyAddressWithDatumHash', but with an inline datum and without the staking key hash.
mustPayToPubKeyWithInlineDatum
    :: forall i o
     . PaymentPubKeyHash
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToPubKeyWithInlineDatum (PaymentPubKeyHash pkh) datum vl =
    singleton (MustPayToAddress (Address (PubKeyCredential pkh) Nothing) (Just $ TxOutDatumInline datum) Nothing vl)

{-# DEPRECATED mustPayWithDatumToPubKeyAddress "Use mustPayToPubKeyAddressWithDatumHash instead" #-}
mustPayWithDatumToPubKeyAddress
    :: forall i o
     . PaymentPubKeyHash
    -> StakingCredential
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayWithDatumToPubKeyAddress = mustPayToPubKeyAddressWithDatumHash

{-# INLINABLE mustPayToPubKeyAddressWithDatumHash #-}
-- | @mustPayToPubKeyAddressWithDatumHash pkh skh d v@ locks a transaction output
-- with a public key address.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a public key
-- output with @pkh@, @skh@, @d@ and @v@ and maybe adds @d@ in the transaction's
-- datum witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the public key transaction output with
-- @pkh@, @skh@, @d@ and @v@ is part of the transaction's outputs.
mustPayToPubKeyAddressWithDatumHash
    :: forall i o
     . PaymentPubKeyHash
    -> StakingCredential
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToPubKeyAddressWithDatumHash (PaymentPubKeyHash pkh) sc datum vl =
    singleton (MustPayToAddress (Address (PubKeyCredential pkh) (Just sc)) (Just $ TxOutDatumHash datum) Nothing vl)

{-# DEPRECATED mustPayWithDatumInTxToPubKeyAddress "Use mustPayToPubKeyAddressWithDatumInTx instead" #-}
mustPayWithDatumInTxToPubKeyAddress
    :: forall i o
     . PaymentPubKeyHash
    -> StakingCredential
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayWithDatumInTxToPubKeyAddress = mustPayToPubKeyAddressWithDatumInTx

{-# INLINABLE mustPayToPubKeyAddressWithDatumInTx #-}
-- | @mustPayToPubKeyAddressWithDatumInTx pkh d v@ is the same as
-- 'mustPayToPubKeyAddressWithDatumHash', but the datum is also added in the
-- transaction body.
mustPayToPubKeyAddressWithDatumInTx
    :: forall i o
     . PaymentPubKeyHash
    -> StakingCredential
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToPubKeyAddressWithDatumInTx (PaymentPubKeyHash pkh) sc datum vl =
    singleton (MustPayToAddress (Address (PubKeyCredential pkh) (Just sc)) (Just $ TxOutDatumInTx datum) Nothing vl)

{-# DEPRECATED mustPayWithInlineDatumToPubKeyAddress "Use mustPayToPubKeyAddressWithInlineDatum instead" #-}
mustPayWithInlineDatumToPubKeyAddress
    :: forall i o
     . PaymentPubKeyHash
    -> StakingCredential
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayWithInlineDatumToPubKeyAddress = mustPayToPubKeyAddressWithInlineDatum

{-# INLINABLE mustPayToPubKeyAddressWithInlineDatum #-}
-- | @mustPayWithInlineInlineDatumToPubKeyAddress pkh d v@ is the same as
-- 'mustPayToPubKeyAddressWithInlineDatum', but the datum is inline in the Tx.
mustPayToPubKeyAddressWithInlineDatum
    :: forall i o
     . PaymentPubKeyHash
    -> StakingCredential
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToPubKeyAddressWithInlineDatum (PaymentPubKeyHash pkh) sc datum vl =
    singleton (MustPayToAddress (Address (PubKeyCredential pkh) (Just sc)) (Just $ TxOutDatumInline datum) Nothing vl)

{-# INLINABLE mustPayToAddressWithReferenceValidator #-}
-- | @mustPayToAddressWithReferenceValidator@ is a helper that calls @mustPayToAddressWithReferenceScript@.
mustPayToAddressWithReferenceValidator
    :: forall i o
    . Address
    -> ValidatorHash
    -> Maybe (TxOutDatum Datum)
    -> Value
    -> TxConstraints i o
mustPayToAddressWithReferenceValidator addr (ValidatorHash vh) = mustPayToAddressWithReferenceScript addr (ScriptHash vh)

{-# INLINABLE mustPayToAddressWithReferenceMintingPolicy #-}
-- | @mustPayToAddressWithReferenceMintingPolicy@ is a helper that calls @mustPayToAddressWithReferenceScript@.
mustPayToAddressWithReferenceMintingPolicy
    :: forall i o
    . Address
    -> MintingPolicyHash
    -> Maybe (TxOutDatum Datum)
    -> Value
    -> TxConstraints i o
mustPayToAddressWithReferenceMintingPolicy addr (MintingPolicyHash vh) = mustPayToAddressWithReferenceScript addr (ScriptHash vh)

{-# INLINABLE mustPayToAddressWithReferenceScript #-}
-- | @mustPayToAddressWithReferenceScript addr scriptHash d v@ creates a transaction output
-- with an reference script. This allows the script to be used as a reference script.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates an
-- output with @addr@, @scriptHash@, @d@ and @v@ and maybe adds @d@ in the transaction's
-- datum witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the transaction output with
-- @addr@, @scriptHash@, @d@ and @v@ is part of the transaction's outputs.
mustPayToAddressWithReferenceScript
    :: forall i o
    . Address
    -> ScriptHash
    -> Maybe (TxOutDatum Datum)
    -> Value
    -> TxConstraints i o
mustPayToAddressWithReferenceScript addr scriptHash datum value =
    singleton (MustPayToAddress addr datum (Just scriptHash) value)

{-# DEPRECATED mustPayToOtherScript "Use mustPayToOtherScriptWithDatumHash instead" #-}
mustPayToOtherScript :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
mustPayToOtherScript = mustPayToOtherScriptWithDatumHash

{-# INLINABLE mustPayToOtherScriptWithDatumHash #-}
-- | @mustPayToOtherScriptWithDatumHash vh d v@ is the same as
-- 'mustPayToOtherScriptAddressWithDatumHash', but without the staking key hash.
mustPayToOtherScriptWithDatumHash :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
mustPayToOtherScriptWithDatumHash vh dv vl =
    singleton (MustPayToAddress (Address (ScriptCredential vh) Nothing) (Just (TxOutDatumHash dv)) Nothing vl)

{-# INLINABLE mustPayToOtherScriptWithDatumInTx #-}
-- | @mustPayToOtherScriptWithDatumInTx vh d v@ is the same as
-- 'mustPayToOtherScriptAddressWithDatumHash', but without the staking key hash.
mustPayToOtherScriptWithDatumInTx :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
mustPayToOtherScriptWithDatumInTx vh dv vl =
    singleton (MustPayToAddress (Address (ScriptCredential vh) Nothing) (Just (TxOutDatumInTx dv)) Nothing vl)

{-# INLINABLE mustPayToOtherScriptWithInlineDatum #-}
-- | @mustPayToOtherScriptWithInlineDatum vh d v@ is the same as
-- 'mustPayToOtherScriptAddressWithDatumHash', but with an inline datum and without the staking key hash.
mustPayToOtherScriptWithInlineDatum :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
mustPayToOtherScriptWithInlineDatum vh dv vl =
    singleton (MustPayToAddress (Address (ScriptCredential vh) Nothing) (Just (TxOutDatumInline dv)) Nothing vl)

{-# DEPRECATED mustPayToOtherScriptAddress "Use mustPayToOtherScriptAddressWithDatumHash instead" #-}
mustPayToOtherScriptAddress :: forall i o. ValidatorHash -> StakingCredential -> Datum -> Value -> TxConstraints i o
mustPayToOtherScriptAddress = mustPayToOtherScriptAddressWithDatumHash

{-# INLINABLE mustPayToOtherScriptAddressWithDatumHash #-}
-- | @mustPayToOtherScriptAddressWithDatumHash vh svh d v@ locks the value @v@ with the given script
-- hash @vh@ alonside a datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @vh@, @svh@, @d@ and @v@ and adds @d@ in the transaction's datum
-- witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the script transaction output with
-- @vh@, @svh@, @d@ and @v@ is part of the transaction's outputs.
-- For @v@, this means that the transactions output must be at least the given value.
-- The output can contain more, or different tokens, but the requested value @v@ must
-- be present.
mustPayToOtherScriptAddressWithDatumHash :: forall i o. ValidatorHash -> StakingCredential -> Datum -> Value -> TxConstraints i o
mustPayToOtherScriptAddressWithDatumHash vh sc dv vl =
    singleton (MustPayToAddress (Address (ScriptCredential vh) (Just sc)) (Just (TxOutDatumHash dv)) Nothing vl)

{-# INLINABLE mustPayToOtherScriptAddressWithDatumInTx #-}
-- | @mustPayToOtherScriptAddressWithDatumInTx vh svh d v@ locks the value @v@ with the given script
-- hash @vh@ alonside a datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @vh@, @svh@, @d@ and @v@ and adds @d@ in the transaction's datum
-- witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the script transaction output with
-- @vh@, @svh@, @d@ and @v@ is part of the transaction's outputs.
-- For @v@, this means that the transactions output must be at least the given value.
-- The output can contain more, or different tokens, but the requested value @v@ must
-- be present.
mustPayToOtherScriptAddressWithDatumInTx
    :: forall i o. ValidatorHash
    -> StakingCredential
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToOtherScriptAddressWithDatumInTx vh sc dv vl =
    singleton (MustPayToAddress (Address (ScriptCredential vh) (Just sc)) (Just (TxOutDatumInTx dv)) Nothing vl)

{-# INLINABLE mustPayToOtherScriptAddressWithInlineDatum #-}
-- | @mustPayToOtherScriptAddressInlineDatum vh d v@ is the same as
-- 'mustPayToOtherScriptAddressWithDatumHash', but with an inline datum.
mustPayToOtherScriptAddressWithInlineDatum
    :: forall i o. ValidatorHash
    -> StakingCredential
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToOtherScriptAddressWithInlineDatum vh sc dv vl =
    singleton (MustPayToAddress (Address (ScriptCredential vh) (Just sc)) (Just (TxOutDatumInline dv)) Nothing vl)

{-# INLINABLE mustPayToAddress #-}
-- | @mustPayToAddress addr v@ locks the value @v@ at the given address @addr@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @addr@ and @v@.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies
-- that the script transaction output with
-- @addr@ and @v@ is part of the transaction's outputs.
mustPayToAddress :: forall i o. Address -> Value -> TxConstraints i o
mustPayToAddress addr vl =
    singleton (MustPayToAddress addr Nothing Nothing vl)

{-# DEPRECATED mustPayToAddressWithDatum "Use mustPayToAddressWithDatumHash instead" #-}
mustPayToAddressWithDatum :: forall i o. Address -> Datum -> Value -> TxConstraints i o
mustPayToAddressWithDatum = mustPayToAddressWithDatumHash

{-# INLINABLE mustPayToAddressWithDatumHash #-}
-- | @mustPayToAddress addr d v@ locks the value @v@
-- at the given address @addr@ alonside a datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @addr@, @d@ and @v@ and adds @d@ in the transaction's datum
-- witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the script transaction output with
-- @addr@, @d@ and @v@ is part of the transaction's outputs.
mustPayToAddressWithDatumHash :: forall i o. Address -> Datum -> Value -> TxConstraints i o
mustPayToAddressWithDatumHash addr dv vl =
    singleton (MustPayToAddress addr (Just (TxOutDatumHash dv)) Nothing vl)

{-# INLINABLE mustPayToAddressWithDatumInTx #-}
-- | @mustPayToAddressWithDatumInTx addr d v@ locks the value @v@
-- at the given address @addr@ alonside a datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @addr@, @d@ and @v@ and adds @d@ in the transaction's datum
-- witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the script transaction output with
-- @addr@, @d@ and @v@ as part of the transaction's outputs.
mustPayToAddressWithDatumInTx
    :: forall i o. Address
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToAddressWithDatumInTx addr dv vl =
    singleton (MustPayToAddress addr (Just (TxOutDatumInTx dv)) Nothing vl)

{-# INLINABLE mustPayToAddressWithInlineDatum #-}
-- | @mustPayToAddressWithInlineDatum vh d v@ is the same as
-- 'mustPayToAddress', but with an inline datum.
mustPayToAddressWithInlineDatum
    :: forall i o. Address
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayToAddressWithInlineDatum addr dv vl =
    singleton (MustPayToAddress addr (Just (TxOutDatumInline dv)) Nothing vl)

{-# INLINABLE mustMintValue #-}
-- | Same as 'mustMintValueWithRedeemer', but sets the redeemer to the unit
-- redeemer.
mustMintValue :: forall i o. Value -> TxConstraints i o
mustMintValue = mustMintValueWithRedeemer unitRedeemer

{-# INLINABLE mustMintValueWithReference #-}
-- | Same as 'mustMintValueWithRedeemerAndReference', but sets the redeemer to the unit
-- redeemer.
mustMintValueWithReference :: forall i o. TxOutRef -> Value -> TxConstraints i o
mustMintValueWithReference = mustMintValueWithRedeemerAndReference unitRedeemer . Just

{-# INLINABLE mustMintValueWithRedeemer #-}
-- | Same as 'mustMintValueWithRedeemerAndReference', but sets the reference to 'Nothing'.
mustMintValueWithRedeemer :: forall i o. Redeemer -> Value -> TxConstraints i o
mustMintValueWithRedeemer red = mustMintValueWithRedeemerAndReference red Nothing

{-# INLINABLE mustMintValueWithRedeemerAndReference #-}
-- | Same as 'mustMintCurrencyWithRedeemerAndReference', but uses the minting policy hash,
-- token name and amount provided by 'Value'.
--
-- Note that we can derive the 'MintingPolicyHash' from the 'Value'\'s currency
-- symbol.
mustMintValueWithRedeemerAndReference :: forall i o. Redeemer -> Maybe TxOutRef -> Value -> TxConstraints i o
mustMintValueWithRedeemerAndReference red mref =
    foldMap valueConstraint . (AssocMap.toList . Value.getValue)
    where
        valueConstraint (currencySymbol, mp) =
            let hs = Value.currencyMPSHash currencySymbol in
            foldMap (Haskell.uncurry (mustMintCurrencyWithRedeemerAndReference mref hs red))
                    (AssocMap.toList mp)

{-# INLINABLE mustMintCurrency #-}
-- | Same as 'mustMintCurrencyWithRedeemer', but sets the redeemer to the unit
-- redeemer.
mustMintCurrency
    :: forall i o
     . MintingPolicyHash
    -> TokenName
    -> Integer
    -> TxConstraints i o
mustMintCurrency mps = mustMintCurrencyWithRedeemer mps unitRedeemer

{-# INLINABLE mustMintCurrencyWithReference #-}
-- | Same as 'mustMintCurrencyWithRedeemerAndReference', but sets the redeemer to the unit
-- redeemer.
mustMintCurrencyWithReference
    :: forall i o
     . TxOutRef
    -> MintingPolicyHash
    -> TokenName
    -> Integer
    -> TxConstraints i o
mustMintCurrencyWithReference ref mps = mustMintCurrencyWithRedeemerAndReference (Just ref) mps unitRedeemer

{-# INLINABLE mustMintCurrencyWithRedeemer #-}
-- | Same as 'mustMintCurrencyWithRedeemerAndReference', but sets the reference to 'Nothing'.
mustMintCurrencyWithRedeemer
    :: forall i o
     . MintingPolicyHash
    -> Redeemer
    -> TokenName
    -> Integer
    -> TxConstraints i o
mustMintCurrencyWithRedeemer = mustMintCurrencyWithRedeemerAndReference Nothing

{-# INLINABLE mustMintCurrencyWithRedeemerAndReference #-}
-- | @mustMintCurrencyWithRedeemerAndReference mref mph r tn a@ creates the given amount @a@ of
-- the currency specified with @mph@, @r@ and @tn@. The minting policy script can be specified
-- with a reference script @mref@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint mints a currency
-- using @mref@, @mph@, @r@, @tn@ and @a@, adds @mph@ in the transaction's minting
-- policy witness set and adds @r@ in the transaction's redeemer witness set.
-- The minting policy must be provided in the
-- 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.typedValidatorLookups' or
-- 'Ledger.Constraints.OffChain.plutusV1MintingPolicy'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- minted currenty @mref@, @mph@, @tn@ and @a@ is part of the transaction's minting
-- information.
mustMintCurrencyWithRedeemerAndReference
    :: forall i o
     . Maybe TxOutRef
    -> MintingPolicyHash
    -> Redeemer
    -> TokenName
    -> Integer
    -> TxConstraints i o
mustMintCurrencyWithRedeemerAndReference mref mph red tn a =
  if a == 0 then mempty else singleton $ MustMintValue mph red tn a mref

{-# INLINABLE mustSpendAtLeast #-}
-- | @mustSpendAtLeast v@ requires the sum of the transaction's inputs value to
-- be at least @v@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint checks if
-- at least the given value is spent in the transaction.
-- When the transaction is created, a 'MkTxError.DeclaredInputMismatch' error
-- is raised if it is not the case.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- sum of the transaction's inputs value to be at least @v@.
mustSpendAtLeast :: forall i o. Value -> TxConstraints i o
mustSpendAtLeast = singleton . MustSpendAtLeast

{-# INLINABLE mustProduceAtLeast #-}
-- | @mustProduceAtLeast v@ requires the sum of the transaction's outputs value to
-- be at least @v@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint checks if
-- at least the given value is produced in the transaction.
-- When the transaction is created, a 'MkTxError.DeclaredOutputMismatch' error
-- is raised if it is not the case.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- sum of the transaction's outputs value to be at least @v@.
mustProduceAtLeast :: forall i o. Value -> TxConstraints i o
mustProduceAtLeast = singleton . MustProduceAtLeast

{-# INLINABLE mustSpendPubKeyOutput #-}
-- | @mustSpendPubKeyOutput utxo@ must spend the given unspent transaction public key output.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @utxo@ as an
-- input to the transaction. Information about this @utxo@ must be provided in
-- the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.unspentOutputs'.
--
-- If several calls to 'mustSpendPubKeyOutput' are performed for the same 'TxOutRef',
-- only one instance of the constraint is kept when the transaction is created.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- transaction spends this @utxo@.
mustSpendPubKeyOutput :: forall i o. TxOutRef -> TxConstraints i o
mustSpendPubKeyOutput = singleton . MustSpendPubKeyOutput

{-# INLINABLE mustSpendScriptOutput #-}
-- | @mustSpendScriptOutput utxo red@ must spend the given unspent transaction script output.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @utxo@ and
-- @red@ as an input to the transaction. Information about this @utxo@ must be
-- provided in the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.unspentOutputs'. The validator must be either provided by
-- 'Ledger.Constraints.OffChain.unspentOutputs' or through
-- 'Ledger.Constraints.OffChain.otherScript' . The datum must be either provided by
-- 'Ledger.Constraints.OffChain.unspentOutputs' or through
-- 'Ledger.Constraints.OffChain.otherData'.
--
-- If several calls to 'mustSpendScriptOutput' are performed for the same 'TxOutRef',
-- if the two constraints have different redeemers, an error will be thrown when the transaction is created.
-- Otherwise, only one instance of the constraint is kept.
-- If combined with 'mustSpendScriptOutputWithReference' for the same 'TxOutRef', see 'mustSpendScriptOutputWithReference'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- transaction spends this @utxo@.
mustSpendScriptOutput :: forall i o. TxOutRef -> Redeemer -> TxConstraints i o
mustSpendScriptOutput txOutref red = singleton $ MustSpendScriptOutput txOutref red Nothing

{-# INLINABLE mustSpendScriptOutputWithReference #-}
-- | @mustSpendScriptOutputWithReference utxo red refTxOutref@ must spend the given unspent
-- transaction script output, using a script reference as witness.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @utxo@ and
-- @red@ as an input to the transaction, and @refTxOutref@ as reference input.
-- Information about @utxo@ and @refTxOutref@ must be
-- provided in the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.unspentOutputs'. The datum must be either provided by
-- 'Ledger.Constraints.OffChain.unspentOutputs' or through
-- 'Ledger.Constraints.OffChain.otherData'.
--
-- If several calls to 'mustSpendScriptOutputWithReference' are performed for the same 'TxOutRef',
-- if the two constraints have different redeemers,
-- or if the two constraints use a different 'TxOutRef' as a TxOutRef, an error will be thrown when the transaction is
-- created.
-- Otherwise, only one instance of the constraint is kept.
--
-- If combined with 'mustSpendScriptOutput' for the same 'TxOutRef', an error is throw if they have a different
-- redeemer.
-- Otherwise, only one instance of the 'mustSpendScriptOutputWithReference' constraint is kept, the
-- 'mustSpendScriptOutput' constraints are ignored.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- transaction spends this @utxo@.
mustSpendScriptOutputWithReference :: TxOutRef -> Redeemer -> TxOutRef -> TxConstraints i o
mustSpendScriptOutputWithReference txOutref red refTxOutref =
    singleton (MustSpendScriptOutput txOutref red (Just refTxOutref))

{-# INLINABLE mustSpendScriptOutputWithMatchingDatumAndValue #-}
-- | @mustSpendScriptOutputWithMatchingDatumAndValue validatorHash datumPredicate valuePredicate redeemer@
-- must spend an output locked by the given validator script hash,
-- which includes a @Datum@ that matches the given datum predicate and a @Value@ that matches the given value predicate.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint checks that there's exactly one output that matches the
-- requirements, and then adds this as an input to the transaction with the given redeemer.
--
-- The outputs that will be considered need to be privided in the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.unspentOutputs'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that there's at least one input
-- that matches the requirements.
mustSpendScriptOutputWithMatchingDatumAndValue
    :: forall i o. ValidatorHash
    -> (Datum -> Bool)
    -> (Value -> Bool)
    -> Redeemer
    -> TxConstraints i o
mustSpendScriptOutputWithMatchingDatumAndValue vh datumPred valuePred red =
    mempty {
        txConstraintFuns = TxConstraintFuns [MustSpendScriptOutputWithMatchingDatumAndValue vh datumPred valuePred red ]
    }

{-# INLINABLE mustUseOutputAsCollateral #-}
-- | @mustUseOutputAsCollateral utxo@ must use the given unspent transaction output
-- reference as collateral input.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @utxo@ as a
-- collateral input to the transaction.
--
-- In 'Ledger.Constraints.OnChain' this constraint has no effect, since
-- no information about collateral inputs is passed to the scripts.
mustUseOutputAsCollateral :: forall i o. TxOutRef -> TxConstraints i o
mustUseOutputAsCollateral = singleton . MustUseOutputAsCollateral

{-# INLINABLE mustReferenceOutput #-}
-- | @mustReferenceOutput utxo@ must reference (not spend!) the given
-- unspent transaction output reference.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @utxo@ as a
-- reference input to the transaction.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- transaction references this @utxo@.
mustReferenceOutput :: forall i o. TxOutRef -> TxConstraints i o
mustReferenceOutput = singleton . MustReferenceOutput

{-# INLINABLE mustSatisfyAnyOf #-}
mustSatisfyAnyOf :: forall i o. [TxConstraints i o] -> TxConstraints i o
mustSatisfyAnyOf = singleton . MustSatisfyAnyOf . map txConstraints

{-# INLINABLE isSatisfiable #-}
-- | Are the constraints satisfiable?
isSatisfiable :: forall i o. TxConstraints i o -> Bool
isSatisfiable TxConstraints{txConstraints} =
    let intervals = mapMaybe (\case { MustValidateInTimeRange i -> Just i; _ -> Nothing }) txConstraints
        itvl = foldl I.intersection I.always $ map toPlutusInterval intervals
    in not (I.isEmpty itvl)

{-# INLINABLE pubKeyPayments #-}
pubKeyPayments :: forall i o. TxConstraints i o -> [(PaymentPubKeyHash, Value)]
pubKeyPayments TxConstraints{txConstraints} =
    Map.toList
    $ Map.fromListWith (<>)
      (txConstraints >>= \case { MustPayToAddress (Address (PubKeyCredential pk) _) _ _ vl -> [(PaymentPubKeyHash pk, vl)]; _ -> [] })

-- | The minimum 'Value' that satisfies all 'MustSpendAtLeast' constraints
{-# INLINABLE mustSpendAtLeastTotal #-}
mustSpendAtLeastTotal :: forall i o. TxConstraints i o -> Value
mustSpendAtLeastTotal = foldl (\/) mempty . fmap f . txConstraints where
    f (MustSpendAtLeast v) = v
    f _                    = mempty

-- | The minimum 'Value' that satisfies all 'MustProduceAtLeast' constraints
{-# INLINABLE mustProduceAtLeastTotal #-}
mustProduceAtLeastTotal :: forall i o. TxConstraints i o -> Value
mustProduceAtLeastTotal = foldl (\/) mempty . fmap f . txConstraints where
    f (MustProduceAtLeast v) = v
    f _                      = mempty

{-# INLINABLE requiredSignatories #-}
requiredSignatories :: forall i o. TxConstraints i o -> [PaymentPubKeyHash]
requiredSignatories = foldMap f . txConstraints where
    f (MustBeSignedBy pk) = [pk]
    f _                   = []

{-# INLINABLE requiredMonetaryPolicies #-}
requiredMonetaryPolicies :: forall i o. TxConstraints i o -> [MintingPolicyHash]
requiredMonetaryPolicies = foldMap f . txConstraints where
    f (MustMintValue mps _ _ _ _) = [mps]
    f _                           = []

{-# INLINABLE requiredDatums #-}
requiredDatums :: forall i o. TxConstraints i o -> [Datum]
requiredDatums = foldMap f . txConstraints where
    f (MustIncludeDatumInTx dv) = [dv]
    f _                         = []

{-# INLINABLE modifiesUtxoSet #-}
-- | Check whether every transaction that satisfies the constraints has to
-- modify the UTXO set.
modifiesUtxoSet :: forall i o. TxConstraints i o -> Bool
modifiesUtxoSet TxConstraints{txConstraints, txOwnOutputs, txOwnInputs} =
    let requiresInputOutput = \case
            MustSpendAtLeast{}        -> True
            MustProduceAtLeast{}      -> True
            MustSpendPubKeyOutput{}   -> True
            MustSpendScriptOutput{}   -> True
            MustMintValue{}           -> True
            MustPayToAddress _ _ _ vl -> not (isZero vl)
            MustSatisfyAnyOf xs       -> any requiresInputOutput $ concat xs
            _                         -> False
    in any requiresInputOutput txConstraints
        || not (null txOwnOutputs)
        || not (null txOwnInputs)

----------------------
-- Off-chain use only
----------------------

-- | A set of constraints for a transaction that collects PlutusV1 script outputs
-- from the address of the given validator script, using the same redeemer script
-- for all outputs.
spendUtxosFromPlutusV1Script
    :: Map Address (Map TxOutRef DecoratedTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
spendUtxosFromPlutusV1Script= spendUtxosFromPlutusV1ScriptFilter (\_ -> const True)

spendUtxosFromPlutusV1ScriptFilter
    :: (TxOutRef -> DecoratedTxOut -> Bool)
    -> Map Address (Map TxOutRef DecoratedTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
spendUtxosFromPlutusV1ScriptFilter flt am vls red =
    let mp'  = fromMaybe Haskell.mempty $ am ^. at (PV1.mkValidatorAddress vls)
        ourUtxo = Map.filterWithKey flt mp'
    in foldMap (flip mustSpendScriptOutput red) $ Map.keys ourUtxo

-- | Given the pay to script address of the 'Validator', collect from it
-- all the outputs that match a predicate, using the 'RedeemerValue'.
spendUtxosFromTheScriptFilter ::
    forall i o
    .  (TxOutRef -> DecoratedTxOut -> Bool)
    -> Map.Map TxOutRef DecoratedTxOut
    -> i
    -> TxConstraints i o
spendUtxosFromTheScriptFilter flt utxo red =
    let ourUtxo :: Map.Map TxOutRef DecoratedTxOut
        ourUtxo = Map.filterWithKey flt utxo
    in spendUtxosFromTheScript ourUtxo red

-- | A version of 'spendUtxosFromScript' that selects all outputs
-- at the address
spendUtxosFromTheScript ::
    forall i o
    .  Map.Map TxOutRef DecoratedTxOut
    -> i
    -> TxConstraints i o
spendUtxosFromTheScript utxo redeemer =
    foldMap (flip mustSpendOutputFromTheScript redeemer) $ Map.keys utxo

-- | A version of 'spendUtxosFromScript' that selects all outputs
-- at the address
--
-- @utxo@ the set of utxos we search into to find the one we want to spendsOutput
-- @ref@ the reference to the utxo that contains the reference script
spendUtxosFromTheReferencedScript ::
    forall i o
    .  Map.Map TxOutRef DecoratedTxOut
    -> i
    -> TxOutRef
    -> TxConstraints i o
spendUtxosFromTheReferencedScript utxo redeemer ref =
    foldMap (\toSpend -> mustSpendOutputFromTheReferencedScript toSpend redeemer ref) $ Map.keys utxo

-- | A set of constraints for a transaction that collects PlutusV2 script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs.
spendUtxosFromPlutusV2Script
    :: Map Address (Map TxOutRef DecoratedTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
spendUtxosFromPlutusV2Script= spendUtxosFromPlutusV2ScriptFilter (\_ -> const True)

spendUtxosFromPlutusV2ScriptFilter
    :: (TxOutRef -> DecoratedTxOut -> Bool)
    -> Map Address (Map TxOutRef DecoratedTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
spendUtxosFromPlutusV2ScriptFilter flt am vls red = -- (Redeemer red) =
    -- let mp'  = fromMaybe mempty $ am ^. at (PV2.mkValidatorAddress vls)
    -- in spendUtxosFromTheScriptFilter @PlutusTx.BuiltinData @PlutusTx.BuiltinData flt mp' red
    let mp'  = fromMaybe Haskell.mempty $ am ^. at (PV2.mkValidatorAddress vls)
        ourUtxo = Map.filterWithKey flt mp'
    in foldMap (flip mustSpendScriptOutput red) $ Map.keys ourUtxo
