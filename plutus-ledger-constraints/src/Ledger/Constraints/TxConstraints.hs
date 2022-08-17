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
module Ledger.Constraints.TxConstraints where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (Bifunctor (bimap))
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty, prettyList), defaultLayoutOptions, hang, layoutPretty, viaShow, vsep, (<+>))

import PlutusTx qualified
import PlutusTx.AssocMap qualified as AssocMap
import PlutusTx.Prelude (Bool (False, True), Foldable (foldMap), Functor (fmap), Integer, JoinSemiLattice ((\/)),
                         Maybe (Just, Nothing), Monoid (mempty), Semigroup ((<>)), any, concat, foldl, map, mapMaybe,
                         not, null, ($), (.), (==), (>>=), (||))

import Ledger.Address (PaymentPubKeyHash, StakePubKeyHash)
import Ledger.Tx (ChainIndexTxOut)
import Plutus.Script.Utils.V1.Address qualified as PV1
import Plutus.Script.Utils.V2.Address qualified as PV2
import Plutus.V1.Ledger.Api (Address, Datum, DatumHash, MintingPolicyHash, POSIXTimeRange, Redeemer, StakeValidatorHash,
                             TxOutRef, Validator, ValidatorHash)
import Plutus.V1.Ledger.Interval qualified as I
import Plutus.V1.Ledger.Scripts (unitRedeemer)
import Plutus.V1.Ledger.Value (TokenName, Value, isZero)
import Plutus.V1.Ledger.Value qualified as Value

import Control.Lens (At (at), (^.))
import Data.Function (const, flip)
import Data.Maybe (fromMaybe)
import Prelude qualified as Haskell
import Prettyprinter.Render.String (renderShowS)

-- | Constraints on transactions that want to spend script outputs
data TxConstraint =
      MustHashDatum DatumHash Datum
    -- ^ The transaction's datum witnesses must contain the given 'DatumHash'
    -- and 'Datum'. Useful when you already have a 'DatumHash' and
    -- want to make sure that it is the actual hash of the 'Datum'.
    | MustIncludeDatum Datum
    -- ^ Like 'MustHashDatum', but the hash of the 'Datum' is computed automatically.
    | MustValidateIn POSIXTimeRange
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
    | MustSpendScriptOutput TxOutRef Redeemer
    -- ^ The transaction must spend the given unspent transaction script output.
    | MustUseOutputAsCollateral TxOutRef
    -- ^ The transaction must include the utxo as collateral input.
    | MustReferenceOutput TxOutRef
    -- ^ The transaction must reference (not spend) the given unspent transaction output.
    | MustMintValue MintingPolicyHash Redeemer TokenName Integer
    -- ^ The transaction must mint the given token and amount.
    | MustPayToPubKeyAddress PaymentPubKeyHash (Maybe StakePubKeyHash) (Maybe Datum) Value
    -- ^ The transaction must create a transaction output with a public key address.
    | MustPayToOtherScript ValidatorHash (Maybe StakeValidatorHash) Datum Value
    -- ^ The transaction must create a transaction output with a script address.
    | MustSatisfyAnyOf [[TxConstraint]]
    -- ^ The transaction must satisfy constraints given as an alternative of conjuctions (DNF),
    -- that is `check (MustSatisfyAnyOf xs) = any (all check) xs`
    deriving stock (Haskell.Show, Generic, Haskell.Eq)
    deriving anyclass (ToJSON, FromJSON)

instance Pretty TxConstraint where
    pretty = \case
        MustIncludeDatum dv ->
            hang 2 $ vsep ["must include datum:", pretty dv]
        MustValidateIn range ->
            "must validate in:" <+> viaShow range
        MustBeSignedBy signatory ->
            "must be signed by:" <+> pretty signatory
        MustSpendAtLeast vl ->
            hang 2 $ vsep ["must spend at least:", pretty vl]
        MustProduceAtLeast vl ->
            hang 2 $ vsep ["must produce at least:", pretty vl]
        MustSpendPubKeyOutput ref ->
            hang 2 $ vsep ["must spend pubkey output:", pretty ref]
        MustSpendScriptOutput ref red ->
            hang 2 $ vsep ["must spend script output:", pretty ref, pretty red]
        MustReferenceOutput ref ->
            hang 2 $ vsep ["must reference output:", pretty ref]
        MustMintValue mps red tn i ->
            hang 2 $ vsep ["must mint value:", pretty mps, pretty red, pretty tn <+> pretty i]
        MustPayToPubKeyAddress pkh skh datum v ->
            hang 2 $ vsep ["must pay to pubkey address:", pretty pkh, pretty skh, pretty datum, pretty v]
        MustPayToOtherScript vlh skh dv vl ->
            hang 2 $ vsep ["must pay to script:", pretty vlh, pretty skh, pretty dv, pretty vl]
        MustHashDatum dvh dv ->
            hang 2 $ vsep ["must hash datum:", pretty dvh, pretty dv]
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
        { icRedeemer :: a -- ^ The typed 'Redeemer' to be used with the target script
        , icTxOutRef :: TxOutRef -- ^ The UTXO to be spent by the target script
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
    mempty { txOwnInputs = [ScriptInputConstraint red txOutRef] }

instance (Pretty a) => Pretty (ScriptInputConstraint a) where
    pretty ScriptInputConstraint{icRedeemer, icTxOutRef} =
        vsep
            [ "Redeemer:" <+> pretty icRedeemer
            , "TxOutRef:" <+> pretty icTxOutRef
            ]

deriving anyclass instance (ToJSON a) => ToJSON (ScriptInputConstraint a)
deriving anyclass instance (FromJSON a) => FromJSON (ScriptInputConstraint a)
deriving stock instance (Haskell.Eq a) => Haskell.Eq (ScriptInputConstraint a)

-- Constraint which specifies that the transaction must produce a transaction
-- output which pays to a target script.
data ScriptOutputConstraint a =
    ScriptOutputConstraint
        { ocDatum :: a -- ^ Typed datum to be used with the target script
        , ocValue :: Value
        } deriving stock (Haskell.Show, Generic, Haskell.Functor)

instance (Pretty a) => Pretty (ScriptOutputConstraint a) where
    pretty ScriptOutputConstraint{ocDatum, ocValue} =
        vsep
            [ "Datum:" <+> pretty ocDatum
            , "Value:" <+> pretty ocValue
            ]

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
-- | @mustValidateIn r@ requires the transaction's validity time range to be contained
--   in @r@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint sets the
-- transaction's validity time range to @r@.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- time range @r@ is entirely contained in the transaction's validity time range.
mustValidateIn :: forall i o. POSIXTimeRange -> TxConstraints i o
mustValidateIn = singleton . MustValidateIn

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

{-# INLINABLE mustHashDatum #-}
-- | @mustHashDatum dh d@ requires the transaction to include the datum hash
-- @dh@ and actual datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @dh@ and @d@
-- in the transaction's datum witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @dh@
-- and @d@ are part of the transaction's datum witness set.
mustHashDatum :: DatumHash -> Datum -> TxConstraints i o
mustHashDatum dvh = singleton . MustHashDatum dvh

{-# INLINABLE mustIncludeDatum #-}
-- | @mustIncludeDatum d@ requires the transaction to include the datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @d@
-- in the transaction's datum witness set alongside it's hash
-- (which is computed automatically).
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@
-- is part of the transaction's datum witness set.
mustIncludeDatum :: forall i o. Datum -> TxConstraints i o
mustIncludeDatum = singleton . MustIncludeDatum

{-# INLINABLE mustPayToTheScript #-}
-- | @mustPayToTheScript d v@ locks the value @v@ with a script alongside a
-- datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @d@ and @v@ and adds @d@ in the transaction's datum witness set.
-- The script address is derived from the typed validator that is provided in
-- the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.typedValidatorLookups'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the new script transaction output with
-- @d@ and @v@ is part of the transaction's outputs.
mustPayToTheScript :: o -> Value -> TxConstraints i o
mustPayToTheScript dt vl =
    mempty { txOwnOutputs = [ScriptOutputConstraint dt vl] }

{-# INLINABLE mustPayToPubKey #-}
-- | @mustPayToPubKey pkh v@ is the same as
-- 'mustPayWithDatumToPubKeyAddress', but without any staking key hash and datum.
mustPayToPubKey :: forall i o. PaymentPubKeyHash -> Value -> TxConstraints i o
mustPayToPubKey pk = singleton . MustPayToPubKeyAddress pk Nothing Nothing

{-# INLINABLE mustPayToPubKeyAddress #-}
-- | @mustPayToPubKeyAddress pkh skh v@ is the same as
-- 'mustPayWithDatumToPubKeyAddress', but without any datum.
mustPayToPubKeyAddress
    :: forall i o
     . PaymentPubKeyHash
    -> StakePubKeyHash
    -> Value
    -> TxConstraints i o
mustPayToPubKeyAddress pkh skh =
     singleton . MustPayToPubKeyAddress pkh (Just skh) Nothing

{-# INLINABLE mustPayWithDatumToPubKey #-}
-- | @mustPayWithDatumToPubKey pkh d v@ is the same as
-- 'mustPayWithDatumToPubKeyAddress', but without the staking key hash.
mustPayWithDatumToPubKey
    :: forall i o
     . PaymentPubKeyHash
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayWithDatumToPubKey pk datum =
    singleton . MustPayToPubKeyAddress pk Nothing (Just datum)

{-# INLINABLE mustPayWithDatumToPubKeyAddress #-}
-- | @mustPayWithDatumToPubKeyAddress pkh skh d v@ locks a transaction output
-- with a public key address.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a public key
-- output with @pkh@, @skh@, @d@ and @v@ and maybe adds @d@ in the transaction's
-- datum witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the public key transaction output with
-- @pkh@, @skh@, @d@ and @v@ is part of the transaction's outputs.
mustPayWithDatumToPubKeyAddress
    :: forall i o
     . PaymentPubKeyHash
    -> StakePubKeyHash
    -> Datum
    -> Value
    -> TxConstraints i o
mustPayWithDatumToPubKeyAddress pkh skh datum =
    singleton . MustPayToPubKeyAddress pkh (Just skh) (Just datum)

{-# INLINABLE mustPayToOtherScript #-}
-- | @mustPayToOtherScript vh d v@ is the same as
-- 'mustPayToOtherScriptAddress', but without the staking key hash.
mustPayToOtherScript :: forall i o. ValidatorHash -> Datum -> Value -> TxConstraints i o
mustPayToOtherScript vh dv vl =
    singleton (MustPayToOtherScript vh Nothing dv vl)

{-# INLINABLE mustPayToOtherScriptAddress #-}
-- | @mustPayToOtherScriptAddress vh svh d v@ locks the value @v@ with the given script
-- hash @vh@ alonside a datum @d@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint creates a script
-- output with @vh@, @svh@, @d@ and @v@ and adds @d@ in the transaction's datum
-- witness set.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that @d@ is
-- part of the datum witness set and that the script transaction output with
-- @vh@, @svh@, @d@ and @v@ is part of the transaction's outputs.
mustPayToOtherScriptAddress :: forall i o. ValidatorHash -> StakeValidatorHash -> Datum -> Value -> TxConstraints i o
mustPayToOtherScriptAddress vh svh dv vl =
    singleton (MustPayToOtherScript vh (Just svh) dv vl)

{-# INLINABLE mustMintValue #-}
-- | Same as 'mustMintValueWithRedeemer', but sets the redeemer to the unit
-- redeemer.
mustMintValue :: forall i o. Value -> TxConstraints i o
mustMintValue = mustMintValueWithRedeemer unitRedeemer

{-# INLINABLE mustMintValueWithRedeemer #-}
-- | Same as 'mustMintCurrentWithRedeemer', but uses the minting policy hash,
-- token name and amount provided by 'Value'.
--
-- Note that we can derive the 'MintingPolicyHash' from the 'Value'\'s currency
-- symbol.
mustMintValueWithRedeemer :: forall i o. Redeemer -> Value -> TxConstraints i o
mustMintValueWithRedeemer red =
    foldMap valueConstraint . (AssocMap.toList . Value.getValue)
    where
        valueConstraint (currencySymbol, mp) =
            let hs = Value.currencyMPSHash currencySymbol in
            foldMap (Haskell.uncurry (mustMintCurrencyWithRedeemer hs red))
                    (AssocMap.toList mp)

{-# INLINABLE mustMintCurrency #-}
-- | Same as 'mustMintCurrentWithRedeemer', but sets the redeemer to the unit
-- redeemer.
mustMintCurrency
    :: forall i o
     . MintingPolicyHash
    -> TokenName
    -> Integer
    -> TxConstraints i o
mustMintCurrency mps = mustMintCurrencyWithRedeemer mps unitRedeemer

{-# INLINABLE mustMintCurrencyWithRedeemer #-}
-- | @mustMintCurrencyWithRedeemer mph r tn a@ creates the given amount @a@ of
-- the currency specified with @mph@, @r@ and @tn@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint mints a currency
-- using @mph@, @r@, @tn@ and @a@, adds @mph@ in the transaction's minting
-- policy witness set and adds @r@ in the transaction's redeemer witness set.
-- The minting policy must be provided in the
-- 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.typedValidatorLookups' or
-- 'Ledger.Constraints.OffChain.plutusV1MintingPolicy'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- minted currenty @mph@, @tn@ and @a@ is part of the transaction's minting
-- information.
mustMintCurrencyWithRedeemer
    :: forall i o
     . MintingPolicyHash
    -> Redeemer
    -> TokenName
    -> Integer
    -> TxConstraints i o
mustMintCurrencyWithRedeemer mps red tn a = if a == 0 then mempty else singleton $ MustMintValue mps red tn a

{-# INLINABLE mustSpendAtLeast #-}
-- | @mustSpendAtLeast v@ requires the sum of the transaction's inputs value to
-- be at least @v@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds the missing
-- input value with an additionnal public key output using the public key hash
-- provided in the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.ownPaymentPubKeyHash' and optionnaly
-- 'Ledger.Constraints.OffChain.ownStakePubKeyHash'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- sum of the transaction's inputs value to be at least @v@.
mustSpendAtLeast :: forall i o. Value -> TxConstraints i o
mustSpendAtLeast = singleton . MustSpendAtLeast

{-# INLINABLE mustProduceAtLeast #-}
-- | @mustProduceAtLeast v@ requires the sum of the transaction's outputs value to
-- be at least @v@.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds the missing
-- output value with an additionnal public key output using the public key hash
-- provided in the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.ownPaymentPubKeyHash' and optionnaly
-- 'Ledger.Constraints.OffChain.ownStakePubKeyHash'.
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
-- 'Ledger.Constraints.OffChain.plutusV1OtherScript' . The datum must be either provided by
-- 'Ledger.Constraints.OffChain.unspentOutputs' or through
-- 'Ledger.Constraints.OffChain.otherData'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that the
-- transaction spends this @utxo@.
mustSpendScriptOutput :: forall i o. TxOutRef -> Redeemer -> TxConstraints i o
mustSpendScriptOutput txOutref = singleton . MustSpendScriptOutput txOutref

{-# INLINABLE mustSpendScriptOutputWithMatchingDatumAndValue #-}
-- | @mustSpendScriptOutputWithMatchingDatumAndValue validatorHash datumPredicate valuePredicate redeemer@
-- must spend an output locked by the given validator script hash,
-- which includes a @Datum@ that matches the given datum predicate and a @Value@ that matches the given value predicate.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint checks that there's exactly one output that matches the requirements,
-- and then adds this as an input to the transaction with the given redeemer.
--
-- The outputs that will be considered need to be privided in the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.unspentOutputs'.
--
-- If used in 'Ledger.Constraints.OnChain', this constraint verifies that there's at least one input
-- that matches the requirements.
mustSpendScriptOutputWithMatchingDatumAndValue :: forall i o. ValidatorHash -> (Datum -> Bool) -> (Value -> Bool) -> Redeemer -> TxConstraints i o
mustSpendScriptOutputWithMatchingDatumAndValue vh datumPred valuePred red =
    mempty {
        txConstraintFuns = TxConstraintFuns [MustSpendScriptOutputWithMatchingDatumAndValue vh datumPred valuePred red ]
    }

{-# INLINABLE mustUseOutputAsCollateral #-}
-- | TODO
mustUseOutputAsCollateral :: forall i o. TxOutRef -> TxConstraints i o
mustUseOutputAsCollateral = singleton . MustUseOutputAsCollateral

{-# INLINABLE mustReferenceOutput #-}
-- | @mustReferenceOutput utxo@ must reference (not spend!) the given
-- unspent transaction output.
--
-- If used in 'Ledger.Constraints.OffChain', this constraint adds @utxo@ as a
-- reference input to the transaction. Information about this @utxo@ must be
-- provided in the 'Ledger.Constraints.OffChain.ScriptLookups' with
-- 'Ledger.Constraints.OffChain.unspentOutputs'.
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
    let intervals = mapMaybe (\case { MustValidateIn i -> Just i; _ -> Nothing }) txConstraints
        itvl = foldl I.intersection I.always intervals
    in not (I.isEmpty itvl)

{-# INLINABLE pubKeyPayments #-}
pubKeyPayments :: forall i o. TxConstraints i o -> [(PaymentPubKeyHash, Value)]
pubKeyPayments TxConstraints{txConstraints} =
    Map.toList
    $ Map.fromListWith (<>)
      (txConstraints >>= \case { MustPayToPubKeyAddress pk _ _ vl -> [(pk, vl)]; _ -> [] })

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
    f (MustMintValue mps _ _ _) = [mps]
    f _                         = []

{-# INLINABLE requiredDatums #-}
requiredDatums :: forall i o. TxConstraints i o -> [Datum]
requiredDatums = foldMap f . txConstraints where
    f (MustIncludeDatum dv) = [dv]
    f _                     = []

{-# INLINABLE modifiesUtxoSet #-}
-- | Check whether every transaction that satisfies the constraints has to
-- modify the UTXO set.
modifiesUtxoSet :: forall i o. TxConstraints i o -> Bool
modifiesUtxoSet TxConstraints{txConstraints, txOwnOutputs, txOwnInputs} =
    let requiresInputOutput = \case
            MustSpendAtLeast{}              -> True
            MustProduceAtLeast{}            -> True
            MustSpendPubKeyOutput{}         -> True
            MustSpendScriptOutput{}         -> True
            MustMintValue{}                 -> True
            MustPayToPubKeyAddress _ _ _ vl -> not (isZero vl)
            MustPayToOtherScript _ _ _ vl   -> not (isZero vl)
            MustSatisfyAnyOf xs             -> any requiresInputOutput $ concat xs
            _                               -> False
    in any requiresInputOutput txConstraints
        || not (null txOwnOutputs)
        || not (null txOwnInputs)

----------------------
-- Off-chain use only
----------------------

-- | A set of constraints for a transaction that collects PlutusV1 script outputs
-- from the address of the given validator script, using the same redeemer script
-- for all outputs.
collectFromPlutusV1Script
    :: Map Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromPlutusV1Script= collectFromPlutusV1ScriptFilter (\_ -> const True)

collectFromPlutusV1ScriptFilter
    :: (TxOutRef -> ChainIndexTxOut -> Bool)
    -> Map Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromPlutusV1ScriptFilter flt am vls red =
    let mp'  = fromMaybe Haskell.mempty $ am ^. at (PV1.mkValidatorAddress vls)
        ourUtxo = Map.filterWithKey flt mp'
    in foldMap (flip mustSpendScriptOutput red) $ Map.keys ourUtxo

-- | Given the pay to script address of the 'Validator', collect from it
-- all the outputs that match a predicate, using the 'RedeemerValue'.
collectFromTheScriptFilter ::
    forall i o
    .  (TxOutRef -> ChainIndexTxOut -> Bool)
    -> Map.Map TxOutRef ChainIndexTxOut
    -> i
    -> TxConstraints i o
collectFromTheScriptFilter flt utxo red =
    let ourUtxo :: Map.Map TxOutRef ChainIndexTxOut
        ourUtxo = Map.filterWithKey flt utxo
    in collectFromTheScript ourUtxo red

-- | A version of 'collectFromScript' that selects all outputs
-- at the address
collectFromTheScript ::
    forall i o
    .  Map.Map TxOutRef ChainIndexTxOut
    -> i
    -> TxConstraints i o
collectFromTheScript utxo redeemer =
    foldMap (flip mustSpendOutputFromTheScript redeemer) $ Map.keys utxo

-- | A set of constraints for a transaction that collects PlutusV2 script outputs
--   from the address of the given validator script, using the same redeemer
--   script for all outputs.
collectFromPlutusV2Script
    :: Map Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromPlutusV2Script= collectFromPlutusV2ScriptFilter (\_ -> const True)

collectFromPlutusV2ScriptFilter
    :: (TxOutRef -> ChainIndexTxOut -> Bool)
    -> Map Address (Map TxOutRef ChainIndexTxOut)
    -> Validator
    -> Redeemer
    -> UntypedConstraints
collectFromPlutusV2ScriptFilter flt am vls red = -- (Redeemer red) =
    -- let mp'  = fromMaybe mempty $ am ^. at (PV2.mkValidatorAddress vls)
    -- in collectFromTheScriptFilter @PlutusTx.BuiltinData @PlutusTx.BuiltinData flt mp' red
    let mp'  = fromMaybe Haskell.mempty $ am ^. at (PV2.mkValidatorAddress vls)
        ourUtxo = Map.filterWithKey flt mp'
    in foldMap (flip mustSpendScriptOutput red) $ Map.keys ourUtxo

-- TODO Uncomment and modify once PlutusV2 TypedValidator are available
-- -- | Given the pay to script address of the 'Validator', collect from it
-- --   all the outputs that match a predicate, using the 'RedeemerValue'.
-- collectFromTheScriptFilter ::
--     forall i o
--     .  (TxOutRef -> ChainIndexTxOut -> Bool)
--     -> Map.Map TxOutRef ChainIndexTxOut
--     -> i
--     -> TxConstraints i o
-- collectFromTheScriptFilter flt utxo red =
--     let ourUtxo :: Map.Map TxOutRef ChainIndexTxOut
--         ourUtxo = Map.filterWithKey flt utxo
--     in collectFromTheScript ourUtxo red

-- -- | A version of 'collectFromScript' that selects all outputs
-- --   at the address
-- collectFromTheScript ::
--     forall i o
--     .  Map.Map TxOutRef ChainIndexTxOut
--     -> i
--     -> TxConstraints i o
-- collectFromTheScript utxo redeemer =
--     foldMap (flip mustSpendOutputFromTheScript redeemer) $ Map.keys utxo
