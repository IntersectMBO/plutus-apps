{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingVia               #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE ViewPatterns              #-}
module Solution2 where

import Control.Lens (At (at), iforM_, makeLensesFor, over, set, use, view, (%=), (.=), (<>=))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (catchError, throwError), runExcept, unless)
-- import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.Freer.TH (makeEffect)

import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (Foldable (fold), traverse_)
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

import Control.Monad.Freer (Eff, LastMember, Member, interpret, run, runM, type (~>))
import Control.Monad.Freer.Reader (Reader, ask, asks, runReader)
import Control.Monad.Freer.State (State, execState, get, put, runState)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Semigroup (First (First, getFirst))
import Ledger (ScriptContext (ScriptContext, scriptContextTxInfo), TxInfo (txInfoData))
import Ledger qualified
import Ledger.Address (PaymentPubKey (PaymentPubKey), PaymentPubKeyHash (PaymentPubKeyHash, unPaymentPubKeyHash),
                       StakePubKeyHash, pubKeyHashAddress)
import Ledger.Address qualified as Address
import Ledger.Constraints.OffChain (MkTxError, ScriptLookups, SomeLookupsAndConstraints (SomeLookupsAndConstraints),
                                    UnbalancedTx, ValueSpentBalances (ValueSpentBalances), emptyUnbalancedTx,
                                    requiredSignatories, tx)
import Ledger.Crypto (pubKeyHash)
import Ledger.Orphans ()
import Ledger.Scripts (Datum (Datum), DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, Validator, ValidatorHash,
                       datumHash, mintingPolicyHash, validatorHash)
import Ledger.Tx (ChainIndexTxOut, RedeemerPtr (RedeemerPtr), ScriptTag (Mint), Tx,
                  TxOut (txOutAddress, txOutDatumHash, txOutValue), TxOutRef)
import Ledger.Tx qualified as Tx
import Ledger.Typed.Scripts (Any, TypedValidator, ValidatorTypes (DatumType, RedeemerType))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Typed.Tx (ConnectionError)
import Ledger.Typed.Tx qualified as Typed
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Contexts qualified as V
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Value (Value)
import Plutus.V1.Ledger.Value qualified as Value
import PlutusTx.Prelude (traceIfFalse)
import PlutusTx.Prelude qualified as PlutusTx

data ConstraintProcessingState =
    ConstraintProcessingState
        { cpsUnbalancedTx              :: UnbalancedTx
        -- ^ The unbalanced transaction that we're building
        -- , cpsMintRedeemers             :: Map.Map MintingPolicyHash Redeemer
        -- -- ^ Redeemers for minting policies.
        -- , cpsValueSpentBalancesInputs  :: ValueSpentBalances
        -- -- ^ Balance of the values given and required for the transaction's
        -- --   inputs
        -- , cpsValueSpentBalancesOutputs :: ValueSpentBalances
        -- -- ^ Balance of the values produced and required for the transaction's
        -- --   outputs
        }

makeLensesFor
    [ ("cpsUnbalancedTx", "unbalancedTx")
    , ("cpsMintRedeemers", "mintRedeemers")
    , ("cpsValueSpentBalancesInputs", "valueSpentInputs")
    , ("cpsValueSpentBalancesOutputs", "valueSpentOutputs")
    ] ''ConstraintProcessingState

initialState :: ConstraintProcessingState
initialState = ConstraintProcessingState
    { cpsUnbalancedTx = emptyUnbalancedTx
    -- , cpsMintRedeemers = mempty
    -- , cpsValueSpentBalancesInputs = ValueSpentBalances mempty mempty
    -- , cpsValueSpentBalancesOutputs = ValueSpentBalances mempty mempty
    }

instance Semigroup ConstraintProcessingState where
    (<>) = undefined

instance Monoid ConstraintProcessingState where
    mappend = (<>)
    mempty  = initialState

data TxConstraints m = TxConstraints
    { onChainTxConstraints  :: [OnChainTxConstraint]
    , offChainTxConstraints :: [OffChainTxConstraint m]
    }

instance PlutusTx.Semigroup (TxConstraints effs) where
    l <> r =
        TxConstraints
            { onChainTxConstraints = onChainTxConstraints l <> onChainTxConstraints r
            , offChainTxConstraints = offChainTxConstraints l <> offChainTxConstraints r
            }

instance Semigroup (TxConstraints effs) where
    (<>) = (<>) -- uses PlutusTx.Semigroup instance

instance PlutusTx.Monoid (TxConstraints effs) where
    mempty = TxConstraints [] []

instance Monoid (TxConstraints effs) where
    mappend = (<>)
    mempty  = mempty

class HasConstraintProcessingState m a where
    processConstraint :: a -> m ConstraintProcessingState

class HasScriptContextCheck a where
    checkScriptContext :: ScriptContext -> a -> Bool

data TxConstraint where
    TxConstraint :: a -> TxConstraint

data OnChainTxConstraint where
    OnChainTxConstraint :: (HasScriptContextCheck a) => a -> OnChainTxConstraint

data OffChainTxConstraint m where
    OffChainTxConstraint :: (HasConstraintProcessingState m a) => a -> OffChainTxConstraint m

newtype MustIncludeDatum' = MustIncludeDatum' Datum

instance HasScriptContextCheck MustIncludeDatum' where
    checkScriptContext ScriptContext { scriptContextTxInfo } (MustIncludeDatum' dv) =
        traceIfFalse "L2" -- "Missing datum"
        $ dv `elem` fmap snd (txInfoData scriptContextTxInfo)

instance (Monad m) => HasConstraintProcessingState m MustIncludeDatum' where
    processConstraint (MustIncludeDatum' dv) = do
        let theHash = datumHash dv
        pure $ set (unbalancedTx . tx . Tx.datumWitnesses . at theHash) (Just dv) initialState

{-# INLINABLE mustIncludeDatum' #-}
mustIncludeDatum' :: Datum -> TxConstraints m
mustIncludeDatum' d = mempty { onChainTxConstraints = [ OnChainTxConstraint $ MustIncludeDatum' d ] }

newtype MustBeSignedBy' = MustBeSignedBy' PaymentPubKeyHash

instance HasScriptContextCheck MustBeSignedBy' where
    checkScriptContext ScriptContext { scriptContextTxInfo } (MustBeSignedBy' pkh) =
        traceIfFalse "L4" -- "Missing signature"
        $ scriptContextTxInfo `V.txSignedBy` unPaymentPubKeyHash pkh

instance (Monad m) => HasConstraintProcessingState m MustBeSignedBy' where
    processConstraint (MustBeSignedBy' pkh) = do
        sigs <- undefined -- asks (Map.singleton pkh . Map.lookup pkh)
        pure $ over (unbalancedTx . requiredSignatories) (\x -> x <> sigs) initialState

{-# INLINABLE mustBeSignedBy' #-}
mustBeSignedBy' :: PaymentPubKeyHash -> TxConstraints m
mustBeSignedBy' pkh = mempty { onChainTxConstraints = [ OnChainTxConstraint $ MustBeSignedBy' pkh ] }

{-# INLINABLE checkScriptContext' #-}
checkScriptContext' :: TxConstraints m -> ScriptContext -> Bool
checkScriptContext' constraints ptx =
    traceIfFalse "Ld" -- "checkScriptContext failed"
    $ all f (onChainTxConstraints constraints)
  where
    f (OnChainTxConstraint c) = checkScriptContext ptx c


{-# INLINABLE validator #-}
validator :: Datum -> Redeemer -> ScriptContext -> Bool
validator _ _ = checkScriptContext' (mustIncludeDatum' undefined <> mustBeSignedBy' undefined)

mkTx :: TxConstraints m -> IO ()
mkTx TxConstraints { offChainTxConstraints = (x:xs) } = do
    -- let _ = run $ runReader (Map.empty :: Map PaymentPubKeyHash PaymentPubKey) $ processConstraint x
    let x = run $ runReader (Map.empty :: Map PaymentPubKeyHash PaymentPubKey) $ processConstraint (MustBeSignedBy' undefined)
    let y = fmap f xs
    -- let cs = fold x
    pure ()
  where
      f :: OffChainTxConstraint a -> IO ConstraintProcessingState
      f (OffChainTxConstraint c) = do
          undefined -- processConstraint c
mkTx _ = undefined

main :: IO ()
main = do
    print "Hello"

