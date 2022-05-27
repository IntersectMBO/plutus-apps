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
    , P.requiredSignatories
    , P.utxoIndex
    , P.validityTimeRange
    , emptyUnbalancedTx
    , P.adjustUnbalancedTx
    , P.MkTxError(..)
    , mkTx
    , mkSomeTx
    -- * Internals exposed for testing
    , P.ValueSpentBalances(..)
    , P.provided
    , P.required
    , P.missingValueSpent
    ) where

import Cardano.Api qualified as C
import Control.Lens (At (at), Traversal', _Left, iforM_, makeLensesFor, over, use, view, (%=), (.=), (<>=))
import Control.Monad (forM_)
import Control.Monad.Except (MonadError (catchError, throwError), runExcept, unless)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (MonadState (get, put), execStateT, gets)

import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (traverse_)
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
import Ledger qualified
import Ledger.Address (PaymentPubKey (PaymentPubKey), PaymentPubKeyHash (PaymentPubKeyHash), StakePubKeyHash,
                       pubKeyHashAddress)
import Ledger.Address qualified as Address
import Ledger.Constraints.TxConstraints (ScriptInputConstraint (ScriptInputConstraint, icRedeemer, icTxOutRef),
                                         ScriptOutputConstraint (ScriptOutputConstraint, ocDatum, ocValue),
                                         TxConstraint (MustPayToPubKeyAddress), TxConstraintFun,
                                         TxConstraintFuns (TxConstraintFuns),
                                         TxConstraints (TxConstraints, txConstraintFuns, txConstraints, txOwnInputs, txOwnOutputs))
import Ledger.Crypto (pubKeyHash)
import Ledger.Orphans ()
import Ledger.Params (Params (..))
import Ledger.Tx (ChainIndexTxOut, RedeemerPtr (RedeemerPtr), ScriptTag (Mint), Tx,
                  TxOut (txOutAddress, txOutDatumHash, txOutValue), TxOutRef)
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI qualified as C
import Ledger.Typed.Scripts (Any, TypedValidator, ValidatorTypes (DatumType, RedeemerType))
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Typed.Tx (ConnectionError)
import Ledger.Typed.Tx qualified as Typed
import Plutus.Script.Utils.V1.Scripts (datumHash, mintingPolicyHash, validatorHash)
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Scripts (Datum (Datum), DatumHash, MintingPolicy, MintingPolicyHash, Redeemer, Validator,
                                 ValidatorHash)
import Plutus.V1.Ledger.Time (POSIXTimeRange)
import Plutus.V1.Ledger.Value (Value)
import Plutus.V1.Ledger.Value qualified as Value

import Ledger (Params (pProtocolParams))
import Ledger.Constraints qualified as P
import Ledger.Constraints.OffChain (UnbalancedTx (..), cpsUnbalancedTx, unbalancedTx, valueSpentOutputs)
import Ledger.Constraints.OffChain qualified as P

tx :: Traversal' UnbalancedTx C.CardanoBuildTx
tx = P.cardanoTx . _Left

emptyCardanoBuildTx :: Params -> C.CardanoBuildTx
emptyCardanoBuildTx Params { pProtocolParams }= C.TxBodyContent
    { C.txIns = mempty
    , C.txInsCollateral = C.TxInsCollateral C.CollateralInAlonzoEra mempty
    , C.txOuts = mempty
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
    }

-- | Given a list of 'SomeLookupsAndConstraints' describing the constraints
--   for several scripts, build a single transaction that runs all the scripts.
mkSomeTx
    :: Params
    -> [P.SomeLookupsAndConstraints]
    -> Either P.MkTxError UnbalancedTx
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
    :: ( FromData (DatumType a)
       , ToData (DatumType a)
       , ToData (RedeemerType a)
       , MonadState P.ConstraintProcessingState m
       , MonadError P.MkTxError m
       )
    => P.ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> m ()
processLookupsAndConstraints lookups TxConstraints{txConstraints, txOwnInputs, txOwnOutputs, txConstraintFuns = TxConstraintFuns txCnsFuns } =
        flip runReaderT lookups $ do
            traverse_ processConstraint txConstraints
            traverse_ P.processConstraintFun txCnsFuns
            traverse_ P.addOwnInput txOwnInputs
            traverse_ P.addOwnOutput txOwnOutputs
            P.addMintingRedeemers
            P.addMissingValueSpent
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
    -> Either P.MkTxError UnbalancedTx
mkTx params lookups txc = mkSomeTx params [P.SomeLookupsAndConstraints lookups txc]

-- | Modify the 'UnbalancedTx' so that it satisfies the constraints, if
--   possible. Fails if a hash is missing from the lookups, or if an output
--   of the wrong type is spent.
processConstraint
    :: ( MonadReader (P.ScriptLookups a) m
       , MonadError P.MkTxError m
       , MonadState P.ConstraintProcessingState m
       )
    => TxConstraint
    -> m ()
processConstraint = \case
    P.MustPayToPubKeyAddress pk skhM mdv vl -> do
        -- if datum is presented, add it to 'datumWitnesses'
        -- forM_ mdv $ \dv -> do
        --     unbalancedTx . tx . Tx.datumWitnesses . at (datumHash dv) .= Just dv
        -- let hash = datumHash <$> mdv
        -- unbalancedTx . tx . Tx.outputs %= (Tx.TxOut{ txOutAddress=pubKeyHashAddress pk skhM
        --                                            , txOutValue=vl
        --                                            , txOutDatumHash=hash
        --                                            } :)
        -- valueSpentOutputs <>= P.provided vl
        pure ()
