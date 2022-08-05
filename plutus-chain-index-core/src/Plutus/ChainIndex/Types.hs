{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

{-| Misc. types used in this package
-}
module Plutus.ChainIndex.Types(
    ChainIndexTx(..)
    , ChainIndexTxOutputs(..)
    , ChainIndexTxOut(..)
    , ReferenceScript(..)
    , BlockId(..)
    , blockId
    , Tip(..)
    , Point(..)
    , pointsToTip
    , tipAsPoint
    , _PointAtGenesis
    , _Point
    , TxValidity(..)
    , TxStatus
    , TxOutStatus
    , RollbackState(..)
    , TxOutState(..)
    , liftTxOutStatus
    , txOutStatusTxOutState
    , BlockNumber(..)
    , Depth(..)
    , Diagnostics(..)
    , TxConfirmedState(..)
    , TxStatusFailure(..)
    , TxIdState(..)
    , TxUtxoBalance(..)
    , tubUnspentOutputs
    , tubUnmatchedSpentInputs
    , TxOutBalance(..)
    , tobUnspentOutputs
    , tobSpentOutputs
    , ChainSyncBlock(..)
    , TxProcessOption(..)
    -- ** Lenses
    , citxTxId
    , citxInputs
    , citxOutputs
    , citxValidRange
    , citxData
    , citxRedeemers
    , citxScripts
    , citxCardanoTx
    , _InvalidTx
    , _ValidTx
    , fromReferenceScript
    ) where

import Cardano.Api qualified as C
import Codec.Serialise (Serialise)
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Class (Serialise (decode, encode))
import Codec.Serialise.Decoding (decodeListLen, decodeWord)
import Codec.Serialise.Encoding (encodeListLen, encodeWord)
import Control.Lens (makeLenses, makePrisms)
import Control.Monad (void)
import Crypto.Hash (SHA256, hash)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), object, (.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as Aeson
import Data.ByteArray qualified as BA
import Data.ByteString.Lazy qualified as BSL
import Data.Default (Default (..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Monoid (Last (..), Sum (..))
import Data.OpenApi qualified as OpenApi
import Data.Semigroup.Generic (GenericSemigroupMonoid (..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word64)
import GHC.Generics (Generic)
import Ledger (Address, SlotRange, SomeCardanoApiTx, TxIn (..), TxOutRef (..))
import Ledger.Blockchain (BlockId (..))
import Ledger.Blockchain qualified as Ledger
import Ledger.Slot (Slot)
import Ledger.Tx.CardanoAPI (fromCardanoScriptInAnyLang)
import Plutus.V1.Ledger.Scripts (Datum, DatumHash, Script, ScriptHash)
import Plutus.V1.Ledger.Tx (Redeemers, TxId)
import Plutus.V2.Ledger.Api (OutputDatum (..), Value (..))
import PlutusTx.Lattice (MeetSemiLattice (..))
import Prettyprinter
import Prettyprinter.Extras (PrettyShow (..))

data ReferenceScript = ReferenceScriptNone | ReferenceScriptInAnyLang C.ScriptInAnyLang
  deriving (Eq, Show, Generic, Serialise, OpenApi.ToSchema)

instance ToJSON ReferenceScript where
  toJSON (ReferenceScriptInAnyLang s) = object ["referenceScript" .= s]
  toJSON ReferenceScriptNone          = Aeson.Null

instance FromJSON ReferenceScript where
  parseJSON = Aeson.withObject "ReferenceScript" $ \o ->
    case Aeson.lookup "referenceScript" o of
      Nothing        -> pure ReferenceScriptNone
      Just refScript -> ReferenceScriptInAnyLang <$> parseJSON refScript

instance Serialise C.ScriptInAnyLang where
    encode (C.ScriptInAnyLang lang script) =
        let
            -- Since lang is a GADT we have to encode the script in all branches
            other = case lang of
                C.SimpleScriptLanguage C.SimpleScriptV1 -> encodeWord 0 <> encode (C.serialiseToCBOR script)
                C.SimpleScriptLanguage C.SimpleScriptV2 -> encodeWord 1 <> encode (C.serialiseToCBOR script)
                C.PlutusScriptLanguage C.PlutusScriptV1 -> encodeWord 2 <> encode (C.serialiseToCBOR script)
                C.PlutusScriptLanguage C.PlutusScriptV2 -> encodeWord 3 <> encode (C.serialiseToCBOR script)
        in encodeListLen 2 <> other
    decode = do
        len <- decodeListLen
        langWord <- decodeWord
        script <- decode
        case (len, langWord) of
            (2, 0) -> do
                let decoded = either (error "Failed to deserialise AsSimpleScriptV1 from CBOR ") id (C.deserialiseFromCBOR (C.AsScript C.AsSimpleScriptV1) script)
                pure $ C.ScriptInAnyLang (C.SimpleScriptLanguage C.SimpleScriptV1) decoded
            (2, 1) -> do
                let decoded = either (error "Failed to deserialise AsSimpleScriptV2 from CBOR ") id (C.deserialiseFromCBOR (C.AsScript C.AsSimpleScriptV2) script)
                pure $ C.ScriptInAnyLang (C.SimpleScriptLanguage C.SimpleScriptV2) decoded
            (2, 2) -> do
                let decoded = either (error "Failed to deserialise AsPlutusScriptV1 from CBOR ") id (C.deserialiseFromCBOR (C.AsScript C.AsPlutusScriptV1) script)
                pure $ C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) decoded
            (2, 3) -> do
                let decoded = either (error "Failed to deserialise AsPlutusScriptV2 from CBOR ") id (C.deserialiseFromCBOR (C.AsScript C.AsPlutusScriptV2) script)
                pure $ C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV2) decoded
            _ -> fail "Invalid ScriptInAnyLang encoding"

instance OpenApi.ToSchema C.ScriptInAnyLang where
    declareNamedSchema _ = pure $ OpenApi.NamedSchema (Just "ScriptInAnyLang") mempty

fromReferenceScript :: ReferenceScript -> Maybe Script
fromReferenceScript ReferenceScriptNone             = Nothing
fromReferenceScript (ReferenceScriptInAnyLang sial) = fst <$> fromCardanoScriptInAnyLang sial

data ChainIndexTxOut = ChainIndexTxOut
  { citoAddress   :: Address -- ^ We can't use AddressInAnyEra here because of missing FromJson instance for Byron era
  , citoValue     :: Value
  , citoDatum     :: OutputDatum
  , citoRefScript :: ReferenceScript
  } deriving (Eq, Show, Generic, Serialise, OpenApi.ToSchema)

instance ToJSON ChainIndexTxOut where
    toJSON ChainIndexTxOut{..} = object
        [ "address" .= toJSON citoAddress
        , "value" .= toJSON citoValue
        , "datum" .= toJSON citoDatum
        , "refScript" .= toJSON citoRefScript
        ]

instance FromJSON ChainIndexTxOut where
    parseJSON =
        Aeson.withObject "ChainIndexTxOut" $ \obj ->
            ChainIndexTxOut
                <$> obj .: "address"
                <*> obj .: "value"
                <*> obj .: "datum"
                <*> obj .:? "refScript" .!= ReferenceScriptNone

instance Pretty ChainIndexTxOut where
    pretty ChainIndexTxOut {citoAddress, citoValue} =
        hang 2 $ vsep ["-" <+> pretty citoValue <+> "addressed to", pretty citoAddress]

-- | List of outputs of a transaction. There are no outputs if the transaction
-- is invalid.
data ChainIndexTxOutputs =
    InvalidTx -- ^ The transaction is invalid so there is no outputs
  | ValidTx [ChainIndexTxOut]
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialise, OpenApi.ToSchema)

makePrisms ''ChainIndexTxOutputs

data ChainIndexTx = ChainIndexTx {
    _citxTxId       :: TxId,
    -- ^ The id of this transaction.
    _citxInputs     :: [TxIn],
    -- ^ The inputs to this transaction.
    _citxOutputs    :: ChainIndexTxOutputs,
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    _citxValidRange :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    _citxData       :: Map DatumHash Datum,
    -- ^ Datum objects recorded on this transaction.
    _citxRedeemers  :: Redeemers,
    -- ^ Redeemers of the minting scripts.
    _citxScripts    :: Map ScriptHash Script,
    -- ^ The scripts (validator, stake validator or minting) part of cardano tx.
    _citxCardanoTx  :: Maybe SomeCardanoApiTx
    -- ^ The full Cardano API tx which was used to populate the rest of the
    -- 'ChainIndexTx' fields. Useful because 'ChainIndexTx' doesn't have all the
    -- details of the tx, so we keep it as a safety net. Might be Nothing if we
    -- are in the emulator.
    } deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialise, OpenApi.ToSchema)

makeLenses ''ChainIndexTx

instance Pretty ChainIndexTx where
    pretty ChainIndexTx{_citxTxId, _citxInputs, _citxOutputs = ValidTx outputs, _citxValidRange, _citxData, _citxRedeemers, _citxScripts} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty _citxInputs))
                , hang 2 (vsep ("outputs:" : fmap pretty outputs))
                , hang 2 (vsep ("scripts hashes:": fmap (pretty . fst) (Map.toList _citxScripts)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
        in nest 2 $ vsep ["Valid tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]
    pretty ChainIndexTx{_citxTxId, _citxInputs, _citxOutputs = InvalidTx, _citxValidRange, _citxData, _citxRedeemers, _citxScripts} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty _citxInputs))
                , hang 2 (vsep ["no outputs:"])
                , hang 2 (vsep ("scripts hashes:": fmap (pretty . fst) (Map.toList _citxScripts)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
        in nest 2 $ vsep ["Invalid tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]

-- | Compute a hash of the block's contents.
blockId :: Ledger.Block -> BlockId
blockId = BlockId
        . BA.convert
        . hash @_ @SHA256
        . BSL.toStrict
        . CBOR.serialise

newtype BlockNumber = BlockNumber { unBlockNumber :: Word64 }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Num, Real, Enum, Integral, ToJSON, FromJSON, OpenApi.ToSchema)

instance Pretty BlockNumber where
    pretty (BlockNumber blockNumber) =
        "BlockNumber " <> pretty blockNumber

-- | The tip of the chain index.
data Tip =
      TipAtGenesis
    | Tip
        { tipSlot    :: Slot -- ^ Last slot
        , tipBlockId :: BlockId -- ^ Last block ID
        , tipBlockNo :: BlockNumber -- ^ Last block number
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

makePrisms ''Tip

-- | When performing a rollback the chain sync protocol does not provide a block
--   number where to resume from.
data Point =
      PointAtGenesis
    | Point
        { pointSlot    :: Slot -- ^ Slot number
        , pointBlockId :: BlockId -- ^ Block number
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makePrisms ''Point

instance Ord Point where
  PointAtGenesis <= _              = True
  _              <= PointAtGenesis = False
  (Point ls _)   <= (Point rs _)   = ls <= rs

instance Pretty Point where
    pretty PointAtGenesis = "PointAtGenesis"
    pretty Point {pointSlot, pointBlockId} =
        "Point("
     <> pretty pointSlot
     <> comma
     <+> pretty pointBlockId
     <>  ")"

tipAsPoint :: Tip -> Point
tipAsPoint TipAtGenesis = PointAtGenesis
tipAsPoint (Tip tSlot tBlockId _) =
    Point { pointSlot = tSlot
          , pointBlockId = tBlockId
          }

pointsToTip :: Point -> Tip -> Bool
pointsToTip PointAtGenesis TipAtGenesis = True
pointsToTip (Point pSlot pBlockId)
            (Tip   tSlot tBlockId _)
  | tSlot == pSlot && tBlockId == pBlockId = True
pointsToTip _ _ = False

-- | This mirrors the previously defined Tip which used the Last monoid definition.
instance Semigroup Tip where
    t <> TipAtGenesis = t
    _ <> t            = t

instance Semigroup Point where
    t <> PointAtGenesis = t
    _ <> t              = t

instance Monoid Tip where
    mempty = TipAtGenesis

instance Monoid Point where
    mempty = PointAtGenesis

instance Ord Tip where
    TipAtGenesis <= _            = True
    _            <= TipAtGenesis = False
    (Tip _ _ lb) <= (Tip _ _ rb) = lb <= rb

instance Pretty Tip where
    pretty TipAtGenesis = "TipAtGenesis"
    pretty Tip {tipSlot, tipBlockId, tipBlockNo} =
            "Tip("
        <>  pretty tipSlot
        <>  comma
        <+> pretty tipBlockId
        <>  comma
        <+> pretty tipBlockNo
        <>  ")"

-- | Validity of a transaction that has been added to the ledger
data TxValidity = TxValid | TxInvalid | UnknownValidity
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Pretty via (PrettyShow TxValidity)

instance MeetSemiLattice TxValidity where
  TxValid /\ TxValid     = TxValid
  TxInvalid /\ TxInvalid = TxInvalid
  _ /\ _                 = UnknownValidity


-- | How many blocks deep the tx is on the chain
newtype Depth = Depth { unDepth :: Int }
    deriving stock (Eq, Ord, Show, Generic)
    deriving newtype (Num, Real, Enum, Integral, Pretty, ToJSON, FromJSON)

instance MeetSemiLattice Depth where
  Depth a /\ Depth b = Depth (max a b)

{- Note [TxStatus state machine]

The status of a transaction is described by the following state machine.

Current state | Next state(s)
-----------------------------------------------------
Unknown       | OnChain
OnChain       | OnChain, Unknown, Committed
Committed     | -

The initial state after submitting the transaction is Unknown.
-}

-- | The status of a Cardano transaction
type TxStatus = RollbackState ()

-- | The rollback state of a Cardano transaction
data RollbackState a =
    Unknown
    -- ^ The transaction is not on the chain. That's all we can say.
  | TentativelyConfirmed Depth TxValidity a
    -- ^ The transaction is on the chain, n blocks deep. It can still be rolled
    -- back.
  | Committed TxValidity a
    -- ^ The transaction is on the chain. It cannot be rolled back anymore.
  deriving stock (Eq, Ord, Show, Generic, Functor)
  deriving anyclass (ToJSON, FromJSON)
  deriving Pretty via (PrettyShow (RollbackState a))

instance MeetSemiLattice a => MeetSemiLattice (RollbackState a) where
  Unknown /\ a = a
  a /\ Unknown = a
  TentativelyConfirmed d1 v1 a1 /\ TentativelyConfirmed d2 v2 a2 =
    TentativelyConfirmed (d1 /\ d2) (v1 /\ v2) (a1 /\ a2)
  TentativelyConfirmed _ v1 a1 /\ Committed v2 a2 = Committed (v1 /\ v2) (a1 /\ a2)
  Committed v1 a1 /\ TentativelyConfirmed _ v2 a2 = Committed (v1 /\ v2) (a1 /\ a2)
  Committed v1 a1 /\ Committed v2 a2 = Committed (v1 /\ v2) (a1 /\ a2)


{- Note [TxOutStatus state machine]

The status of a transaction output is described by the following state machine.

Current state           | Next state(s)
-----------------------------------------------------
TxOutUnknown            | TxOutTentativelyUnspent
TxOutTentativelyUnspent | TxOutUnknown, TxOutTentativelySpent, TxOutConfirmedUnspent
TxOutTentativelySpent   | TxOutUnknown, TxOutConfirmedSpent
TxOutConfirmedUnspent   | TxOutConfirmedSpent
TxOutConfirmedSpent     | -

The initial state after submitting the transaction is 'TxOutUnknown'.
-}

type TxOutStatus = RollbackState TxOutState

data TxOutState = Spent TxId -- Spent by this transaction
                | Unspent
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  deriving Pretty via (PrettyShow TxOutState)

-- | Maybe extract the 'TxOutState' (Spent or Unspent) of a 'TxOutStatus'.
txOutStatusTxOutState :: TxOutStatus -> Maybe TxOutState
txOutStatusTxOutState Unknown                      = Nothing
txOutStatusTxOutState (TentativelyConfirmed _ _ s) = Just s
txOutStatusTxOutState (Committed _ s)              = Just s

-- | Converts a 'TxOutStatus' to a 'TxStatus'. Possible since a transaction
-- output belongs to a transaction.
--
-- Note, however, that we can't convert a 'TxStatus' to a 'TxOutStatus'.
liftTxOutStatus :: TxOutStatus -> TxStatus
liftTxOutStatus = void

data Diagnostics =
    Diagnostics
        { numTransactions    :: Integer
        , numScripts         :: Integer
        , numAddresses       :: Integer
        , numAssetClasses    :: Integer
        , numUnspentOutputs  :: Int
        , numUnmatchedInputs :: Int
        , someTransactions   :: [TxId]
        , unspentTxOuts      :: [ChainIndexTxOut]
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)

-- | Datatype returned when we couldn't get the state of a tx or a tx output.
data TxStatusFailure
      -- | We couldn't return the status because the 'TxIdState' was in a ...
      -- state ... that we didn't know how to decode in
      -- 'Plutus.ChainIndex.TxIdState.transactionStatus'.
      = TxIdStateInvalid BlockNumber TxId TxIdState
      -- | We couldn't return the status because the 'TxOutBalance' does not
      -- contain the target tx output.
      | TxOutBalanceStateInvalid BlockNumber TxOutRef TxOutBalance
      | InvalidRollbackAttempt BlockNumber TxId TxIdState
      deriving (Show, Eq)

data TxIdState = TxIdState
  { txnsConfirmed :: Map TxId TxConfirmedState
  -- ^ Number of times this transaction has been added as well as other
  -- necessary metadata.
  , txnsDeleted   :: Map TxId (Sum Int)
  -- ^ Number of times this transaction has been deleted.
  }
  deriving stock (Eq, Generic, Show)

-- A semigroup instance that merges the two maps, instead of taking the
-- leftmost one.
instance Semigroup TxIdState where
  TxIdState{txnsConfirmed=c, txnsDeleted=d} <> TxIdState{txnsConfirmed=c', txnsDeleted=d'}
    = TxIdState { txnsConfirmed = Map.unionWith (<>) c c'
                , txnsDeleted   = Map.unionWith (<>) d d'
                }

instance Monoid TxIdState where
    mappend = (<>)
    mempty  = TxIdState { txnsConfirmed=mempty, txnsDeleted=mempty }

data TxConfirmedState =
  TxConfirmedState
    { timesConfirmed :: Sum Int
    , blockAdded     :: Last BlockNumber
    , validity       :: Last TxValidity
    }
    deriving stock (Eq, Generic, Show)
    deriving (Monoid) via (GenericSemigroupMonoid TxConfirmedState)

instance Semigroup TxConfirmedState where
    (TxConfirmedState tc ba v) <> (TxConfirmedState tc' ba' v') =
        TxConfirmedState (tc <> tc') (ba <> ba') (v <> v')

-- | The effect of a transaction (or a number of them) on the tx output set.
data TxOutBalance =
  TxOutBalance
    { _tobUnspentOutputs :: Set TxOutRef
    -- ^ Outputs newly added by the transaction(s)
    , _tobSpentOutputs   :: Map TxOutRef TxId
    -- ^ Outputs spent by the transaction(s) along with the tx id that spent it
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

instance Semigroup TxOutBalance where
    l <> r =
        TxOutBalance
            { _tobUnspentOutputs = _tobUnspentOutputs r
                                <> (_tobUnspentOutputs l `Set.difference` Map.keysSet (_tobSpentOutputs r))
            , _tobSpentOutputs   = _tobSpentOutputs l <> _tobSpentOutputs r
            }

instance Monoid TxOutBalance where
    mappend = (<>)
    mempty = TxOutBalance mempty mempty

makeLenses ''TxOutBalance

-- | The effect of a transaction (or a number of them) on the utxo set.
data TxUtxoBalance =
    TxUtxoBalance
        { _tubUnspentOutputs       :: Set TxOutRef
        -- ^ Outputs newly added by the transaction(s)
        , _tubUnmatchedSpentInputs :: Set TxOutRef
        -- ^ Outputs spent by the transaction(s) that have no matching unspent output
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (FromJSON, ToJSON, Serialise)

makeLenses ''TxUtxoBalance

instance Semigroup TxUtxoBalance where
    l <> r =
        TxUtxoBalance
            { _tubUnspentOutputs       = _tubUnspentOutputs r
                                      <> (_tubUnspentOutputs l `Set.difference` _tubUnmatchedSpentInputs r)
            , _tubUnmatchedSpentInputs = (_tubUnmatchedSpentInputs r `Set.difference` _tubUnspentOutputs l)
                                      <> _tubUnmatchedSpentInputs l
            }

instance Monoid TxUtxoBalance where
    mappend = (<>)
    mempty = TxUtxoBalance mempty mempty


-- | User-customizable options to process a transaction.
-- See #73 for more motivations.
newtype TxProcessOption = TxProcessOption
    { tpoStoreTx :: Bool
    -- ^ Should the chain index store this transaction or not.
    -- If not, only handle the UTXOs.
    -- This, for example, allows applications to skip unwanted pre-Alonzo transactions.
    }
    deriving (Show)

-- We should think twice when setting the default option.
-- For now, it should store all data to avoid weird non-backward-compatible bugs in the future.
instance Default TxProcessOption where
    def = TxProcessOption { tpoStoreTx = True }

-- | A block of transactions to be synced.
data ChainSyncBlock = Block
    { blockTip :: Tip
    , blockTxs :: [(ChainIndexTx, TxProcessOption)]
    }
    deriving (Show)
