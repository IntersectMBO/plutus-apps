{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

{-| The chain index' version of a transaction
-}
module Plutus.ChainIndex.Tx(
    ChainIndexTx(..)
    , ChainIndexTxOutputs(..)
    , fromOnChainTx
    , txOutRefs
    , txOutsWithRef
    , txOutRefMap
    , txOutRefMapForAddr
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
    ) where

import Codec.Serialise (Serialise)
import Control.Arrow ((&&&))
import Control.Lens (makeLenses, makePrisms)
import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi qualified as OpenApi
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple (swap)
import GHC.Generics (Generic)
import Ledger (Tx (..))
import Ledger.Address (Address)
import Ledger.Blockchain (OnChainTx (..))
import Ledger.Scripts (Datum, DatumHash, Redeemer, RedeemerHash, Script, ScriptHash, redeemerHash)
import Ledger.Slot (SlotRange)
import Ledger.Tx (Certificate (certificateRedeemer), SomeCardanoApiTx, TxId, TxIn, TxInput (txInputType),
                  TxInputType (TxConsumeScriptAddress), TxOut (txOutAddress), TxOutRef (TxOutRef),
                  Withdrawal (withdrawalRedeemer), fillTxInputWitnesses, txId)
import Prettyprinter

-- | List of outputs of a transaction. There are no outputs if the transaction
-- is invalid.
data ChainIndexTxOutputs =
    InvalidTx -- ^ The transaction is invalid so there is no outputs
  | ValidTx [TxOut]
  deriving (Show, Eq, Generic, ToJSON, FromJSON, Serialise, OpenApi.ToSchema)

makePrisms ''ChainIndexTxOutputs

data ChainIndexTx = ChainIndexTx {
    _citxTxId       :: TxId,
    -- ^ The id of this transaction.
    _citxInputs     :: Set TxIn,
    -- ^ The inputs to this transaction.
    _citxOutputs    :: ChainIndexTxOutputs,
    -- ^ The outputs of this transaction, ordered so they can be referenced by index.
    _citxValidRange :: !SlotRange,
    -- ^ The 'SlotRange' during which this transaction may be validated.
    _citxData       :: Map DatumHash Datum,
    -- ^ Datum objects recorded on this transaction.
    _citxRedeemers  :: Map RedeemerHash Redeemer,
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
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList _citxInputs)))
                , hang 2 (vsep ("outputs:" : fmap pretty outputs))
                , hang 2 (vsep ("scripts hashes:": fmap (pretty . fst) (Map.toList _citxScripts)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
        in nest 2 $ vsep ["Valid tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]
    pretty ChainIndexTx{_citxTxId, _citxInputs, _citxOutputs = InvalidTx, _citxValidRange, _citxData, _citxRedeemers, _citxScripts} =
        let lines' =
                [ hang 2 (vsep ("inputs:" : fmap pretty (Set.toList _citxInputs)))
                , hang 2 (vsep ["no outputs:"])
                , hang 2 (vsep ("scripts hashes:": fmap (pretty . fst) (Map.toList _citxScripts)))
                , "validity range:" <+> viaShow _citxValidRange
                , hang 2 (vsep ("data:": fmap (pretty . snd) (Map.toList _citxData) ))
                , hang 2 (vsep ("redeemers:": fmap (pretty . snd) (Map.toList _citxRedeemers) ))
                ]
        in nest 2 $ vsep ["Invalid tx" <+> pretty _citxTxId <> colon, braces (vsep lines')]

-- | Get tx output references from tx.
txOutRefs :: ChainIndexTx -> [TxOutRef]
txOutRefs ChainIndexTx { _citxTxId, _citxOutputs = ValidTx outputs } =
    map (\idx -> TxOutRef _citxTxId idx) [0 .. fromIntegral $ length outputs - 1]
txOutRefs ChainIndexTx{_citxOutputs = InvalidTx} = []

-- | Get tx output references and tx outputs from tx.
txOutsWithRef :: ChainIndexTx -> [(TxOut, TxOutRef)]
txOutsWithRef tx@ChainIndexTx { _citxOutputs = ValidTx outputs } = zip outputs $ txOutRefs tx
txOutsWithRef ChainIndexTx { _citxOutputs = InvalidTx }          = []

-- | Get 'Map' of tx outputs references to tx.
txOutRefMap :: ChainIndexTx -> Map TxOutRef (TxOut, ChainIndexTx)
txOutRefMap tx =
    fmap (, tx) $ Map.fromList $ fmap swap $ txOutsWithRef tx

-- | Get 'Map' of tx outputs from tx for a specific address.
txOutRefMapForAddr :: Address -> ChainIndexTx -> Map TxOutRef (TxOut, ChainIndexTx)
txOutRefMapForAddr addr tx =
    Map.filter ((==) addr . txOutAddress . fst) $ txOutRefMap tx

-- | Convert a 'OnChainTx' to a 'ChainIndexTx'. An invalid 'OnChainTx' will not
-- produce any 'ChainIndexTx' outputs and the collateral inputs of the
-- 'OnChainTx' will be the inputs of the 'ChainIndexTx'.
fromOnChainTx :: OnChainTx -> ChainIndexTx
fromOnChainTx = \case
    Valid tx@Tx{txInputs, txOutputs, txValidRange, txData, txScripts, txWithdrawals, txCertificates, txMintingScripts} ->
        let  redeemers = allRedeemers txWithdrawals txCertificates txMintingScripts txInputs
        in
        ChainIndexTx
            { _citxTxId = txId tx
            , _citxInputs = Set.fromList $ map (fillTxInputWitnesses tx) txInputs
            , _citxOutputs = ValidTx txOutputs
            , _citxValidRange = txValidRange
            , _citxData = txData
            , _citxRedeemers = redeemersToMap redeemers
            , _citxScripts = txScripts
            , _citxCardanoTx = Nothing
            }
    Invalid tx@Tx{txCollateral, txValidRange, txData, txScripts, txMintingScripts, txWithdrawals, txCertificates} ->
        let  redeemers = allRedeemers txWithdrawals txCertificates txMintingScripts txCollateral in
        ChainIndexTx
            { _citxTxId = txId tx
            , _citxInputs = Set.fromList $ map (fillTxInputWitnesses tx) txCollateral
            , _citxOutputs = InvalidTx
            , _citxValidRange = txValidRange
            , _citxData = txData
            , _citxRedeemers = redeemersToMap redeemers
            , _citxScripts = txScripts
            , _citxCardanoTx = Nothing
            }
    where
        redeemersToMap :: [Redeemer] -> Map RedeemerHash Redeemer
        redeemersToMap = Map.fromList . map (redeemerHash &&& id)

        allRedeemers txWithdrawals txCertificates txMintingScripts txInputs =
            mapMaybe withdrawalRedeemer txWithdrawals
            <> mapMaybe certificateRedeemer txCertificates
            <> Map.elems txMintingScripts
            <> mapMaybe (
                (\case
                    TxConsumeScriptAddress rd _ _ -> Just rd
                    _                             -> Nothing) . txInputType) txInputs
