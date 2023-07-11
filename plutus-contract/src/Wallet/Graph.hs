{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

-- | Support for visualisation of a blockchain as a graph.
module Wallet.Graph
  ( txnFlows
  , graph
  , FlowGraph
  , FlowLink
  , TxRef
  , UtxOwner
  , UtxoLocation
  ) where

import Data.Aeson.Types (ToJSON, toJSON)
import Data.Bifunctor (first)
import Data.List (nub)
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import Data.Text qualified as Text
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import Ledger.Address
import Ledger.Blockchain
import Ledger.Credential (Credential (..))
import Ledger.Crypto
import Ledger.Index qualified as Index
import Ledger.Tx

-- | The owner of an unspent transaction output.
data UtxOwner
  = PubKeyOwner PubKey
    -- ^ Funds owned by a known public key.
  | ScriptOwner
    -- ^ Funds locked by script.
  | OtherOwner
    -- ^ All other funds (that is, funds owned by a public key we are not interested in).
  deriving (Eq, Ord, Show, Generic, ToJSON)

-- | Given a set of known public keys, compute the owner of a given transaction output.
owner :: Set.Set PubKey -> TxOut -> UtxOwner
owner keys tx =
  let hashMap = foldMap (\pk -> Map.singleton (pubKeyHash pk) pk) keys
  in case cardanoAddressCredential (txOutAddress tx) of
    ScriptCredential{}                                       -> ScriptOwner
    PubKeyCredential pkh | Just pk <- Map.lookup pkh hashMap -> PubKeyOwner pk
    _                                                        -> OtherOwner

-- | A wrapper around the first 8 digits of a 'TxId'.
newtype TxRef =
  TxRef Text.Text
  deriving (Eq, Ord, Show, Generic)

instance ToJSON TxRef where
  toJSON (TxRef t) = toJSON t

mkRef :: C.TxId -> TxRef
mkRef (C.TxId txId) = TxRef . Text.pack . take 8 $ show txId

-- | The location of a transaction in a blockchain specified by two indices: the index of the containing
-- block in the chain, and the index of the transaction within the block.
data UtxoLocation = UtxoLocation
  { utxoLocBlock    :: Integer
  , utxoLocBlockIdx :: Integer
  } deriving (Eq, Ord, Show, Generic, ToJSON)

-- | A link in the flow graph.
data FlowLink = FlowLink
  { flowLinkSource    :: TxRef -- ^ The source transaction.
  , flowLinkTarget    :: TxRef -- ^ The target transaction.
  , flowLinkValue     :: Integer -- ^ The value of Ada along this edge.
  , flowLinkOwner     :: UtxOwner -- ^ The owner of this edge.
  , flowLinkSourceLoc :: UtxoLocation -- ^ The location of the source transaction.
  , flowLinkTargetLoc :: Maybe UtxoLocation -- ^ The location of the target transaction, if 'Nothing' then it is unspent.
  } deriving (Show, Generic, ToJSON)

-- | The flow graph, consisting of a set of nodes ('TxRef's) and edges ('FlowLink's).
data FlowGraph = FlowGraph
  { flowGraphLinks :: [FlowLink]
  , flowGraphNodes :: [TxRef]
  } deriving (Show, Generic, ToJSON)

-- | Construct a graph from a list of 'FlowLink's.
graph :: [FlowLink] -> FlowGraph
graph lnks = FlowGraph {..}
  where
    flowGraphLinks = lnks
    flowGraphNodes = nub $ fmap flowLinkSource lnks ++ fmap flowLinkTarget lnks

-- | Compute the 'FlowLink's for a 'Blockchain' given a set of known 'PubKey's.
txnFlows :: [PubKey] -> Blockchain -> [FlowLink]
txnFlows keys bc = catMaybes (utxoLinks ++ foldMap extract bc')
  where
    bc' = foldMap (\(blockNum, txns) -> fmap (first (UtxoLocation blockNum)) txns) $ zipWithIndex $ zipWithIndex <$> reverse bc

    sourceLocations :: Map.Map C.TxIn UtxoLocation
    sourceLocations = Map.fromList $ foldMap (uncurry outRefsWithLoc) bc'

    knownKeys :: Set.Set PubKey
    knownKeys = Set.fromList keys

    index = Index.initialise bc
    utxos = Map.keys $ C.unUTxO index
    utxoLinks = uncurry (flow Nothing) <$> zip (utxoTargets <$> utxos) utxos

    extract :: (UtxoLocation, OnChainTx) -> [Maybe FlowLink]
    extract (loc, tx) =
      let targetRef = mkRef $ getCardanoTxId $ unOnChain tx in
      fmap (flow (Just loc) targetRef) (consumableInputs tx)
    -- make a flow for a TxOutRef

    flow :: Maybe UtxoLocation -> TxRef -> C.TxIn -> Maybe FlowLink
    flow tgtLoc tgtRef rf@(C.TxIn txId _) = do
      src <- Index.lookup rf index
      sourceLoc <- Map.lookup rf sourceLocations
      let sourceRef = mkRef txId
      pure FlowLink
            { flowLinkSource = sourceRef
            , flowLinkTarget = tgtRef
            , flowLinkValue = fromIntegral $ C.selectLovelace $ txOutValue src
            , flowLinkOwner = owner knownKeys src
            , flowLinkSourceLoc = sourceLoc
            , flowLinkTargetLoc = tgtLoc
            }

    zipWithIndex = zip [1..]

-- | Annotate the 'TxOutRef's produced by a transaction with the location of the transaction.
outRefsWithLoc :: UtxoLocation -> OnChainTx -> [(C.TxIn, UtxoLocation)]
outRefsWithLoc loc = eitherTx (const []) (fmap (\txo -> (snd txo, loc)) . getCardanoTxOutRefs)

-- | Create a 'TxRef' from a 'TxOutRef'.
utxoTargets :: C.TxIn -> TxRef
utxoTargets (C.TxIn (C.TxId rf) (C.TxIx idx)) = TxRef $ Text.unwords ["utxo", Text.pack $ take 8 $ show rf, Text.pack $ show idx]
