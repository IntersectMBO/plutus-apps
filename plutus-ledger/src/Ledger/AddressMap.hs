{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia   #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeFamilies  #-}
-- | 'AddressMap's and functions for working on them.
--
-- 'AddressMap's are used to represent the limited knowledge about the state of the ledger that
-- the wallet retains. Rather than keeping the entire ledger (which can be very large) the wallet
-- only tracks the UTxOs at particular addresses.
module Ledger.AddressMap(
    AddressMap(..),
    UtxoMap,
    addAddress,
    addAddresses,
    fundsAt,
    values,
    traverseWithKey,
    singleton,
    updateAddresses,
    updateAllAddresses,
    lookupOutRef,
    fromChain
    ) where

import Codec.Serialise.Class (Serialise)
import Control.Lens (At (..), Index, IxValue, Ixed (..), Lens', alaf, at, lens, non, (&), (.~), (^.))
import Control.Monad (join)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Monoid (First (..))
import Data.Set qualified as Set
import GHC.Generics (Generic)

import Cardano.Api qualified as C
import Ledger.Address (CardanoAddress)
import Ledger.Blockchain (Blockchain, OnChainTx, consumableInputs, outputsProduced, unOnChain)
import Ledger.Tx (CardanoTx, TxOut (..), txOutAddress, txOutValue)

type UtxoMap = Map C.TxIn (CardanoTx, TxOut)

-- | A map of 'Address'es and their unspent outputs.
newtype AddressMap = AddressMap { getAddressMap :: Map CardanoAddress UtxoMap }
    deriving stock (Show, Eq, Generic)
    deriving newtype (Serialise)
    deriving (ToJSON, FromJSON)

-- | An address map with a single unspent transaction output.
singleton :: (CardanoAddress, C.TxIn, CardanoTx, TxOut) -> AddressMap
singleton (addr, ref, tx, ot) = AddressMap $ Map.singleton addr (Map.singleton ref (tx, ot))

-- | Determine the unspent output that an input refers to
lookupOutRef :: C.TxIn -> AddressMap -> Maybe TxOut
lookupOutRef outRef = fmap snd . alaf First foldMap (Map.lookup outRef) . getAddressMap

instance Semigroup AddressMap where
    (AddressMap l) <> (AddressMap r) = AddressMap (Map.unionWith add l r) where
        add = Map.union

instance Monoid AddressMap where
    mappend = (<>)
    mempty = AddressMap Map.empty

type instance Index AddressMap = CardanoAddress
type instance IxValue AddressMap = Map C.TxIn (CardanoTx, TxOut)

instance Ixed AddressMap where
    ix adr f (AddressMap mp) = AddressMap <$> ix adr f mp

instance At AddressMap where
    at idx = lens g s where
        g (AddressMap mp) = mp ^. at idx
        s (AddressMap mp) utxo = AddressMap $ mp & at idx .~ utxo

-- | Get the funds available at a particular address.
fundsAt :: CardanoAddress -> Lens' AddressMap UtxoMap
fundsAt addr = at addr . non mempty

-- | Add an address with no unspent outputs to a map. If the address already
--   exists, do nothing.
addAddress :: CardanoAddress -> AddressMap -> AddressMap
addAddress adr (AddressMap mp) = AddressMap $ Map.alter upd adr mp where
    upd :: Maybe UtxoMap -> Maybe UtxoMap
    upd = maybe (Just Map.empty) Just

-- | Add a list of 'Address'es with no unspent outputs to the map.
addAddresses :: [CardanoAddress] -> AddressMap -> AddressMap
addAddresses = flip (foldr addAddress)

-- | The total value of unspent outputs (which the map knows about) at an address.
values :: AddressMap -> Map CardanoAddress C.Value
values = Map.map (fold . Map.map (txOutValue . snd)) . getAddressMap

-- | Walk through the address map, applying an effectful function to each entry.
traverseWithKey ::
     Applicative f
  => (CardanoAddress -> Map C.TxIn (CardanoTx, TxOut) -> f (Map C.TxIn (CardanoTx, TxOut)))
  -> AddressMap
  -> f AddressMap
traverseWithKey f (AddressMap m) = AddressMap <$> Map.traverseWithKey f m

-- | Create an 'AddressMap' with the unspent outputs of a single transaction.
fromTxOutputs :: OnChainTx -> AddressMap
fromTxOutputs tx =
    AddressMap . Map.fromListWith Map.union . fmap mkUtxo . Map.toList . outputsProduced $ tx where
    mkUtxo (ref, txo) = (txOutAddress txo, Map.singleton ref (unOnChain tx, txo))

-- | Create a map of unspent transaction outputs to their addresses (the
-- "inverse" of an 'AddressMap', without the values)
knownAddresses :: AddressMap -> Map C.TxIn CardanoAddress
knownAddresses = Map.fromList . unRef . Map.toList . getAddressMap where
    unRef :: [(CardanoAddress, Map C.TxIn (CardanoTx, TxOut))] -> [(C.TxIn, CardanoAddress)]
    unRef lst = do
        (a, outRefs) <- lst
        (rf, _) <- Map.toList outRefs
        pure (rf, a)

-- | Update an 'AddressMap' with the inputs and outputs of a new
-- transaction. @updateAddresses@ does /not/ add or remove any keys from the map.
updateAddresses :: OnChainTx -> AddressMap -> AddressMap
updateAddresses tx utxo = AddressMap $ Map.mapWithKey upd (getAddressMap utxo) where
    -- adds the newly produced outputs, and removes the consumed outputs, for
    -- an address `adr`
    upd :: CardanoAddress -> Map C.TxIn (CardanoTx, TxOut) -> Map C.TxIn (CardanoTx, TxOut)
    upd adr mp = Map.union (producedAt adr) mp `Map.withoutKeys` consumedFrom adr

    -- The TxOutRefs produced by the transaction, for a given address
    producedAt :: CardanoAddress -> Map C.TxIn (CardanoTx, TxOut)
    producedAt adr = Map.findWithDefault mempty adr outputs

    -- The TxOutRefs consumed by the transaction, for a given address
    consumedFrom :: CardanoAddress -> Set.Set C.TxIn
    consumedFrom adr = Map.findWithDefault mempty adr consumedInputs

    AddressMap outputs = fromTxOutputs tx

    consumedInputs = inputs (knownAddresses utxo) tx

-- | Update an 'AddressMap' with the inputs and outputs of a new
-- transaction, including all addresses in the transaction.
updateAllAddresses :: OnChainTx -> AddressMap -> AddressMap
-- updateAddresses handles getting rid of spent outputs, so all we have to do is add in the
-- new things. We can do this by just merging in `fromTxOutputs`, which will have many of the
-- things that are already there, but also the new things.
updateAllAddresses tx utxo = updateAddresses tx utxo <> fromTxOutputs tx

-- | The inputs consumed by a transaction, indexed by address.
inputs ::
    Map C.TxIn CardanoAddress
    -- ^ A map of 'TxOutRef's to their 'Address'es
    -> OnChainTx
    -> Map CardanoAddress (Set.Set C.TxIn)
inputs addrs = Map.fromListWith Set.union
    . fmap (fmap Set.singleton . swap)
    . mapMaybe (\a -> sequence (a, Map.lookup a addrs))
    . consumableInputs

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- | The unspent transaction outputs of the ledger as a whole.
fromChain :: Blockchain -> AddressMap
fromChain = foldr updateAllAddresses mempty . join
