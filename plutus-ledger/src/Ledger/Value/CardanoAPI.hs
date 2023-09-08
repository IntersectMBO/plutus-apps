{-# LANGUAGE NumericUnderscores #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Ledger.Value.CardanoAPI (
    C.Value
  , C.Lovelace(..)
  , C.AssetId(..)
  , C.PolicyId
  , C.AssetName
  , C.selectAsset
  , C.valueToList
  , C.valueFromList
  , C.selectLovelace
  , C.filterValue
  , C.negateValue
  , lovelaceToValue
  , lovelaceValueOf
  , adaValueOf
  , isZero
  , isAdaOnlyValue
  , noAdaValue
  , adaOnlyValue
  , adaToCardanoValue
  , singleton
  , assetIdValue
  , scale
  , split
  , policyId
  , toCardanoValue
  , fromCardanoValue
  , toCardanoAssetId
  , fromCardanoAssetId
  , combine
  , valueGeq
  , valueLeq
  ) where

import Cardano.Api qualified as C
import Data.Bifunctor (bimap)
import Data.List (partition)
import Data.Maybe (isJust)
import Data.Monoid (All (All, getAll))
import Data.Ratio (denominator, numerator)
import Ledger.Scripts (MintingPolicy (..), Versioned (..), withCardanoApiScript)
import Ledger.Tx.CardanoAPI.Internal (adaToCardanoValue, fromCardanoAssetId, fromCardanoValue, toCardanoAssetId,
                                      toCardanoValue)
import PlutusTx.Lattice (JoinSemiLattice (..))

lovelaceToValue :: C.Lovelace -> C.Value
lovelaceToValue 0 = mempty
lovelaceToValue l = C.lovelaceToValue l

lovelaceValueOf :: Integer -> C.Value
lovelaceValueOf = C.lovelaceToValue . C.Lovelace

adaValueOf :: Rational -> C.Value
adaValueOf r = if denominator l == 1 then lovelaceValueOf (numerator l) else error "Ledger.Value.CardanoAPI: value is not a whole number of lovelace"
  where l = r * 1_000_000

isZero :: C.Value -> Bool
isZero = all (\(_, q) -> q == 0) . C.valueToList

isAdaOnlyValue :: C.Value -> Bool
isAdaOnlyValue = isJust . C.valueToLovelace

noAdaValue :: C.Value -> C.Value
noAdaValue = C.filterValue (/= C.AdaAssetId)

adaOnlyValue :: C.Value -> C.Value
adaOnlyValue = C.filterValue (== C.AdaAssetId)

singleton :: C.PolicyId -> C.AssetName -> Integer -> C.Value
singleton pid an = assetIdValue (C.AssetId pid an)

assetIdValue :: C.AssetId -> Integer -> C.Value
assetIdValue aid n = C.valueFromList [(aid, C.Quantity n)]

scale :: Integer -> C.Value -> C.Value
scale i = C.valueFromList . fmap (fmap (* C.Quantity i)) . C.valueToList

split :: C.Value -> (C.Value, C.Value)
split = bimap (C.negateValue . C.valueFromList) C.valueFromList . partition ((< 0) . snd) . C.valueToList

policyId :: Versioned MintingPolicy -> C.PolicyId
policyId = withCardanoApiScript C.scriptPolicyId . fmap getMintingPolicy

combine :: Monoid m => (C.AssetId -> C.Quantity -> C.Quantity -> m) -> C.Value -> C.Value -> m
combine f v1 v2 = merge (C.valueToList v1) (C.valueToList v2)
    where
        -- Merge assuming the lists are ascending (thanks to Map.toList)
        merge [] [] = mempty
        merge [] ((ar, qr):rs) = f ar 0 qr <> merge [] rs
        merge ((al, ql):ls) [] = f al ql 0 <> merge ls []
        merge ls'@((al, ql):ls) rs'@((ar, qr):rs) = case compare al ar of
            EQ -> f al ql qr <> merge ls rs
            LT -> f al ql 0 <> merge ls rs'
            GT -> f ar 0 qr <> merge ls' rs

valueGeq :: C.Value -> C.Value -> Bool
valueGeq lv rv = getAll $ combine (\_ l r -> All (l >= r)) lv rv

valueLeq :: C.Value -> C.Value -> Bool
valueLeq lv rv = getAll $ combine (\_ l r -> All (l <= r)) lv rv

instance JoinSemiLattice C.Value where
  (\/) = combine (\a ql qr -> C.valueFromList [(a, ql `max` qr)])
