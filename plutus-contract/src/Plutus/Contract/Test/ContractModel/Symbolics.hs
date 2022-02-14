{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Plutus.Contract.Test.ContractModel.Symbolics where

import Ledger.Ada qualified as Ada
import Ledger.Value (AssetClass, Value, assetClassValue, isZero, leq)
import PlutusTx.Monoid qualified as PlutusTx

import Data.Aeson qualified as JSON
import Data.Foldable
import Data.Map (Map)
import Data.Map qualified as Map

import Test.QuickCheck.StateModel hiding (Action, Actions, arbitraryAction, initialState, monitoring, nextState,
                                   perform, precondition, shrinkAction, stateAfter)

{- Note [Symbolic Tokens and Symbolic Values]
  Symbolic tokens represent tokens that are created during runtime of a `ContractModel` test.
  It is important that these tokens are *symbolic* as there needs to be a phase-separation
  between the generation and execution part of a `ContractModel` test in order to ensure that
  failing test cases can be shrunk - which is crucial for debugging.

  An important invariant of symbolic values is that different symbolic tokens represent
  different actual tokens. This is enforced by a uniqueness check in the `ContractModel`
  tests.

  A symbolic token is a Var Int. You might expect it to be a Var [AssetClass] but because the
  execution of test code is split between two monads we end up needing two indirections.

  See Note [The Env contract] on how to get the meaning of the symbolic tokens out of the
  inner monad.
-}

-- | Symbolic tokens and values
newtype AssetKey = AssetKey Int deriving (Ord, Eq, Show, Num, JSON.FromJSONKey, JSON.ToJSONKey)
data SymToken = SymToken { symVar :: Var AssetKey, symVarIdx :: String } deriving (Ord, Eq)
data SymValue = SymValue { symValMap :: Map SymToken Integer, actualValPart :: Value } deriving (Show)

instance Show SymToken where
  show (SymToken (Var i) n) = "tok" ++ show i ++ "." ++ n

instance Semigroup SymValue where
  (SymValue m v) <> (SymValue m' v') = SymValue (Map.unionWith (+) m m') (v <> v')
instance Monoid SymValue where
  mempty = SymValue mempty mempty
instance Eq SymValue where
  (SymValue m v) == (SymValue m' v') = Map.filter (/= 0) m == Map.filter (/= 0) m'
                                     && v == v'
-- | Check if a symbolic value is zero
symIsZero :: SymValue -> Bool
symIsZero (SymValue m v) = all (==0) m && isZero v

-- | Check if one symbolic value is less than or equal to another
symLeq :: SymValue -> SymValue -> Bool
symLeq (SymValue m v) (SymValue m' v') = v `leq` v' && all (<=0) (Map.unionWith (+) m (negate <$> m'))

symLeqWiggle :: Integer -> SymValue -> SymValue -> Bool
symLeqWiggle w (SymValue m v) (SymValue m' v') = v `leq` v' && all (<=w) (Map.unionWith (+) m (negate <$> m'))

symAssetClassValue :: SymToken -> Integer -> SymValue
symAssetClassValue _ 0 = SymValue mempty mempty
symAssetClassValue t i = SymValue (Map.singleton t i) mempty

toValue :: (SymToken -> AssetClass) -> SymValue -> Value
toValue symTokenMap (SymValue m v) = v <> fold [ assetClassValue (symTokenMap t) v | (t, v) <- Map.toList m ]

class SymValueLike v where
  toSymValue :: v -> SymValue

instance SymValueLike Value where
  toSymValue = SymValue mempty

instance SymValueLike SymValue where
  toSymValue = id

instance SymValueLike Ada.Ada where
  toSymValue = toSymValue . Ada.toValue

inv :: SymValue -> SymValue
inv (SymValue m v) = SymValue (negate <$> m) (PlutusTx.inv v)
