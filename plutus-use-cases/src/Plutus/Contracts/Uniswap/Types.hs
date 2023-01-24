{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# options_ghc -Wno-redundant-constraints #-}
{-# options_ghc -fno-specialise            #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Plutus.Contracts.Uniswap.Types
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.Data
import Data.OpenApi qualified as OpenApi
import GHC.Generics (Generic)
import Ledger.Value.Orphans ()
import Plutus.Script.Utils.Value (AssetClass (..), CurrencySymbol (..), TokenName (..), Value, assetClass,
                                  assetClassValue, assetClassValueOf)
import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell
import Text.Printf (PrintfArg)

-- | Uniswap coin token
data U = U deriving (Haskell.Show, Haskell.Eq, Generic, Data)
PlutusTx.makeIsDataIndexed ''U [('U, 0)]
PlutusTx.makeLift ''U

-- | "A"-side coin token
data A = A deriving Data
PlutusTx.makeIsDataIndexed ''A [('A, 0)]
PlutusTx.makeLift ''A

-- | "B"-side coin token
data B = B deriving Data
PlutusTx.makeIsDataIndexed ''B [('B, 0)]
PlutusTx.makeLift ''B

-- | Pool-state coin token
data PoolState = PoolState deriving Data
PlutusTx.makeIsDataIndexed ''PoolState [('PoolState, 0)]
PlutusTx.makeLift ''PoolState

-- | Liquidity-state coin token
data Liquidity = Liquidity deriving Data
PlutusTx.makeIsDataIndexed ''Liquidity [('Liquidity, 0)]
PlutusTx.makeLift ''Liquidity

instance OpenApi.ToSchema BuiltinByteString where
    declareNamedSchema _ = Haskell.pure $ OpenApi.NamedSchema (Just "Bytes") Haskell.mempty

deriving newtype instance OpenApi.ToSchema TokenName
deriving newtype instance OpenApi.ToSchema CurrencySymbol
deriving newtype instance OpenApi.ToSchema AssetClass

-- | A single 'AssetClass'. Because we use three coins, we use a phantom type to track
-- which one is which.
newtype Coin a = Coin { unCoin :: AssetClass }
  deriving stock   (Haskell.Show, Generic, Data)
  deriving newtype (ToJSON, FromJSON, Eq, Haskell.Eq, Haskell.Ord, OpenApi.ToSchema)
PlutusTx.makeIsDataIndexed ''Coin [('Coin, 0)]
PlutusTx.makeLift ''Coin

-- | Likewise for 'Integer'; the corresponding amount we have of the
-- particular 'Coin'.
newtype Amount a = Amount { unAmount :: Integer }
  deriving stock   (Haskell.Show, Generic, Data)
  deriving newtype (ToJSON, FromJSON, Eq, Ord, PrintfArg)
  deriving newtype (Haskell.Eq, Haskell.Ord, Haskell.Num)
  deriving newtype (AdditiveGroup, AdditiveMonoid, AdditiveSemigroup, MultiplicativeSemigroup)
PlutusTx.makeIsDataIndexed ''Amount [('Amount, 0)]
PlutusTx.makeLift ''Amount

{-# INLINABLE valueOf #-}
valueOf :: Coin a -> Amount a -> Value
valueOf c a = assetClassValue (unCoin c) (unAmount a)

{-# INLINABLE unitValue #-}
unitValue :: Coin a -> Value
unitValue c = valueOf c 1

{-# INLINABLE isUnity #-}
isUnity :: Value -> Coin a -> Bool
isUnity v c = amountOf v c == 1

{-# INLINABLE amountOf #-}
amountOf :: Value -> Coin a -> Amount a
amountOf v = Amount . assetClassValueOf v . unCoin

{-# INLINABLE mkCoin #-}
mkCoin:: CurrencySymbol -> TokenName -> Coin a
mkCoin c = Coin . assetClass c

newtype Uniswap = Uniswap
    { usCoin :: Coin U
    } deriving stock    (Haskell.Show, Generic, Data)
      deriving anyclass (ToJSON, FromJSON, OpenApi.ToSchema)
      deriving newtype  (Haskell.Eq, Haskell.Ord)
PlutusTx.makeIsDataIndexed ''Uniswap [('Uniswap, 0)]
PlutusTx.makeLift ''Uniswap

data LiquidityPool = LiquidityPool
    { lpCoinA :: Coin A
    , lpCoinB :: Coin B
    }
    deriving (Haskell.Show, Generic, ToJSON, FromJSON, Data)
PlutusTx.makeIsDataIndexed ''LiquidityPool [('LiquidityPool, 0)]
PlutusTx.makeLift ''LiquidityPool

instance Eq LiquidityPool where
    {-# INLINABLE (==) #-}
    x == y = (lpCoinA x == lpCoinA y && lpCoinB x == lpCoinB y) ||
              -- Make sure the underlying coins aren't equal.
             (unCoin (lpCoinA x) == unCoin (lpCoinB y) && unCoin (lpCoinB x) == unCoin (lpCoinA y))

data UniswapAction = Create LiquidityPool | Close | Swap | Remove | Add
    deriving Haskell.Show
PlutusTx.makeIsDataIndexed ''UniswapAction [ ('Create , 0)
                                           , ('Close,   1)
                                           , ('Swap,    2)
                                           , ('Remove,  3)
                                           , ('Add,     4)
                                           ]
PlutusTx.makeLift ''UniswapAction

data UniswapDatum =
      Factory [LiquidityPool]
    | Pool LiquidityPool (Amount Liquidity)
    deriving stock (Haskell.Show)
PlutusTx.makeIsDataIndexed ''UniswapDatum [ ('Factory, 0)
                                          , ('Pool,    1)
                                          ]
PlutusTx.makeLift ''UniswapDatum
