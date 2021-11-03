{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Data type used for minting and burning the thread token value.
module Plutus.Contract.StateMachine.MintingPolarity where

import PlutusTx qualified
import PlutusTx.Prelude
import Prelude qualified as Haskell

data MintingPolarity = Mint | Burn deriving (Haskell.Eq, Haskell.Show)

PlutusTx.makeIsDataIndexed ''MintingPolarity [('Mint,0),('Burn,1)]

instance Eq MintingPolarity where
    Mint == Mint = True
    Burn == Burn = True
    _ == _       = False
