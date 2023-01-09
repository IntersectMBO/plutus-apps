{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}

module Plutus.PAB.Effects.ContractTest.Uniswap
  where

import Control.Monad (forM_, when)
import Data.Semigroup qualified as Semigroup
import Data.Void (Void)
import Ledger
import Ledger.Constraints
import Plutus.Contract
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Script.Utils.Value as Value
import Wallet.Emulator.Types (Wallet (..), walletPubKey)

initContract :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
initContract = do
    ownAddr <- ownAddress
    cur   <- Currency.mintContract ownAddr [(tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs tn amount | tn <- tokenNames]
    forM_ wallets $ \w -> do
        let addr = walletAddress w
        when (addr /= ownAddr) $ do
            mkTxConstraints @Void mempty (mustPayToAddress addr v)
              >>= submitTxConfirmed . adjustUnbalancedTx
    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 4]]

tokenNames :: [TokenName]
tokenNames = ["A", "B", "C", "D"]
