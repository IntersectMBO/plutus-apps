{-# LANGUAGE ViewPatterns #-}
module Plutus.Contract.Test.ContractModel.MissingLovelace
  ( calculateDelta
  ) where

import Data.Set qualified as Set
import Ledger.Ada qualified as Ada
import Ledger.Value (Value, noAdaValue)
import PlutusTx.Prelude qualified as P
import Wallet.Emulator (Wallet)

-- | Returns the calculated delta between initial and final values. Might be false positive.
--
-- The tests check if a wallet's funds are equal to some expected value at the end.
-- Unfortunately, because of the adjustion of transactions, the outputs' costs change
-- and it's hard to track these changes in the tests layer.
--
-- This function tries to check if the difference between final and initial values ('realDelta')
-- is a result of combination of operations between output's costs and the expected delta.
calculateDelta
  :: Value
  -- ^ Expected delta of the test
  -> Ada.Ada
  -- ^ Initial value of the wallet before the test
  -> Ada.Ada
  -- ^ Final value of the wallet after the test
  -> Wallet
  -- ^ Testing wallet
  -> [(Wallet, [Ada.Ada])]
  -- ^ Missing lovelace costs of outputs from 'AdjustingUnbalancedTx' logs
  -> Value
calculateDelta expectedDelta initialValue finalValue w (Set.toList . Set.fromList -> txOutCosts) =
  let
    expectedAda = Ada.fromValue expectedDelta

    -- the list of deltas: combinations (+/-) between outputs' costs,
    -- the expected delta and the wallet's output costs.
    deltas =
      let wTxOutCosts = concatMap snd $ filter ((== w) . fst) txOutCosts
          allWalletsTxOutCosts = concatMap snd txOutCosts
      in map P.abs $ concat
        [ [ P.abs val P.- P.abs wCost
          , P.abs val P.+ P.abs wCost ] | val <- [expectedAda, 0] ++ wTxOutCosts
                                        , wCost <- allWalletsTxOutCosts ]

    realDelta = finalValue P.- initialValue

    missingDelta =
      -- We check if 'realDelta' is a result of combination of operations between initial delta and outputs' costs
      -- by checking if 'realDelta''s is divisible by any delta without a reminder.
      if or [(P.abs realDelta) `mod` d == 0 | d <- deltas, d /= 0] then
        -- if yes, we return a sum of 'realDelta''s ada with non-ada value of the expected delta
        let missingAda = Ada.toValue realDelta
            missingNonAda = noAdaValue expectedDelta
        in missingAda <> missingNonAda
      -- otherwise we just return the expected delta
      else expectedDelta
  in
    -- if ada in the expected delta is the same as the real delta, then we don't need to check anything
    -- and can just return it
    if expectedAda == realDelta then expectedDelta
    -- otherwise we return the missing delta that is needed to pass the test
    else missingDelta
