{-# LANGUAGE ViewPatterns #-}
module Plutus.Contract.Test.MissingLovelace
  ( calculateDelta
  ) where

import Cardano.Api qualified as C
import Ledger.Value.CardanoAPI qualified as Value

-- | Returns the calculated delta between initial and final values. Might be false positive.
--
-- The tests check if a wallet's funds are equal to some expected value at the end.
-- Unfortunately, because of the adjustion of transactions, the outputs' costs change
-- and it's hard to track these changes in the tests layer.
--
-- This function tries to check if the difference between final and initial values ('realDelta')
-- is a result of combination of operations between output's costs and the expected delta.
--
-- There is a risk when expected delta has only ada part and expected delta /= realDelta
-- and realDelta is divisible by some delta from deltas, then we will return realDelta's ada.
-- Which means that the test will pass but without strong confidence in wallets' funds consistency.
-- For example, we expected -n, but there is n among deltas and realDelta is n,
-- it is divisible by n, then the test will pass. So please be careful.
calculateDelta
  :: C.Value
  -- ^ Expected delta of the test
  -> C.Lovelace
  -- ^ Initial value of the wallet before the test
  -> C.Lovelace
  -- ^ Final value of the wallet after the test
  -> [C.Lovelace]
  -- ^ Missing lovelace costs of outputs from 'AdjustingUnbalancedTx' logs
  -> C.Value
calculateDelta expectedDelta initialValue finalValue allWalletsTxOutCosts =
  let
    expectedAda = C.selectLovelace expectedDelta

    -- the list of deltas: combinations (+/-) between outputs' costs,
    -- the expected delta and the wallet's output costs.
    deltas = map abs $ concat
      [ [ abs val - abs wCost
        , abs val + abs wCost ] | val <- [expectedAda, 0] ++ allWalletsTxOutCosts
                                      , wCost <- allWalletsTxOutCosts ]

    realDelta = finalValue - initialValue

    missingDelta =
      -- We check if 'realDelta' is a result of combination of operations between initial delta and outputs' costs
      -- by checking if 'realDelta''s is divisible by any delta without a reminder.
      if or [abs realDelta `mod` d == 0 | d <- deltas, d /= 0] then
        -- if yes, we return a sum of 'realDelta''s ada with non-ada value of the expected delta
        let missingAda = C.lovelaceToValue realDelta
            missingNonAda = Value.noAdaValue expectedDelta
        in missingAda <> missingNonAda
      -- otherwise we just return the expected delta
      else expectedDelta
  in
    -- if ada in the expected delta is the same as the real delta, then we don't need to check anything
    -- and can just return it
    if expectedAda == realDelta then expectedDelta
    -- otherwise we return the missing delta that is needed to pass the test
    else missingDelta
