{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module TxInject.RandomTx(
    -- $randomTx
    generateTx
    ) where

import Control.Monad.Primitive (PrimMonad, PrimState)
import Data.Default (def)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Hedgehog.Gen qualified as Gen
import System.Random.MWC as MWC

import Ledger.Ada qualified as Ada
import Ledger.Address (PaymentPrivateKey, PaymentPubKey)
import Ledger.Address qualified as Address
import Ledger.CardanoWallet qualified as CW
import Ledger.Generators qualified as Generators
import Ledger.Index (UtxoIndex (..), ValidationCtx (..), runValidation, validateTransaction)
import Ledger.Params (Params (pSlotConfig))
import Ledger.Slot (Slot (..))
import Ledger.Tx (Tx, TxOut (..))
import Ledger.Tx qualified as Tx

-- $randomTx
-- Generate a random, valid transaction that moves some ada
-- around between the emulator wallets.

{- | This function will generate a random transaction, given a `GenIO` and a
     `ChainState`.

     It is worth mentioning that, for now, this is as much as we need, however,
     should we need to generate transactions such that, for example, we are growing
     the Utxo index (which could be useful for performace testing) we will have to
     change this function quite a bit.

     Right now, the Utxo it will generate will be 10 (the number of values we are
     splitting the source into) * 2 (due to the random distribuition) * 10 (the
     number of wallets) ~ 200 entries.

     The Utxo generation will also be heavily affected by the `splitVal` function
     implementation. Please make sure to read it's documentation if you want to split
     the value into more than 10 outputs.
-}
generateTx
  :: GenIO       -- ^ Reused across all function invocations (for performance reasons).
  -> Slot        -- ^ Used to validate transctions.
  -> UtxoIndex   -- ^ Used to generate new transactions.
  -> IO Tx
generateTx gen slot (UtxoIndex utxo) = do
  (_, sourcePubKey) <- pickNEL gen keyPairs
  let sourceAddress = Address.pubKeyAddress sourcePubKey Nothing
  -- outputs at the source address
      sourceOutputs
  -- we restrict ourselves to outputs that contain no currencies other than Ada,
  -- so that we can then split the total amount using 'Generators.splitVal'.
  --
  -- TODO: A generalised version of 'Generators.splitVal' that works on 'Value'
  -- We definitely need this for creating multi currency transactions!
        =
          filter
            (\(_, TxOut {txOutValue}) ->
                txOutValue ==
                  Ada.toValue (Ada.fromValue txOutValue)) $
          filter
            (\(_, TxOut {txOutAddress}) ->
                txOutAddress == sourceAddress) $
          Map.toList utxo
  -- list of inputs owned by 'sourcePrivKey' that we are going to spend
  -- in the transaction
  inputs <- sublist gen sourceOutputs
  if null inputs
  then generateTx gen slot (UtxoIndex utxo)
  else do
    -- Total Ada amount that we want to spend
    let sourceAda =
          foldMap
            (txOutValue . snd)
            inputs
        -- inputs of the transaction
        sourceTxIns = Set.fromList $ fmap (Tx.pubKeyTxIn . fst) inputs
    tx <- Gen.sample $
      Generators.genValidTransactionSpending sourceTxIns sourceAda
    slotCfg <- Gen.sample Generators.genSlotConfig
    let (validationResult, _) =
          runValidation (validateTransaction slot tx) (ValidationCtx (UtxoIndex utxo) (def { pSlotConfig = slotCfg }))
    case validationResult of
      Nothing -> pure tx
      Just  _ -> generateTx gen slot (UtxoIndex utxo)

keyPairs :: NonEmpty (PaymentPrivateKey, PaymentPubKey)
keyPairs =
    fmap
        (\mockWallet -> (CW.paymentPrivateKey mockWallet, CW.paymentPubKey mockWallet))
        (CW.knownMockWallet 1 :| drop 1 CW.knownMockWallets)

-- | Pick a random element from a non-empty list
pickNEL :: PrimMonad m => Gen (PrimState m) -> NonEmpty a -> m a
pickNEL gen (x :| xs) = fmap (fromMaybe x) $ pick gen (x : xs)

-- | Pick a random element from a list
pick :: PrimMonad m => Gen (PrimState m) -> [a] -> m (Maybe a)
pick _ [] = return Nothing
pick gen xs = do
    idx <- MWC.uniformR (0, pred $ length xs) gen
    return $ Just $ xs !! idx

-- | Pick a random sublist
sublist :: PrimMonad m => Gen (PrimState m) -> [a] -> m [a]
sublist gen list = do
    includes <- traverse (\_ -> MWC.uniform gen) [1 .. length list]
    return $ fmap fst $ filter snd $ zip list includes
