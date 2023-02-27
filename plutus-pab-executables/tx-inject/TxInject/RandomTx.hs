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
import Hedgehog.Gen qualified as Gen
import System.Random.MWC as MWC

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Generators (TxInputWitnessed (TxInputWitnessed))
import Cardano.Node.Emulator.Generators qualified as Generators
import Cardano.Node.Emulator.Params (Params (pSlotConfig))
import Cardano.Node.Emulator.Validation qualified as Validation
import Ledger.Address (CardanoAddress)
import Ledger.CardanoWallet qualified as CW
import Ledger.Index (UtxoIndex (..))
import Ledger.Slot (Slot (..))
import Ledger.Tx (CardanoTx (CardanoTx), SomeCardanoApiTx (CardanoApiEmulatorEraTx), TxInType (ConsumePublicKeyAddress),
                  txOutAddress, txOutValue)
import Ledger.Tx.CardanoAPI (fromPlutusIndex)
import Ledger.Value.CardanoAPI (isAdaOnlyValue)

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
  -> IO (C.Tx C.BabbageEra)
generateTx gen slot (UtxoIndex utxo) = do
  sourceAddress <- pickNEL gen keyPairs
  -- outputs at the source address
  let sourceOutputs
  -- we restrict ourselves to outputs that contain no currencies other than Ada,
  -- so that we can then split the total amount using 'Generators.splitVal'.
  --
  -- TODO: A generalised version of 'Generators.splitVal' that works on 'Value'
  -- We definitely need this for creating multi currency transactions!
        =
          filter
            (\(_, txOut ) -> isAdaOnlyValue (txOutValue txOut)) $
          filter
            (\(_, txOut) ->
                txOutAddress txOut == sourceAddress) $
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
        sourceTxIns = fmap ((`TxInputWitnessed` ConsumePublicKeyAddress) . fst) inputs
    txn <- Gen.sample $
      Generators.genValidTransactionSpending sourceTxIns sourceAda
    slotCfg <- Gen.sample Generators.genSlotConfig
    let
      params = def { pSlotConfig = slotCfg }
      utxoIndex = either (error . show) id $ fromPlutusIndex $ UtxoIndex utxo
      validationResult = Validation.validateCardanoTx params slot utxoIndex txn
    case validationResult of
      Left _  -> case txn of CardanoTx (CardanoApiEmulatorEraTx cTx) -> pure cTx
      Right _ -> generateTx gen slot (UtxoIndex utxo)

keyPairs :: NonEmpty CardanoAddress
keyPairs = fmap CW.mockWalletAddress (CW.knownMockWallet 1 :| drop 1 CW.knownMockWallets)

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
