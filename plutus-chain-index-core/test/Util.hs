{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Util where

import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Ledger (TxOutRef)
import Ledger.Address (CardanoAddress)
import Plutus.ChainIndex (Page (pageItems), PageQuery (PageQuery), utxoSetAtAddress)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.Effects (ChainIndexQueryEffect)
import Plutus.ChainIndex.Tx (ChainIndexTx, ChainIndexTxOut (ChainIndexTxOut, citoAddress), txOuts)

-- | Get all address credentials from a block.
addrCredsFromBlock :: [ChainIndexTx] -> [CardanoAddress]
addrCredsFromBlock =
  fmap (\ChainIndexTxOut{citoAddress} -> citoAddress)
  . foldMap txOuts

-- | Get the UTxO set from a block.
utxoSetFromBlockAddrs :: Member ChainIndexQueryEffect effs => [ChainIndexTx] -> Eff effs [[TxOutRef]]
utxoSetFromBlockAddrs block = forM (addrCredsFromBlock block) $ \addr -> do
  let pq = PageQuery 200 Nothing
  UtxosResponse _ utxoRefs <- utxoSetAtAddress pq addr
  pure $ pageItems utxoRefs
