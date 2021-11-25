{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE NamedFieldPuns   #-}

module Util where

import Control.Lens (view)
import Control.Monad (forM)
import Control.Monad.Freer (Eff, Member)
import Ledger (Address (Address, addressCredential), TxOut (TxOut, txOutAddress), TxOutRef)
import Ledger.Credential (Credential)
import Plutus.ChainIndex (Page (pageItems), PageQuery (PageQuery), citxOutputs, utxoSetAtAddress)
import Plutus.ChainIndex.Api (UtxosResponse (UtxosResponse))
import Plutus.ChainIndex.Effects (ChainIndexQueryEffect)
import Plutus.ChainIndex.Tx (ChainIndexTx, _ValidTx)

-- | Get all address credentials from a block.
addrCredsFromBlock :: [ChainIndexTx] -> [Credential]
addrCredsFromBlock =
  fmap (\TxOut { txOutAddress = Address { addressCredential }} -> addressCredential)
  . view (traverse . citxOutputs . _ValidTx)

-- | Get the UTxO set from a block.
utxoSetFromBlockAddrs :: Member ChainIndexQueryEffect effs => [ChainIndexTx] -> Eff effs [[TxOutRef]]
utxoSetFromBlockAddrs block = forM (addrCredsFromBlock block) $ \addr -> do
  let pq = PageQuery 200 Nothing
  UtxosResponse _ utxoRefs <- utxoSetAtAddress pq addr
  pure $ pageItems utxoRefs
