module Ledger.Generators.Internal where

import Data.Default (def)
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger.Params (Params)
import Plutus.V1.Ledger.Tx (Tx, TxOut (..), TxOutRef)

-- | Blockchain for testing the emulator implementation and traces.
--
--   To avoid having to rely on functions from the implementation of
--   plutus-ledger (in particular, 'Ledger.Tx.unspentOutputs') we note the
--   unspent outputs of the chain when it is first created.
data Mockchain = Mockchain {
    mockchainInitialTxPool :: [Tx],
    mockchainUtxo          :: Map TxOutRef TxOut,
    mockchainParams        :: Params
    } deriving Show

-- | The empty mockchain.
emptyChain :: Mockchain
emptyChain = Mockchain [] Map.empty def
