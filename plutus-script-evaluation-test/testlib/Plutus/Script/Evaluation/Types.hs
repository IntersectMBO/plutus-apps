{-# LANGUAGE StrictData       #-}
{-# LANGUAGE TypeApplications #-}

module Plutus.Script.Evaluation.Types
  ( ScriptEvent (..),
    Checkpoint (..),
    StreamerState (..),
    ScriptM,
    Block,
    encodeChainPoint,
    decodeChainPoint,
  )
where

import Cardano.Api qualified as Cardano
import Cardano.Binary (FromCBOR (fromCBOR), ToCBOR (toCBOR))
import Cardano.Ledger.Alonzo.TxInfo qualified as Alonzo
import Codec.Serialise qualified as CBOR
import Codec.Serialise.Decoding qualified as CBOR
import Codec.Serialise.Encoding qualified as CBOR
import Control.Monad.Trans.State (StateT)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (Proxy))
import Data.Word (Word64)

-- | A script evaluation that happens on-chain, which either succeeded or failed.
data ScriptEvent
  = ScriptEventSuccess [Alonzo.PlutusDebug]
  | ScriptEventFailure (NonEmpty Alonzo.PlutusDebug)

instance CBOR.Serialise ScriptEvent where
  encode = \case
    ScriptEventSuccess ds -> CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> toCBOR ds
    ScriptEventFailure ds -> CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> toCBOR ds

  decode = do
    CBOR.decodeListLenOf 2
    CBOR.decodeWord >>= \case
      0 -> ScriptEventSuccess <$> fromCBOR
      1 -> ScriptEventFailure <$> fromCBOR
      _ -> fail "unknown tag"

-- | A checkpoint from which the streamer can resume.
data Checkpoint = Checkpoint
  { cChainPoint  :: Cardano.ChainPoint,
    cLedgerState :: Cardano.LedgerState
  }

instance CBOR.Serialise Checkpoint where
  encode (Checkpoint _chainPoint _ledgerState) =
    error "Not implemented: need https://github.com/input-output-hk/cardano-node/pull/3993"
    -- mconcat
    --   [CBOR.encodeListLen 2, encodeChainPoint chainPoint, Cardano.encodeLedgerState ledgerState]
  decode =
    error "Not implemented: need https://github.com/input-output-hk/cardano-node/pull/3993"
    -- do
    --   CBOR.decodeListLenOf 2
    --   Checkpoint <$> decodeChainPoint <*> Cardano.decodeLedgerState

encodeChainPoint :: Cardano.ChainPoint -> CBOR.Encoding
encodeChainPoint p = CBOR.encode $ case p of
  Cardano.ChainPointAtGenesis  -> Nothing
  Cardano.ChainPoint slot hash -> Just (slot, Cardano.serialiseToRawBytes hash)

decodeChainPoint :: CBOR.Decoder s Cardano.ChainPoint
decodeChainPoint =
  CBOR.decode >>= \case
    Nothing -> pure Cardano.ChainPointAtGenesis
    Just (slot, hashRawBytes) ->
      maybe
        (fail "decodeChainPoint: Unable to decode block hash")
        (pure . Cardano.ChainPoint slot)
        ( Cardano.deserialiseFromRawBytes
            (Cardano.proxyToAsType (Proxy @(Cardano.Hash Cardano.BlockHeader)))
            hashRawBytes
        )

-- | State we maintain when consuming the stream of ledger state and events
data StreamerState = StreamerState
  { ssCount  :: Word64,
    ssEvents :: [ScriptEvent]
  }

type ScriptM = StateT StreamerState IO

type Block = Cardano.BlockInMode Cardano.CardanoMode
