{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.PlutusExample.WitnessRedeemer
  ( witnessScript
  , witnessScriptShortBs
  ) where

import Prelude hiding (($))

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Ledger hiding (singleton)
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts qualified as Scripts

import Plutus.V1.Ledger.Scripts qualified as Plutus
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> ScriptContext -> Bool
mkPolicy pkh ctx = traceIfFalse "not signed by redeemer pubkeyhash" checkWitness
  where
    info :: TxInfo
    info  = scriptContextTxInfo ctx

    checkWitness :: Bool
    checkWitness  = txSignedBy info pkh

policy :: Scripts.MintingPolicy
policy = Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy mkPolicy ||])

script :: Plutus.Script
script = Plutus.unMintingPolicyScript policy

witnessScriptShortBs :: SBS.ShortByteString
witnessScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

witnessScript :: PlutusScript PlutusScriptV1
witnessScript = PlutusScriptSerialised witnessScriptShortBs
