{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Cardano.PlutusExample.Witness
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

keyHash :: PubKeyHash
keyHash = "cf3fd78fe693b5a67d7194332445a349edd74bd36e26a561572fd3fa"

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> BuiltinData -> ScriptContext -> Bool
mkPolicy pkh _ ctx = traceIfFalse "not signed by pubkeyhash" checkWitness
  where
    info :: TxInfo
    info  = scriptContextTxInfo ctx

    checkWitness :: Bool
    checkWitness  = txSignedBy info pkh

policy :: PubKeyHash -> Scripts.MintingPolicy
policy h = Ledger.mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
     `PlutusTx.applyCode`
      PlutusTx.liftCode h

script :: Plutus.Script
script = Plutus.unMintingPolicyScript (policy keyHash)

witnessScriptShortBs :: SBS.ShortByteString
witnessScriptShortBs = SBS.toShort . LBS.toStrict $ serialise script

witnessScript :: PlutusScript PlutusScriptV1
witnessScript = PlutusScriptSerialised witnessScriptShortBs
