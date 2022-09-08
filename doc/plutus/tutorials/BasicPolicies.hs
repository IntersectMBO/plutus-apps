{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module BasicPolicies where

import PlutusCore.Default qualified as PLC
import PlutusTx qualified
import PlutusTx.Lift qualified as PlutusTx
import PlutusTx.Prelude qualified as PlutusTx

import Ledger qualified
import Ledger.Typed.Scripts qualified as Typed
import Ledger.Value qualified as Value

tname :: Value.TokenName
tname = PlutusTx.error ()

key :: Ledger.PubKeyHash
key = PlutusTx.error ()

-- BLOCK1
oneAtATimePolicy :: () -> Ledger.ScriptContext -> PlutusTx.Bool
oneAtATimePolicy _ ctx =
    -- 'ownCurrencySymbol' lets us get our own hash (= currency symbol)
    -- from the context
    let ownSymbol = Ledger.ownCurrencySymbol ctx
        txinfo = Ledger.scriptContextTxInfo ctx
        minted = Ledger.txInfoMint txinfo
    -- Here we're looking at some specific token name, which we
    -- will assume we've got from elsewhere for now.
    in Value.valueOf minted ownSymbol tname PlutusTx.== 1

-- We can use 'compile' to turn a minting policy into a compiled Plutus Core program,
-- just as for validator scripts. We also provide a 'mkUntypedMintingPolicy' function
-- to handle the boilerplate.
oneAtATimeCompiled :: PlutusTx.CompiledCode (PlutusTx.BuiltinData -> PlutusTx.BuiltinData -> ())
oneAtATimeCompiled = $$(PlutusTx.compile [|| Typed.mkUntypedMintingPolicy oneAtATimePolicy ||])
-- BLOCK2
singleSignerPolicy :: Ledger.ScriptContext -> PlutusTx.Bool
singleSignerPolicy ctx = Ledger.txSignedBy (Ledger.scriptContextTxInfo ctx) key
-- BLOCK3
