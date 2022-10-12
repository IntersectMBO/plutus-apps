{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies
    ( UntypedMintingPolicy
    , mkUntypedMintingPolicy
    , mkForwardingMintingPolicy
    , forwardToValidator
    ) where

import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies (UntypedMintingPolicy)
import Plutus.V2.Ledger.Api (Address (Address, addressCredential), Credential (ScriptCredential), MintingPolicy,
                             ValidatorHash, mkMintingPolicyScript)
import Plutus.V2.Ledger.Contexts (ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
                                  ScriptPurpose (Minting), TxInfo (TxInfo, txInfoInputs))
import Plutus.V2.Ledger.Contexts qualified as PV2
import Plutus.V2.Ledger.Tx (TxOut (TxOut, txOutAddress))
import PlutusTx (UnsafeFromData (unsafeFromBuiltinData))
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False), any, check, ($), (.), (==))

-- TODO: we should add a TypedMintingPolicy interface here

{-# INLINABLE mkUntypedMintingPolicy #-}
-- | Converts a custom redeemer from a minting policy function to an
-- untyped minting policy function. See Note [Scripts returning Bool].
--
-- Here's an example of how this function can be used:
--
-- @
--   import PlutusTx qualified
--   import Plutus.V2.Ledger.Scripts qualified as Plutus
--   import Plutus.Script.Utils.V2.Scripts (mkUntypedMintingPolicy)
--
--   newtype MyCustomRedeemer = MyCustomRedeemer Integer
--   PlutusTx.unstableMakeIsData ''MyCustomRedeemer
--
--   mkMintingPolicy :: MyCustomRedeemer -> ScriptContext -> Bool
--   mkMintingPolicy _ _ = True
--
--   validator :: Plutus.Validator
--   validator = Plutus.mkMintingPolicyScript
--       $$(PlutusTx.compile [|| wrap ||])
--    where
--       wrap = mkUntypedMintingPolicy mkMintingPolicy
-- @
mkUntypedMintingPolicy
    :: UnsafeFromData r
    => (r -> PV2.ScriptContext -> Bool)
    -> UntypedMintingPolicy
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedMintingPolicy f r p =
    check $ f (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)

-- | A minting policy that checks whether the validator script was run
--   in the minting transaction.
mkForwardingMintingPolicy :: ValidatorHash -> MintingPolicy
mkForwardingMintingPolicy vshsh =
    mkMintingPolicyScript
     $ $$(PlutusTx.compile [|| \(hsh :: ValidatorHash) ->
         mkUntypedMintingPolicy (forwardToValidator hsh)
         ||])
       `PlutusTx.applyCode` PlutusTx.liftCode vshsh

{-# INLINABLE forwardToValidator #-}
forwardToValidator :: ValidatorHash -> () -> PV2.ScriptContext -> Bool
forwardToValidator h _ ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose=Minting _} =
    let checkHash TxOut{txOutAddress=Address{addressCredential=ScriptCredential vh}} = vh == h
        checkHash _                                                                  = False
    in any (checkHash . PV2.txInInfoResolved) txInfoInputs
forwardToValidator _ _ _ = False

