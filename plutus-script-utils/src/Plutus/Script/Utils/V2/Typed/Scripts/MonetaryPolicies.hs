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
    ( mkForwardingMintingPolicy
    , forwardToValidator
    ) where

import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)
import Plutus.V2.Ledger.Api (Address (Address, addressCredential), Credential (ScriptCredential), MintingPolicy,
                             ValidatorHash, mkMintingPolicyScript)
import Plutus.V2.Ledger.Contexts (ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
                                  ScriptPurpose (Minting), TxInfo (TxInfo, txInfoInputs))
import Plutus.V2.Ledger.Contexts qualified as PV2
import Plutus.V2.Ledger.Tx (TxOut (TxOut, txOutAddress))
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False), any, ($), (.), (==))

-- TODO: we should add a TypedMintingPolicy interface here

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

