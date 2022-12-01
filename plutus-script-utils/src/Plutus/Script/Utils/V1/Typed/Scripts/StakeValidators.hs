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
module Plutus.Script.Utils.V1.Typed.Scripts.StakeValidators
    ( mkForwardingStakeValidator
    , forwardToValidator
    ) where

import Plutus.Script.Utils.Typed (mkUntypedStakeValidator)
import Plutus.V1.Ledger.Api (Address (Address, addressCredential), Credential (ScriptCredential), StakeValidator,
                             ValidatorHash, mkStakeValidatorScript)
import Plutus.V1.Ledger.Contexts (ScriptContext (ScriptContext, scriptContextPurpose, scriptContextTxInfo),
                                  ScriptPurpose (Certifying, Rewarding), TxInfo (TxInfo, txInfoInputs))
import Plutus.V1.Ledger.Contexts qualified as PV1
import Plutus.V1.Ledger.Tx (TxOut (TxOut, txOutAddress))
import PlutusTx qualified
import PlutusTx.Prelude (Bool (False), any, ($), (.), (==))

-- TODO: we should add a TypedStakeValidator interface here

-- | A stake validator that checks whether the validator script was run
--   in the right transaction.
mkForwardingStakeValidator :: ValidatorHash -> StakeValidator
mkForwardingStakeValidator vshsh =
    mkStakeValidatorScript
    $ $$(PlutusTx.compile [|| \(hsh :: ValidatorHash) ->
        mkUntypedStakeValidator (forwardToValidator hsh)
        ||])
      `PlutusTx.applyCode` PlutusTx.liftCode vshsh

{-# INLINABLE forwardToValidator #-}
forwardToValidator :: ValidatorHash -> () -> ScriptContext -> Bool
forwardToValidator h _ ScriptContext{scriptContextTxInfo=TxInfo{txInfoInputs}, scriptContextPurpose} =
    let checkHash TxOut{txOutAddress=Address{addressCredential=ScriptCredential vh}} = vh == h
        checkHash _                                                                  = False
        result = any (checkHash . PV1.txInInfoResolved) txInfoInputs
    in case scriptContextPurpose of
        Rewarding _  -> result
        Certifying _ -> result
        _            -> False
