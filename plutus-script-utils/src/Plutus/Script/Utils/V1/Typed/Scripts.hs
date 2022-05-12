{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V1.Typed.Scripts
    ( module Export
    , Validator
    , MintingPolicy
    , StakeValidator
    ) where

import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies as Export hiding (forwardToValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.StakeValidators as Export hiding (forwardToValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators as Export
import Plutus.V1.Ledger.Scripts (MintingPolicy, StakeValidator, Validator)

{- Note [Scripts returning Bool]
It used to be that the signal for validation failure was a script being `error`. This is nice for
the validator, since you can determine whether the script evaluation is error-or-not without having
to look at what the result actually *is* if there is one.

However, from the script author's point of view, it would be nicer to return a Bool, since
otherwise you end up doing a lot of `if realCondition then () else error ()` which is rubbish.

So we changed the result type to be Bool. But now we have to answer the question of how the
validator knows what the result value is. All *sorts* of terms can be True or False in disguise.
The easiest way to tell is by reducing it to the previous problem: apply a function which does a
pattern match and returns error in the case of False and () otherwise. Then, as before, we just
check for error in the overall evaluation.
-}
