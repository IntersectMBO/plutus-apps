{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V1.Typed.Scripts
  ( module Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies,
    module Plutus.Script.Utils.V1.Typed.Scripts.StakeValidators,
    module Plutus.Script.Utils.V1.Typed.Scripts.Validators,
    Validator,
    MintingPolicy,
    StakeValidator,
    TypedScriptTxOut (tyTxOutData, tyTxOutTxOut),
    TypedScriptTxOutRef (tyTxOutRefOut, tyTxOutRefRef),
    makeTypedScriptTxOut,
    typeScriptTxOut,
    typeScriptTxOutRef,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.Script.Utils.V1.Typed.Scripts.MonetaryPolicies hiding (forwardToValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.StakeValidators hiding (forwardToValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators
import Plutus.V1.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum (Datum), FromData, MintingPolicy,
                             StakeValidator, ToData (..), TxOut (TxOut, txOutAddress, txOutDatumHash, txOutValue),
                             TxOutRef, Validator, Value, addressCredential)

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

-- | A 'TxOut' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOut a = (FromData (DatumType a), ToData (DatumType a)) =>
  TypedScriptTxOut
  { tyTxOutTxOut :: TxOut,
    tyTxOutData  :: DatumType a
  }

instance Eq (DatumType a) => Eq (TypedScriptTxOut a) where
  l == r =
    tyTxOutTxOut l == tyTxOutTxOut r
      && tyTxOutData l == tyTxOutData r

-- | Create a 'TypedScriptTxOut' from a correctly-typed data script, an address, and a value.
makeTypedScriptTxOut ::
  forall out.
  (ToData (DatumType out), FromData (DatumType out)) =>
  TypedValidator out ->
  DatumType out ->
  Value ->
  TypedScriptTxOut out
makeTypedScriptTxOut ct d value =
  TypedScriptTxOut @out
    TxOut
      { txOutAddress = validatorAddress ct,
        txOutValue = value,
        txOutDatumHash = Just (datumHash $ Datum $ toBuiltinData d)
      }
    d

-- | A 'TxOutRef' tagged by a phantom type: and the connection type of the output.
data TypedScriptTxOutRef a = TypedScriptTxOutRef
  { tyTxOutRefRef :: TxOutRef,
    tyTxOutRefOut :: TypedScriptTxOut a
  }

instance Eq (DatumType a) => Eq (TypedScriptTxOutRef a) where
  l == r =
    tyTxOutRefRef l == tyTxOutRefRef r
      && tyTxOutRefOut l == tyTxOutRefOut r

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOut ::
  forall out m.
  ( FromData (DatumType out),
    ToData (DatumType out),
    MonadError ConnectionError m
  ) =>
  TypedValidator out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOut out)
typeScriptTxOut tv txOutRef txOut datum = do
  case addressCredential (txOutAddress txOut) of
    PubKeyCredential _ ->
      throwError $ WrongOutType ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case txOutDatumHash txOut of
        Just dh | datumHash datum == dh -> do
          checkValidatorAddress tv (txOutAddress txOut)
          dsVal <- checkDatum tv datum
          pure $ TypedScriptTxOut @out txOut dsVal
        _ -> throwError $ NoDatum txOutRef (datumHash datum)

-- | Create a 'TypedScriptTxOut' from an existing 'TxOut' by checking the types of its parts.
typeScriptTxOutRef ::
  forall out m.
  ( FromData (DatumType out),
    ToData (DatumType out),
    MonadError ConnectionError m
  ) =>
  TypedValidator out ->
  TxOutRef ->
  TxOut ->
  Datum ->
  m (TypedScriptTxOutRef out)
typeScriptTxOutRef tv txOutRef txOut datum = do
  tyOut <- typeScriptTxOut tv txOutRef txOut datum
  pure $ TypedScriptTxOutRef txOutRef tyOut
