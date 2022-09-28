{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Plutus.Script.Utils.V2.Typed.Scripts
  ( module Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies
  , module Plutus.Script.Utils.V2.Typed.Scripts.StakeValidators
  , module Plutus.Script.Utils.V2.Typed.Scripts.Validators
  , Validator
  , MintingPolicy
  , StakeValidator
  , TypedScriptTxOut (..)
  , TypedScriptTxOutRef (..)
  , typeScriptTxOut
  , typeScriptTxOutRef
  , ConnectionError (..)
  )
where

import Control.Monad.Except (MonadError (throwError))
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (ConnectionError (..))
import Plutus.Script.Utils.V1.Typed.Scripts.Validators qualified as V1
import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies hiding (forwardToValidator)
import Plutus.Script.Utils.V2.Typed.Scripts.StakeValidators hiding (forwardToValidator)
import Plutus.Script.Utils.V2.Typed.Scripts.Validators
import Plutus.V2.Ledger.Api (Credential (PubKeyCredential, ScriptCredential), Datum, FromData, MintingPolicy,
                             OutputDatum (OutputDatum, OutputDatumHash), StakeValidator, ToData (..),
                             TxOut (txOutAddress, txOutDatum), TxOutRef, Validator, addressCredential)

--
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
      throwError $ V1.WrongOutType V1.ExpectedScriptGotPubkey
    ScriptCredential _vh ->
      case txOutDatum txOut of
        OutputDatum d | datumHash datum == datumHash d -> do
          V1.checkValidatorAddress tv (txOutAddress txOut)
          dsVal <- V1.checkDatum tv datum
          pure $ TypedScriptTxOut @out txOut dsVal
        OutputDatumHash dh | datumHash datum == dh -> do
          V1.checkValidatorAddress tv (txOutAddress txOut)
          dsVal <- V1.checkDatum tv datum
          pure $ TypedScriptTxOut @out txOut dsVal
        _ -> throwError $ V1.NoDatum txOutRef (datumHash datum)


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
