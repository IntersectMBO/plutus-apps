{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Ledger.Typed.Scripts
  ( module Export
  , TypedScriptTxIn (tyTxInTxIn, tyTxInOutRef)
  , makeTypedScriptTxIn
  , txInValue
  , makePubKeyTxIn
  , typePubKeyTxIn
  , typeScriptTxIn
  ) where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (FromJSON (..), ToJSON (..))
import GHC.Generics (Generic)
import Ledger.Tx.Internal (LedgerPlutusVersion, TxIn (TxIn, txInRef, txInType),
                           TxInType (ConsumePublicKeyAddress, ConsumeScriptAddress))
import Ledger.Typed.Scripts.Orphans as Export ()
import Plutus.Script.Utils.V1.Typed.Scripts as Export
import Plutus.Script.Utils.V1.Typed.TypeUtils as Export
import Plutus.V1.Ledger.Api (Datum (Datum), FromData, Redeemer (Redeemer), ToData (..), TxOut (txOutValue), TxOutRef,
                             Value)

-- | A 'TxIn' tagged by two phantom types: a list of the types of the data scripts in the transaction; and the connection type of the input.
data TypedScriptTxIn a = TypedScriptTxIn
  { tyTxInTxIn   :: TxIn,
    tyTxInOutRef :: TypedScriptTxOutRef a
  }

instance Eq (DatumType a) => Eq (TypedScriptTxIn a) where
  l == r =
    tyTxInTxIn l == tyTxInTxIn r
      && tyTxInOutRef l == tyTxInOutRef r

-- instance (FromJSON (DatumType a), FromData (DatumType a), ToData (DatumType a)) => FromJSON (TypedScriptTxIn a) where
--   parseJSON (Data.Aeson.Object v) =
--     TypedScriptTxIn <$> v .: "tyTxInTxIn" <*> v .: "tyTxInOutRef"
--   parseJSON invalid = typeMismatch "Object" invalid

-- instance (ToJSON (DatumType a)) => ToJSON (TypedScriptTxIn a) where
--   toJSON TypedScriptTxIn {tyTxInTxIn, tyTxInOutRef} =
--     object ["tyTxInTxIn" .= tyTxInTxIn, "tyTxInOutRef" .= tyTxInOutRef]

-- | Create a 'TypedScriptTxIn' from a correctly-typed validator, redeemer, and output ref.
makeTypedScriptTxIn ::
  forall inn.
  (ToData (RedeemerType inn), ToData (DatumType inn)) =>
  LedgerPlutusVersion ->
  TypedValidator inn ->
  RedeemerType inn ->
  TypedScriptTxOutRef inn ->
  TypedScriptTxIn inn
makeTypedScriptTxIn lang si r tyRef =
  let d = Export.tyTxOutData (Export.tyTxOutRefOut tyRef)
      vs = validatorScript si
      rs = Redeemer (toBuiltinData r)
      ds = Datum (toBuiltinData d)
      txInT = ConsumeScriptAddress lang vs rs ds
   in TypedScriptTxIn @inn (TxIn (Export.tyTxOutRefRef tyRef) (Just txInT)) tyRef

txInValue :: TypedScriptTxIn a -> Value
txInValue = txOutValue . tyTxOutTxOut . tyTxOutRefOut . tyTxInOutRef

-- | A public-key 'TxIn'. We need this to be sure that it is not a script input.
newtype PubKeyTxIn = PubKeyTxIn {unPubKeyTxIn :: TxIn}
  deriving stock (Eq, Generic)
  deriving newtype (FromJSON, ToJSON)

-- | Create a 'PubKeyTxIn'.
makePubKeyTxIn :: TxOutRef -> PubKeyTxIn
makePubKeyTxIn ref = PubKeyTxIn . TxIn ref . Just $ ConsumePublicKeyAddress

-- | Create a 'PubKeyTxIn' from an existing 'TxIn' by checking that it has the right payment type.
typePubKeyTxIn ::
  forall m.
  (MonadError ConnectionError m) =>
  TxIn ->
  m PubKeyTxIn
typePubKeyTxIn txIn =
  case txInType txIn of
    Just ConsumePublicKeyAddress -> pure $ PubKeyTxIn txIn
    Just _                       -> throwError MissingInType -- $ WrongInType x -- TODO
    Nothing                      -> throwError MissingInType

-- | Create a 'TypedScriptTxIn' from an existing 'TxIn' by checking the types of its parts.
typeScriptTxIn ::
  forall inn m.
  ( FromData (RedeemerType inn),
    ToData (RedeemerType inn),
    FromData (DatumType inn),
    ToData (DatumType inn),
    MonadError ConnectionError m
  ) =>
  (TxOutRef -> Maybe (TxOut, Datum)) ->
  TypedValidator inn ->
  TxIn ->
  m (TypedScriptTxIn inn)
typeScriptTxIn lookupRef typedValidator txIn =
  case txInType txIn of
    Just (ConsumeScriptAddress lang _val re da) -> do
      rsVal <- checkRedeemer typedValidator re
      _ <- checkDatum typedValidator da
      let txOutRef = txInRef txIn
      case lookupRef txOutRef of
        Just (txOut, datum) -> do
          typedOut <- typeScriptTxOutRef @inn typedValidator txOutRef txOut datum
          pure $ makeTypedScriptTxIn lang typedValidator rsVal typedOut
        Nothing -> throwError UnknownRef
    Just _ -> throwError MissingInType -- $ WrongInType x -- TODO
    Nothing -> throwError MissingInType
