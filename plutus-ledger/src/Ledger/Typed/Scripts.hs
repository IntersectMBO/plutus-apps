{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Ledger.Typed.Scripts
  ( module Export
  , TypedScriptTxIn (tyTxInTxIn, tyTxInOutRef)
  , makeTypedScriptTxIn
  ) where

import Ledger.Tx.Internal (TxIn (TxIn), TxInType (ConsumeScriptAddress))
import Ledger.Typed.Scripts.Orphans as Export ()
import Plutus.Script.Utils.Typed as Export
import Plutus.Script.Utils.V1.Typed.Scripts as Export
import Plutus.V1.Ledger.Api (Datum (Datum), Redeemer (Redeemer), ToData (..))

-- | A 'TxIn' tagged by two phantom types: a list of the types of the data scripts in the transaction; and the connection type of the input.
data TypedScriptTxIn a = TypedScriptTxIn
  { tyTxInTxIn   :: TxIn,
    tyTxInOutRef :: TypedScriptTxOutRef a
  }

instance Eq (DatumType a) => Eq (TypedScriptTxIn a) where
  l == r =
    tyTxInTxIn l == tyTxInTxIn r
      && tyTxInOutRef l == tyTxInOutRef r

-- | Create a 'TypedScriptTxIn' from a correctly-typed validator, redeemer, and output ref.
makeTypedScriptTxIn ::
  forall inn.
  (ToData (RedeemerType inn), ToData (DatumType inn)) =>
  TypedValidator inn ->
  RedeemerType inn ->
  TypedScriptTxOutRef inn ->
  TypedScriptTxIn inn
makeTypedScriptTxIn si r tyRef =
  let d = Export.tyTxOutData (Export.tyTxOutRefOut tyRef)
      vs = validatorScript si
      rs = Redeemer (toBuiltinData r)
      ds = Datum (toBuiltinData d)
      txInT = ConsumeScriptAddress (Export.tvLanguage si) vs rs ds
   in TypedScriptTxIn @inn (TxIn (Export.tyTxOutRefRef tyRef) (Just txInT)) tyRef
