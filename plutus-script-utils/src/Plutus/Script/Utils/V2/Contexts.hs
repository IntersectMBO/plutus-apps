{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
module Plutus.Script.Utils.V2.Contexts
    ( findTxInByTxOutRef
    , findTxRefInByTxOutRef
    ) where

import Plutus.V2.Ledger.Api (TxOutRef)
import Plutus.V2.Ledger.Api qualified as PV2
import PlutusTx.Prelude (Maybe, find, (==))

{-# INLINABLE findTxInByTxOutRef #-}
findTxInByTxOutRef :: TxOutRef -> PV2.TxInfo -> Maybe PV2.TxInInfo
findTxInByTxOutRef outRef PV2.TxInfo{PV2.txInfoInputs} =
    find (\PV2.TxInInfo{PV2.txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoInputs

{-# INLINABLE findTxRefInByTxOutRef #-}
findTxRefInByTxOutRef :: TxOutRef -> PV2.TxInfo -> Maybe PV2.TxInInfo
findTxRefInByTxOutRef outRef PV2.TxInfo{PV2.txInfoReferenceInputs} =
    find (\PV2.TxInInfo{PV2.txInInfoOutRef} -> txInInfoOutRef == outRef) txInfoReferenceInputs
