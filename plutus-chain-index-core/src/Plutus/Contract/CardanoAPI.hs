{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-|

Interface to the transaction types from 'cardano-api'

-}
module Plutus.Contract.CardanoAPI(
    fromCardanoBlock
  , fromCardanoBlock'
  , fromCardanoTx
  , setValidity
  , fromCardanoTxOut
  , fromCardanoTxOutRefScript
  , toChainIndexTxEmptyScripts
  , toChainIndexInternalTx
  , toChainIndexTx
  , toNonAdaAssetClassList
  , module Export
) where

import Cardano.Api qualified as C
import Cardano.Api.Byron qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Ledger.Alonzo.Language qualified as Alonzo
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Core qualified as Ledger
import Codec.Serialise (deserialiseOrFail)
import Codec.Serialise qualified as Codec
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BSL
import Data.ByteString.Short qualified as SBS
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Ledger qualified as P
import Ledger.Value qualified as P
import Ledger.Ada qualified as Ada
import Ledger.Tx.CardanoAPI as Export
import Plutus.ChainIndex.Types (ChainIndexInternalTx (..), ChainIndexTx (..), ChainIndexTxOut (..), ChainIndexTxOutputs (..), ReferenceScript (..))
import PlutusTx.AssocMap qualified as PMap
import PlutusTx.Prelude qualified as PlutusTx


-- | returns a list of non ada asset classes contained in value
toNonAdaAssetClassList :: P.Value -> [P.AssetClass]
toNonAdaAssetClassList v = goOuter (PMap.toList $ P.getValue v) []
  where
    goOuter [] acc = acc
    goOuter ((cs, m) : tl) acc = goOuter tl (goInner cs acc (PMap.toList m))

    goInner _ acc [] = acc
    goInner cs acc ((tn, a) : tl)
     | a == 0 || ( cs == Ada.adaSymbol && tn == Ada.adaToken ) = goInner cs acc tl
     | otherwise = goInner cs (P.AssetClass (cs, tn) : acc) tl


fromCardanoBlock :: C.BlockInMode C.CardanoMode -> [ChainIndexTx]
fromCardanoBlock (C.BlockInMode (C.Block C.BlockHeader {} txs) eraInMode) =
  map (fromCardanoTx eraInMode) txs

-- | same as fromCardanoBlock but returns a ChainIndexInternalTx instance instead
fromCardanoBlock' :: C.BlockInMode C.CardanoMode -> [ChainIndexInternalTx]
fromCardanoBlock' (C.BlockInMode (C.Block C.BlockHeader {} txs) eraInMode) =
  map (fromCardanoTx' eraInMode) txs


-- | Converts a ChainIndexTx to a ChainIndexInternalTx
toChainIndexInternalTx :: ChainIndexTx -> ChainIndexInternalTx
toChainIndexInternalTx ChainIndexTx {..} =
  ChainIndexInternalTx
  { ciitxTxId = _citxTxId
  , ciitxValidRange = _citxValidRange
  , ciitxInputs = _citxInputs
  , ciitxOutputs = _citxOutputs
  , ciitxData = _citxData
  , ciitxRedeemers = _citxRedeemers
  , ciitxScripts = Map.fromList $ map (\(sh, P.Versioned s pvl) -> (sh, P.Versioned (BSL.toStrict $ Codec.serialise s) pvl))
                                $ Map.toList _citxScripts
  , ciitxCardanoTx = _citxCardanoTx
  }

-- | Converts a chain index internal tx to a chain index tx but set an empty script map
-- | For internal use only. Mainly in ChainIndex.Handlers.
toChainIndexTxEmptyScripts :: ChainIndexInternalTx -> ChainIndexTx
toChainIndexTxEmptyScripts ChainIndexInternalTx {..} =
  ChainIndexTx
  { _citxTxId = ciitxTxId
  , _citxValidRange = ciitxValidRange
  , _citxInputs = ciitxInputs
  , _citxOutputs = ciitxOutputs
  , _citxData = ciitxData
  , _citxRedeemers = ciitxRedeemers
  , _citxScripts =  Map.empty
  , _citxCardanoTx = ciitxCardanoTx
  }

-- | Converts a chain index internal tx to a chain index tx
toChainIndexTx :: ChainIndexInternalTx -> ChainIndexTx
toChainIndexTx ChainIndexInternalTx {..} =
  ChainIndexTx
  { _citxTxId = ciitxTxId
  , _citxValidRange = ciitxValidRange
  , _citxInputs = ciitxInputs
  , _citxOutputs = ciitxOutputs
  , _citxData = ciitxData
  , _citxRedeemers = ciitxRedeemers
  , _citxScripts =  Map.fromList $ mapMaybe fromByteString $ Map.toList ciitxScripts
  , _citxCardanoTx = ciitxCardanoTx
  }
  where
    fromByteString :: (P.ScriptHash, P.Versioned ByteString) -> Maybe(P.ScriptHash, P.Versioned P.Script)
    fromByteString (sh, P.Versioned bs pvl) =
      let script = fmap (\s -> (sh, P.Versioned s pvl))
                   $ deserialiseOrFail
                   $ BSL.fromStrict bs
      in either (const Nothing) Just script

-- | Convert a Cardano API tx of any given era to a Plutus chain index internal tx.
fromCardanoTx'
  :: C.IsCardanoEra era
  => C.EraInMode era C.CardanoMode
  -> C.Tx era
  -> ChainIndexInternalTx
fromCardanoTx' eraInMode tx@(C.Tx txBody@(C.TxBody C.TxBodyContent{..}) _) =
    let txOutputs = map fromCardanoTxOut txOuts
        scriptMap = plutusScriptsFromTxBody' txBody
        isTxScriptValid = fromTxScriptValidity txScriptValidity
        (datums, redeemers) = scriptDataFromCardanoTxBody txBody
        -- We need to sort the inputs as the order is important
        -- to find the corresponding redeemers
        inputs = sort $
          if isTxScriptValid
            then fst <$> txIns
            else case txInsCollateral of
                   C.TxInsCollateralNone     -> []
                   C.TxInsCollateral _ txins -> txins
        collateralOut = case txReturnCollateral of
          C.TxReturnCollateralNone     -> Nothing
          C.TxReturnCollateral _ txOut -> Just $ fromCardanoTxOut txOut

    in ChainIndexInternalTx
            { ciitxTxId = fromCardanoTxId (C.getTxId txBody)
            , ciitxValidRange = fromCardanoValidityRange txValidityRange
            -- If the transaction is invalid, we use collateral inputs
            , ciitxInputs = fmap ((`P.TxIn` Nothing) . fromCardanoTxIn) inputs
            -- No outputs if the one of scripts failed
            , ciitxOutputs = if isTxScriptValid then ValidTx txOutputs
                                                else InvalidTx collateralOut
            , ciitxData = datums
            , ciitxRedeemers = redeemers
            , ciitxScripts = scriptMap
            , ciitxCardanoTx = Just $ SomeTx tx eraInMode
            }

-- | Convert a Cardano API tx of any given era to a Plutus chain index tx.
fromCardanoTx
  :: C.IsCardanoEra era
  => C.EraInMode era C.CardanoMode
  -> C.Tx era
  -> ChainIndexTx
fromCardanoTx eraInMode tx = toChainIndexTx $ fromCardanoTx' eraInMode tx



fromCardanoTxOut :: C.TxOut C.CtxTx era -> ChainIndexTxOut
fromCardanoTxOut (C.TxOut addr val datum refScript) =
    ChainIndexTxOut
        (fromCardanoAddressInEra addr)
        (fromCardanoValue $ C.txOutValueToValue val)
        (fromCardanoTxOutDatum datum)
        (fromCardanoTxOutRefScript refScript)

setValidity :: Bool -> C.Tx era -> C.Tx era
setValidity validity (C.Tx (C.ShelleyTxBody shelleyBasedEra txBody scripts dat aux _) era) =
  C.Tx (C.ShelleyTxBody shelleyBasedEra txBody scripts dat aux (toTxScriptValidity shelleyBasedEra validity)) era
setValidity _ tx = tx -- @setValidity@ only applies in Alonzo era (and newer)

fromCardanoTxOutRefScript :: C.ReferenceScript era -> ReferenceScript
fromCardanoTxOutRefScript = \case
    C.ReferenceScriptNone      -> ReferenceScriptNone
    C.ReferenceScript _ script -> ReferenceScriptInAnyLang script



-- | Note that Plutus scripts are only supported in Alonzo era and onwards.
--   Keeps the script at ByteString level, i.e., not performing deserialisation.
plutusScriptsFromTxBody' :: C.TxBody era -> Map P.ScriptHash (P.Versioned ByteString)
plutusScriptsFromTxBody' C.ByronTxBody {} = mempty
plutusScriptsFromTxBody' (C.ShelleyTxBody shelleyBasedEra _ scripts _ _ _) =
  Map.fromList $ mapMaybe (fromLedgerScript' shelleyBasedEra) scripts



-- | Convert a script from a Cardano api in shelley based era to a Plutus script along with it's hash.
--  Keeps the script at ByteString level, i.e., not performing deserialisation.
-- Note that Plutus scripts are only supported in Alonzo era and onwards.
fromLedgerScript'
  :: C.ShelleyBasedEra era
  -> Ledger.Script (C.ShelleyLedgerEra era)
  -> Maybe (P.ScriptHash, P.Versioned ByteString)
fromLedgerScript' C.ShelleyBasedEraShelley _      = Nothing
fromLedgerScript' C.ShelleyBasedEraAllegra _      = Nothing
fromLedgerScript' C.ShelleyBasedEraMary _         = Nothing
fromLedgerScript' C.ShelleyBasedEraAlonzo script  = fromLedgerPlutusScript' script
fromLedgerScript' C.ShelleyBasedEraBabbage script = fromLedgerPlutusScript' script

-- | Convert a `cardano-ledger` Plutus script from the Alonzo era and onwards to
-- a 'Script' along with it's hash.
--  The script is still kept at ByteString level.
fromLedgerPlutusScript' :: Alonzo.Script a -> Maybe (P.ScriptHash, P.Versioned ByteString)
fromLedgerPlutusScript' Alonzo.TimelockScript {} = Nothing
fromLedgerPlutusScript' (Alonzo.PlutusScript Alonzo.PlutusV1 bs) =
  let csh = C.PlutusScript C.PlutusScriptV1 (C.PlutusScriptSerialised bs)
      sh = P.ScriptHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes $ C.hashScript csh
  in Just (sh, P.Versioned (BSL.fromStrict $ SBS.fromShort bs) P.PlutusV1)
fromLedgerPlutusScript' (Alonzo.PlutusScript Alonzo.PlutusV2 bs) =
  let csh = C.PlutusScript C.PlutusScriptV2 (C.PlutusScriptSerialised bs)
      sh = P.ScriptHash $ PlutusTx.toBuiltin $ C.serialiseToRawBytes $ C.hashScript csh
  in Just (sh, P.Versioned (BSL.fromStrict $ SBS.fromShort bs) P.PlutusV2)
