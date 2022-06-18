{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-|
    Defines helper functions for `Tx`. Reexported from Ledger.Tx.
-}
module Ledger.Tx.Internal
    ( lookupSignature
    , lookupMintingScripts
    , lookupScript
    , lookupValidator
    , lookupMintingPolicy
    , lookupStakeValidator
    , fillTxInputWitnesses
    , pubKeyTxInput
    , addMintingPolicy
    , addScriptTxInput
    , validValuesTx
    , spentOutputs
    , updateUtxoCollateral
    ) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Ledger.Crypto (PubKey, Signature)
import Ledger.Orphans ()
import Ledger.Tx.Types.Tx as Export (Tx (..))
import Ledger.Tx.Types.TxInput as Export (TxInput (TxInput, txInputRef), TxInputType (..))
import Ledger.Value qualified as V
import Plutus.Script.Utils.V1.Scripts (StakeValidator, StakeValidatorHash, datumHash, mintingPolicyHash, validatorHash)
import Plutus.V1.Ledger.Api (Datum, Redeemer)
import Plutus.V1.Ledger.Scripts (MintingPolicy (MintingPolicy), MintingPolicyHash (MintingPolicyHash), Script,
                                 ScriptHash (ScriptHash), StakeValidator (StakeValidator),
                                 StakeValidatorHash (StakeValidatorHash), Validator (Validator),
                                 ValidatorHash (ValidatorHash))
import Plutus.V1.Ledger.Tx (TxIn (TxIn), TxInType (..), TxOut (txOutValue), TxOutRef)

-- | The transaction output references consumed by a transaction.
spentOutputs :: Tx -> [TxOutRef]
spentOutputs = map txInputRef . txInputs

-- | Update a map of unspent transaction outputs and signatures
--   for a failed transaction using its collateral inputs.
updateUtxoCollateral :: Tx -> Map TxOutRef TxOut -> Map TxOutRef TxOut
updateUtxoCollateral tx unspent = unspent `Map.withoutKeys` (Set.fromList . map txInputRef . txCollateral $ tx)

-- | Check that all values in a transaction are non-negative.
validValuesTx :: Tx -> Bool
validValuesTx Tx{..}
  = all (nonNegative . txOutValue) txOutputs  && nonNegative txFee
    where
      nonNegative i = V.geq i mempty

lookupSignature :: PubKey -> Tx -> Maybe Signature
lookupSignature s Tx{txSignatures} = Map.lookup s txSignatures

-- | Get MintingPolicy scripts for MintingPolicyHash'es included in the transaction,
-- Nothing means the transaction misses given script witness.
lookupMintingScripts :: Tx -> [Maybe MintingPolicy]
lookupMintingScripts Tx{txMintingScripts, txScripts} =
    map (\mph -> MintingPolicy <$> Map.lookup (toScriptHash mph) txScripts ) (Map.keys txMintingScripts)
    where
        toScriptHash (MintingPolicyHash b) = ScriptHash b

lookupScript :: Map ScriptHash Script -> ScriptHash -> Maybe Script
lookupScript txScripts hash  = Map.lookup hash txScripts

lookupValidator :: Map ScriptHash Script -> ValidatorHash -> Maybe Validator
lookupValidator txScripts = fmap Validator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (ValidatorHash b) = ScriptHash b

lookupMintingPolicy :: Map ScriptHash Script -> MintingPolicyHash -> Maybe MintingPolicy
lookupMintingPolicy txScripts = fmap MintingPolicy . lookupScript txScripts . toScriptHash
    where
        toScriptHash (MintingPolicyHash b) = ScriptHash b

lookupStakeValidator :: Map ScriptHash Script -> StakeValidatorHash -> Maybe StakeValidator
lookupStakeValidator txScripts = fmap StakeValidator . lookupScript txScripts . toScriptHash
    where
        toScriptHash (StakeValidatorHash b) = ScriptHash b

-- | Translate TxInput to TxIn taking script and datum witnesses from Tx.
fillTxInputWitnesses :: Tx -> TxInput -> TxIn
fillTxInputWitnesses tx (TxInput outRef _inType) = case _inType of
    TxConsumePublicKeyAddress -> TxIn outRef (Just ConsumePublicKeyAddress)
    TxConsumeSimpleScriptAddress -> TxIn outRef (Just ConsumeSimpleScriptAddress)
    TxConsumeScriptAddress redeemer vlh dh -> TxIn outRef $ do
        datum <- Map.lookup dh (txData tx)
        validator <- lookupValidator (txScripts tx) vlh
        Just $ ConsumeScriptAddress validator redeemer datum

pubKeyTxInput :: TxOutRef -> TxInput
pubKeyTxInput outRef = TxInput outRef TxConsumePublicKeyAddress

-- | Add minting policy together with the redeemer into txMintingScripts and txScripts accordingly.
addMintingPolicy :: MintingPolicy -> Redeemer -> Tx -> Tx
addMintingPolicy vl@(MintingPolicy script) rd tx@Tx{txMintingScripts, txScripts} = tx
    {txMintingScripts = Map.insert mph rd txMintingScripts,
     txScripts = Map.insert (ScriptHash b) script txScripts}
    where
        mph@(MintingPolicyHash b) = mintingPolicyHash vl

-- | Add minting policy together with the redeemer into txMintingScripts and txScripts accordingly.
addScriptTxInput :: TxOutRef -> Validator -> Redeemer -> Datum -> Tx -> Tx
addScriptTxInput outRef vl@(Validator script) rd dt tx@Tx{txInputs, txScripts, txData} = tx
    {txInputs = TxInput outRef (TxConsumeScriptAddress rd vlHash dtHash) : txInputs,
     txScripts = Map.insert (ScriptHash b) script txScripts,
     txData = Map.insert dtHash dt txData}
    where
        dtHash = datumHash dt
        vlHash@(ValidatorHash b) = validatorHash vl
