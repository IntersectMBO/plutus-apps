{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Spec.Plutus.Contract.Wallet
    ( tests
    -- TODO: remove export, added to silence the warnings
    , jsonInvProp
    ) where

import Cardano.Api qualified as C
import Data.Aeson (decode, encode)
import Data.Functor.Identity (Identity)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog (MonadGen, Property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (TxOutRef (TxOutRef))
import Ledger.Scripts qualified as Script
import Ledger.Tx.CardanoAPI (fromCardanoPolicyId, fromCardanoTxId)
import Plutus.Contract.Wallet (ExportTx (ExportTx), ExportTxInput (ExportTxInput, etxiAssets, etxiId, etxiTxIx),
                               ExportTxRedeemer (MintingRedeemer, SpendingRedeemer))
import Plutus.V1.Ledger.Scripts (MintingPolicyHash)
import Test.Tasty (TestTree, testGroup)
-- import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
    testGroup
        "Plutus.Cardano.Wallet"
        -- TODO: Reenable once we update `cardano-node` with the following PR merged:
        -- https://github.com/input-output-hk/cardano-node/pull/3847
        []
        -- [ testProperty "ExportTx FromJSON and ToJSON inverse property" jsonInvProp
        -- ]

jsonInvProp :: Property
jsonInvProp = Hedgehog.property $ do
    exportTx <- Hedgehog.forAll exportTxGen
    Hedgehog.tripping exportTx encode decode

exportTxGen :: (Hedgehog.GenBase m ~ Identity, MonadFail m, MonadGen m) => m ExportTx
exportTxGen = do
    exportTxInputs <- Gen.list (Range.linear 0 5) exportTxInputGen
    ExportTx
        <$> Hedgehog.fromGenT (Gen.genTx C.BabbageEra)
        <*> pure exportTxInputs
        <*> exportTxRedeemersGen exportTxInputs

exportTxInputGen :: (Hedgehog.GenBase m ~ Identity, MonadFail m, MonadGen m) => m ExportTxInput
exportTxInputGen = do
    C.TxIn txId txIx <- Hedgehog.fromGenT Gen.genTxIn
    C.TxOut addressInEra txOutValue txOutDatum _ <- Hedgehog.fromGenT (Gen.genTxOutTxContext C.BabbageEra)
    let datumToScriptDataHash C.TxOutDatumNone         = Nothing
        datumToScriptDataHash (C.TxOutDatumHash _ h)   = Just h
        datumToScriptDataHash (C.TxOutDatumInTx _ d)   = Just $ C.hashScriptData d
        datumToScriptDataHash (C.TxOutDatumInline _ d) = Just $ C.hashScriptData d
    pure $ ExportTxInput
        txId
        txIx
        addressInEra
        (C.txOutValueToLovelace txOutValue)
        (datumToScriptDataHash txOutDatum)
        (currenciesFromTxOutValue txOutValue)

currenciesFromTxOutValue :: C.TxOutValue C.BabbageEra -> [(C.PolicyId, C.AssetName, C.Quantity)]
currenciesFromTxOutValue txOutValue =
    mapMaybe currencyFromValue $ C.valueToList $ C.txOutValueToValue txOutValue
  where
    currencyFromValue :: (C.AssetId, C.Quantity) -> Maybe (C.PolicyId, C.AssetName, C.Quantity)
    currencyFromValue (C.AdaAssetId, _)                   = Nothing
    currencyFromValue (C.AssetId policyId assetName, qty) = Just (policyId, assetName, qty)

exportTxRedeemersGen :: MonadGen m => [ExportTxInput] -> m [ExportTxRedeemer]
exportTxRedeemersGen [] = pure []
exportTxRedeemersGen inputs = do
    let spendingGenM = fmap exportTxSpendingRedeemerGen $ NonEmpty.nonEmpty $ fmap getTxOutRef inputs
        mintingGenM = fmap exportTxMintingRedeemerGen $ NonEmpty.nonEmpty $ concatMap getMintingPolicyHashes inputs
    case (spendingGenM, mintingGenM) of
      (Just spendingGen, Just mintingGen) ->
          Gen.list (Range.linear 0 (length inputs)) $ Gen.choice [mintingGen, spendingGen]
      (Just spendingGen, Nothing) ->
          Gen.list (Range.linear 0 (length inputs)) spendingGen
      (Nothing, Just mintingGen) ->
          Gen.list (Range.linear 0 (length inputs)) mintingGen
      (Nothing, Nothing) -> pure []
  where
    getTxOutRef :: ExportTxInput -> TxOutRef
    getTxOutRef ExportTxInput { etxiId, etxiTxIx = (C.TxIx txIx) } =
        TxOutRef (fromCardanoTxId etxiId) (toInteger txIx)

    getMintingPolicyHashes :: ExportTxInput -> [MintingPolicyHash]
    getMintingPolicyHashes ExportTxInput { etxiAssets } =
        fmap (\(policyId, _, _) -> fromCardanoPolicyId policyId) etxiAssets

exportTxSpendingRedeemerGen :: MonadGen m => NonEmpty TxOutRef -> m ExportTxRedeemer
exportTxSpendingRedeemerGen txOutRefs = do
    txOutRef <- Gen.element (NonEmpty.toList txOutRefs)
    pure $ SpendingRedeemer Script.unitRedeemer txOutRef

exportTxMintingRedeemerGen :: MonadGen m => NonEmpty MintingPolicyHash -> m ExportTxRedeemer
exportTxMintingRedeemerGen policyHashes = do
    policyHash <- Gen.element (NonEmpty.toList policyHashes)
    pure $ MintingRedeemer Script.unitRedeemer policyHash
