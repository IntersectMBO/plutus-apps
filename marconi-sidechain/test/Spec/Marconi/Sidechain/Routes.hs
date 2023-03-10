{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Marconi.Sidechain.Routes (tests) where

import Cardano.Api qualified as C
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (Proxy))
import Gen.Marconi.ChainIndex.Types qualified as CGen
import Hedgehog (Property, forAll, property, tripping)
import Marconi.ChainIndex.Indexers.MintBurn (TxMintRow (TxMintRow))
import Marconi.ChainIndex.Indexers.Utxo (Utxo (Utxo), UtxoRow (UtxoRow))
import Marconi.Sidechain.Api.Routes (AddressUtxoResult (AddressUtxoResult),
                                     CurrentSyncedPointResult (CurrentSyncedPointResult),
                                     MintingPolicyHashTxResult (MintingPolicyHashTxResult))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Golden (goldenVsStringDiff)
import Test.Tasty.Hedgehog (testPropertyNamed)

tests :: TestTree
tests = testGroup "Spec.Marconi.Sidechain.Routes"
    [ testGroup "ToJSON/FromJSON rountrip"
        [ testPropertyNamed
            "CurrentSyncedPointResult"
            "propJSONRountripCurrentSyncedPointResult"
            propJSONRountripCurrentSyncedPointResult
        ]
    , testGroup "Golden test for query results"
        [ goldenVsStringDiff
            "Golden test for CurrentSyncedPointResult in JSON format when chain point is at genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/current-synced-point-response-1.json"
            goldenCurrentChainPointGenesisResult
        , goldenVsStringDiff
            "Golden test for CurrentSyncedPointResult in JSON format when chain point is at point other than genesis"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/current-synced-point-response-2.json"
            goldenCurrentChainPointResult
        , goldenVsStringDiff
            "Golden test for AddressUtxoResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/address-utxo-response.json"
            goldenAddressUtxoResult
        , goldenVsStringDiff
            "Golden test for MintingPolicyHashTxResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/mintingpolicyhash-tx-response.json"
            goldenMintingPolicyHashTxResult
        , goldenVsStringDiff
            "Golden test for EpochStakePoolDelegationResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/epoch-stakepooldelegation-response.json"
            goldenEpochStakePoolDelegationResult
        , goldenVsStringDiff
            "Golden test for EpochNonResult in JSON format"
            (\expected actual -> ["diff", "--color=always", expected, actual])
            "test/Spec/Marconi/Sidechain/Api/Routes/Golden/epoch-nonce-response.json"
            goldenEpochNonceResult
        ]
    ]

propJSONRountripCurrentSyncedPointResult :: Property
propJSONRountripCurrentSyncedPointResult = property $ do
    cp <- CurrentSyncedPointResult <$> forAll CGen.genChainPoint
    tripping cp Aeson.encode Aeson.decode

goldenCurrentChainPointGenesisResult :: IO ByteString
goldenCurrentChainPointGenesisResult = do
    pure $ Aeson.encodePretty $ CurrentSyncedPointResult C.ChainPointAtGenesis

goldenCurrentChainPointResult :: IO ByteString
goldenCurrentChainPointResult = do
    let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
    blockHeaderHash <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

    pure $ Aeson.encodePretty $ CurrentSyncedPointResult $ C.ChainPoint (C.SlotNo 1) blockHeaderHash

goldenAddressUtxoResult :: IO ByteString
goldenAddressUtxoResult = do
    let addressBech32 = "addr_test1vpfwv0ezc5g8a4mkku8hhy3y3vp92t7s3ul8g778g5yegsgalc6gc"
    addr <-
        either
            (error . show)
            pure
            $ C.deserialiseFromBech32 (C.AsAddress C.AsShelleyAddr) addressBech32

    let datumCbor = "4"
    datum <-
        either
            (error . show)
            pure
            $ C.deserialiseFromCBOR C.AsScriptData datumCbor

    let scriptCbor = "484701000022220011"
    plutusScript <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsPlutusScript C.AsPlutusScriptV1) scriptCbor
    let script = C.PlutusScript C.PlutusScriptV1 plutusScript

    let txIdRawBytes = "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000"
    txId <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex C.AsTxId txIdRawBytes

    let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
    blockHeaderHash <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

    let utxos =
            [ Utxo
                (C.AddressShelley addr)
                txId
                (C.TxIx 0)
                Nothing
                Nothing
                (C.lovelaceToValue $ C.Lovelace 10_000_000)
                Nothing
                Nothing
            , Utxo
                (C.AddressShelley addr)
                txId
                (C.TxIx 0)
                (Just datum)
                (Just $ C.hashScriptData datum)
                (C.lovelaceToValue $ C.Lovelace 10_000_000)
                (Just $ C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1) script)
                (Just $ C.hashScript script)
            ]
        result = AddressUtxoResult $ fmap (\utxo -> UtxoRow utxo (C.SlotNo 1) blockHeaderHash) utxos
    pure $ Aeson.encodePretty result

goldenMintingPolicyHashTxResult :: IO ByteString
goldenMintingPolicyHashTxResult = do
    let redeemerCbor = "4"
    redeemerData <-
        either
            (error . show)
            pure
            $ C.deserialiseFromCBOR C.AsScriptData redeemerCbor

    let scriptCbor = "484701000022220011"
    plutusScript <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsPlutusScript C.AsPlutusScriptV1) scriptCbor
    let script = C.PlutusScript C.PlutusScriptV1 plutusScript
        policyId = C.scriptPolicyId script

    let txIdRawBytes = "ec7d3bd7c6a3a31368093b077af0db46ceac77956999eb842373e08c6420f000"
    txId <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex C.AsTxId txIdRawBytes

    let blockHeaderHashRawBytes = "6161616161616161616161616161616161616161616161616161616161616161"
    blockHeaderHash <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

    let mints =
            [ TxMintRow
                (C.SlotNo 1)
                blockHeaderHash
                txId
                policyId
                "myassetname"
                (C.Quantity $ -10)
                0
                redeemerData
            , TxMintRow
                (C.SlotNo 1)
                blockHeaderHash
                txId
                policyId
                "myassetname"
                (C.Quantity 10)
                0
                redeemerData
            ]
        result = MintingPolicyHashTxResult mints
    pure $ Aeson.encodePretty result

goldenEpochStakePoolDelegationResult :: IO ByteString
goldenEpochStakePoolDelegationResult = pure ""

goldenEpochNonceResult :: IO ByteString
goldenEpochNonceResult = pure ""
