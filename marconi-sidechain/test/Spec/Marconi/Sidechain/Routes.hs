{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}

module Spec.Marconi.Sidechain.Routes (tests) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Monad (forM)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty qualified as Aeson
import Data.ByteString.Lazy (ByteString)
import Data.Proxy (Proxy (Proxy))
import Gen.Cardano.Api.Typed qualified as CGen
import Gen.Marconi.ChainIndex.Types qualified as CGen
import Gen.Marconi.ChainIndex.Types qualified as Gen
import Hedgehog (Property, forAll, property, tripping)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Marconi.ChainIndex.Indexers.EpochStakepoolSize (EpochSPDRow (EpochSPDRow))
import Marconi.ChainIndex.Indexers.MintBurn (TxMintRow (TxMintRow))
import Marconi.ChainIndex.Indexers.Utxo (Utxo (Utxo), UtxoRow (UtxoRow))
import Marconi.Sidechain.Api.Routes (AddressUtxoResult (AddressUtxoResult),
                                     CurrentSyncedPointResult (CurrentSyncedPointResult),
                                     EpochStakePoolDelegationResult (EpochStakePoolDelegationResult),
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
        , testPropertyNamed
            "EpochStakePoolDelegationResult"
            "propJSONRountripEpochStakePoolDelegationResult"
            propJSONRountripEpochStakePoolDelegationResult
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

propJSONRountripEpochStakePoolDelegationResult :: Property
propJSONRountripEpochStakePoolDelegationResult = property $ do
    spds <- fmap EpochStakePoolDelegationResult $ forAll $ Gen.list (Range.linear 1 10) $ do
        EpochSPDRow
            <$> Gen.genEpochNo
            <*> Gen.genPoolId
            <*> CGen.genLovelace
            <*> Gen.genSlotNo
            <*> Gen.genHashBlockHeader
            <*> Gen.genBlockNo
    tripping spds Aeson.encode Aeson.decode

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
goldenEpochStakePoolDelegationResult = do
    let blockHeaderHashRawBytes = "578f3cb70f4153e1622db792fea9005c80ff80f83df028210c7a914fb780a6f6"
    blockHeaderHash <-
        either
            (error . show)
            pure
            $ C.deserialiseFromRawBytesHex (C.AsHash (C.proxyToAsType $ Proxy @C.BlockHeader)) blockHeaderHashRawBytes

    let poolIdsBech32 =
            [ "pool1z22x50lqsrwent6en0llzzs9e577rx7n3mv9kfw7udwa2rf42fa"
            , "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6"
            , "pool174mw7e20768e8vj4fn8y6p536n8rkzswsapwtwn354dckpjqzr8"
            ]
    poolIds <- forM poolIdsBech32 $ \poolIdBech32 -> do
        either
            (error . show)
            pure
            $ C.deserialiseFromBech32 (C.AsHash (C.proxyToAsType $ Proxy @C.StakePoolKey)) poolIdBech32

    let lovelace = C.Lovelace 100000000000000
        slotNo = C.SlotNo 1382422
        epochNo = C.EpochNo 6
        blockNo = C.BlockNo 64903

    let spds = fmap (\poolId -> EpochSPDRow epochNo poolId lovelace slotNo blockHeaderHash blockNo) poolIds
        result = EpochStakePoolDelegationResult spds
    pure $ Aeson.encodePretty result

goldenEpochNonceResult :: IO ByteString
goldenEpochNonceResult = pure ""
