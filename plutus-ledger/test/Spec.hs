{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
module Main(main) where

import Cardano.Api qualified as C
import Cardano.Crypto.Hash qualified as Crypto
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.Aeson.Internal qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.List (sort)
import Gen.Cardano.Api.Typed qualified as Gen
import Hedgehog (Property, forAll, fromGenT, property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (Slot (Slot))
import Ledger.Interval qualified as Interval
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI (CardanoBuildTx (CardanoBuildTx), CardanoTx (CardanoTx))
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.CardanoAPISpec qualified
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value qualified as Value hiding (scale)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all tests" [
    testGroup "intervals" [
        testPropertyNamed "member" "intvlMember," intvlMember,
        testPropertyNamed "contains" "intvlContains" intvlContains
        ],
    testGroup "Etc." [
        testPropertyNamed "encodeByteString" "encodeByteStringTest," encodeByteStringTest,
        testPropertyNamed "encodeSerialise" "encodeSerialiseTest" encodeSerialiseTest
        ],
    testGroup "Value" ([
        testPropertyNamed "TokenName looks like escaped bytestring ToJSON/FromJSON" "tokenname_escaped_roundtrip" (jsonRoundTrip . pure $ ("\NUL0xc0ffee" :: Value.TokenName))
        ] ++ (let   vlJson :: BSL.ByteString
                    vlJson = "{\"getValue\":[[{\"unCurrencySymbol\":\"ab01ff\"},[[{\"unTokenName\":\"myToken\"},50]]]]}"
                    vlValue = Value.singleton "ab01ff" "myToken" 50
                in byteStringJson vlJson vlValue)
          ++ (let   vlJson :: BSL.ByteString
                    vlJson = "{\"getValue\":[[{\"unCurrencySymbol\":\"\"},[[{\"unTokenName\":\"\"},50]]]]}"
                    vlValue = Ada.lovelaceValueOf 50
                in byteStringJson vlJson vlValue)),
    testGroup "TxIn" [
        testPropertyNamed "Check that Ord instances of TxIn match" "txInOrdInstanceEquivalenceTest" txInOrdInstanceEquivalenceTest
    ],
    testGroup "CardanoTx" [
        testPropertyNamed "Value ToJSON/FromJSON" "genCardanoTx" (jsonRoundTrip genCardanoTx)
    ],
    testGroup "CardanoBuildTx" [
        testPropertyNamed "Value ToJSON/FromJSON" "genCardanoBuildTx" (jsonRoundTrip genCardanoBuildTx)
    ],
    Ledger.Tx.CardanoAPISpec.tests
    ]

intvlMember :: Property
intvlMember = property $ do
    (i1, i2) <- forAll $ (,) <$> Gen.integral (fromIntegral <$> Range.linearBounded @Int) <*> Gen.integral (fromIntegral <$> Range.linearBounded @Int)
    let (from, to) = (min i1 i2, max i1 i2)
        i          = Interval.interval (Slot from) (Slot to)
    Hedgehog.assert $ Interval.member (Slot from) i || Interval.isEmpty i
    Hedgehog.assert $ not (Interval.member (Slot (from-1)) i) || Interval.isEmpty i
    Hedgehog.assert $ Interval.member (Slot to) i || Interval.isEmpty i
    Hedgehog.assert $ not (Interval.member (Slot (to+1)) i) || Interval.isEmpty i

intvlContains :: Property
intvlContains = property $ do
    -- generate two intervals from a sorted list of ints
    -- the outer interval contains the inner interval
    ints <- forAll $ traverse (const $ Gen.integral (fromIntegral <$> Range.linearBounded @Int)) [(1::Integer)..4]
    let [i1, i2, i3, i4] = Slot <$> sort ints
        outer = Interval.interval i1 i4
        inner = Interval.interval i2 i3

    Hedgehog.assert $ Interval.contains outer inner

encodeByteStringTest :: Property
encodeByteStringTest = property $ do
    bs <- forAll $ Gen.bytes $ Range.linear 0 1000
    let enc    = JSON.String $ JSON.encodeByteString bs
        result = Aeson.iparse JSON.decodeByteString enc

    Hedgehog.assert $ result == Aeson.ISuccess bs

encodeSerialiseTest :: Property
encodeSerialiseTest = property $ do
    txt <- forAll $ Gen.text (Range.linear 0 1000) Gen.unicode
    let enc    = JSON.String $ JSON.encodeSerialise txt
        result = Aeson.iparse JSON.decodeSerialise enc

    Hedgehog.assert $ result == Aeson.ISuccess txt

jsonRoundTrip :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => Hedgehog.Gen a -> Property
jsonRoundTrip gen = property $ do
    bts <- forAll gen
    let enc    = JSON.toJSON bts
        result = Aeson.iparse JSON.parseJSON enc

    Hedgehog.annotateShow (result, bts)
    Hedgehog.assert $ result == Aeson.ISuccess bts

byteStringJson :: (Show a, Eq a, JSON.ToJSON a, JSON.FromJSON a) => BSL.ByteString -> a -> [TestTree]
byteStringJson jsonString value =
    [ testCase "decoding" $
        HUnit.assertEqual "Simple Decode" (Right value) (JSON.eitherDecode jsonString)
    , testCase "encoding" $ HUnit.assertEqual "Simple Encode" jsonString (JSON.encode value)
    ]

-- | Check that Ord instances of cardano-api's 'TxIn' and plutus-ledger-api's 'TxIn' match.
txInOrdInstanceEquivalenceTest :: Property
txInOrdInstanceEquivalenceTest = property $ do
    txIns <- sort <$> forAll (Gen.list (Range.singleton 10) genTxIn)
    let toPlutus = map ((`Tx.TxIn` Nothing) . CardanoAPI.fromCardanoTxIn)
    let plutusTxIns = sort $ toPlutus txIns
    Hedgehog.assert $ toPlutus txIns == plutusTxIns

genTxIn :: Hedgehog.MonadGen m => m C.TxIn
genTxIn = do
    txId <- (\t -> C.TxId $ Crypto.castHash $ Crypto.hashWith (const t) ()) <$> Gen.utf8 (Range.singleton 5) Gen.unicode
    txIx <- C.TxIx <$> Gen.integral (Range.linear 0 maxBound)
    return $ C.TxIn txId txIx

genCardanoBuildTx :: Hedgehog.Gen CardanoBuildTx
genCardanoBuildTx = do
    tx <- Gen.genTxBodyContent C.BabbageEra
    let tx' = tx {
        C.txCertificates = C.TxCertificatesNone,
        C.txUpdateProposal = C.TxUpdateProposalNone,
        C.txAuxScripts = onlyPlutusScripts $ C.txAuxScripts tx
    }
    pure $ CardanoBuildTx tx'
    where
        onlyPlutusScripts C.TxAuxScriptsNone         = C.TxAuxScriptsNone
        onlyPlutusScripts (C.TxAuxScripts p scripts) = C.TxAuxScripts p $ filter isPlutusScript scripts
        isPlutusScript (C.ScriptInEra _ C.PlutusScript{}) = True
        isPlutusScript _                                  = False

-- TODO Unfortunately, there's no way to get a warning if another era has been
-- added to EraInMode. Alternative way?
genCardanoTx :: Hedgehog.Gen CardanoTx
genCardanoTx = Gen.choice [ genByronEraInCardanoModeTx
                                 , genShelleyEraInCardanoModeTx
                                 , genAllegraEraInCardanoModeTx
                                 , genMaryEraInCardanoModeTx
                                 , genAlonzoEraInCardanoModeTx
                                 , genBabbageEraInCardanoModeTx
                                 ]

genByronEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genByronEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ByronEra
  pure $ CardanoTx tx C.ByronEraInCardanoMode

genShelleyEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genShelleyEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.ShelleyEra
  pure $ CardanoTx tx C.ShelleyEraInCardanoMode

genAllegraEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genAllegraEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.AllegraEra
  pure $ CardanoTx tx C.AllegraEraInCardanoMode

genMaryEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genMaryEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.MaryEra
  pure $ CardanoTx tx C.MaryEraInCardanoMode

genAlonzoEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genAlonzoEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.AlonzoEra
  pure $ CardanoTx tx C.AlonzoEraInCardanoMode

genBabbageEraInCardanoModeTx :: Hedgehog.Gen CardanoTx
genBabbageEraInCardanoModeTx = do
  tx <- fromGenT $ Gen.genTx C.BabbageEra
  pure $ CardanoTx tx C.BabbageEraInCardanoMode
