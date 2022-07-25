{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# LANGUAGE NumericUnderscores #-}
module Main(main) where

import Cardano.Api qualified as Api
import Cardano.Crypto.Hash qualified as Crypto
import Control.Monad (forM_)
import Data.Aeson qualified as JSON
import Data.Aeson.Extras qualified as JSON
import Data.Aeson.Internal qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.String (IsString (fromString))
import Hedgehog (Property, forAll, property)
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (DiffMilliSeconds (DiffMilliSeconds), Interval (Interval), LowerBound (LowerBound), Slot (Slot),
               UpperBound (UpperBound), fromMilliSeconds, interval)
import Ledger qualified
import Ledger.Ada qualified as Ada
import Ledger.Bytes as Bytes
import Ledger.Generators qualified as Gen
import Ledger.Interval qualified as Interval
import Ledger.TimeSlot (SlotConfig (..))
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Tx qualified as Tx
import Ledger.Tx.CardanoAPI qualified as CardanoAPI
import Ledger.Tx.CardanoAPISpec qualified
import Ledger.Value qualified as Value
import PlutusTx.AssocMap qualified as AMap
import PlutusTx.Prelude qualified as PlutusTx
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (testCase)
import Test.Tasty.HUnit qualified as HUnit
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "all tests" [
    testGroup "UTXO model" [
        testPropertyNamed "initial transaction is valid" "initialTxnValid" initialTxnValid
        ],
    testGroup "intervals" [
        testPropertyNamed "member" "intvlMember," intvlMember,
        testPropertyNamed "contains" "intvlContains" intvlContains
        ],
    testGroup "values" [
        testPropertyNamed "additive identity" "valueAddIdentity," valueAddIdentity,
        testPropertyNamed "additive inverse" "valueAddInverse," valueAddInverse,
        testPropertyNamed "scalar identity" "valueScalarIdentity," valueScalarIdentity,
        testPropertyNamed "scalar distributivity" "valueScalarDistrib" valueScalarDistrib
        ],
    testGroup "Etc." [
        testPropertyNamed "splitVal" "splitVal," splitVal,
        testPropertyNamed "splitVal should respect min Ada per tx output" "splitValMinAda," splitValMinAda,
        testPropertyNamed "encodeByteString" "encodeByteStringTest," encodeByteStringTest,
        testPropertyNamed "encodeSerialise" "encodeSerialiseTest" encodeSerialiseTest
        ],
    testGroup "LedgerBytes" [
        testPropertyNamed "show-fromHex" "ledgerBytesShowFromHexProp," ledgerBytesShowFromHexProp,
        testPropertyNamed "toJSON-fromJSON" "ledgerBytesToJSONProp" ledgerBytesToJSONProp
        ],
    testGroup "Value" ([
        testPropertyNamed "Value ToJSON/FromJSON" "value_json_roundtrip" (jsonRoundTrip Gen.genValue),
        testPropertyNamed "CurrencySymbol ToJSON/FromJSON" "currency_symbol_json_roundtrip" (jsonRoundTrip $ Value.currencySymbol <$> Gen.genSizedByteStringExact 32),
        testPropertyNamed "TokenName ToJSON/FromJSON" "tokenname_json_roundtrip" (jsonRoundTrip Gen.genTokenName),
        testPropertyNamed "TokenName looks like escaped bytestring ToJSON/FromJSON" "tokenname_escaped_roundtrip" (jsonRoundTrip . pure $ ("\NUL0xc0ffee" :: Value.TokenName)),
        testPropertyNamed "CurrencySymbol IsString/Show" "currencySymbolIsStringShow" currencySymbolIsStringShow
        ] ++ (let   vlJson :: BSL.ByteString
                    vlJson = "{\"getValue\":[[{\"unCurrencySymbol\":\"ab01ff\"},[[{\"unTokenName\":\"myToken\"},50]]]]}"
                    vlValue = Value.singleton "ab01ff" "myToken" 50
                in byteStringJson vlJson vlValue)
          ++ (let   vlJson :: BSL.ByteString
                    vlJson = "{\"getValue\":[[{\"unCurrencySymbol\":\"\"},[[{\"unTokenName\":\"\"},50]]]]}"
                    vlValue = Ada.lovelaceValueOf 50
                in byteStringJson vlJson vlValue)),
    testGroup "Tx" [
        testPropertyNamed "TxOut fromTxOut/toTxOut" "ciTxOutRoundTrip" ciTxOutRoundTrip
        ],
    testGroup "TxInfo" [
        testPropertyNamed "TxInfo has non empty ada txMint and txFee" "txInfoNonEmptyAda" txInfoNonEmptyAda
    ],
    testGroup "TxIn" [
        testPropertyNamed "Check that Ord instances of TxIn match" "txInOrdInstanceEquivalenceTest" txInOrdInstanceEquivalenceTest
    ],
    testGroup "TimeSlot" [
        testPropertyNamed "time range of starting slot" "initialSlotToTimeProp," initialSlotToTimeProp,
        testPropertyNamed "slot of starting time range" "initialTimeToSlotProp," initialTimeToSlotProp,
        testPropertyNamed "slot number >=0 when converting from time" "slotIsPositiveProp," slotIsPositiveProp,
        testPropertyNamed "slotRange to timeRange inverse property" "slotToTimeInverseProp," slotToTimeInverseProp,
        testPropertyNamed "timeRange to slotRange inverse property" "timeToSlotInverseProp," timeToSlotInverseProp,
        testPropertyNamed "slot to time range inverse to slot range" "slotToTimeRangeBoundsInverseProp"
            slotToTimeRangeBoundsInverseProp,
        testPropertyNamed "slot to time range has lower bound <= upper bound" "slotToTimeRangeHasLowerAndUpperBoundsProp"
            slotToTimeRangeHasLowerAndUpperBoundsProp,
        testPropertyNamed "POSIX time to UTC time inverse property" "posixTimeToUTCTimeInverseProp" posixTimeToUTCTimeInverseProp
        ],
    -- TODO: Reenable once we update `cardano-node` with the following PR merged:
    -- https://github.com/input-output-hk/cardano-node/pull/3837
    -- testGroup "SomeCardanoApiTx" [
    --     testPropertyNamed "Value ToJSON/FromJSON" "genSomeCardanoApiTx" (jsonRoundTrip Gen.genSomeCardanoApiTx)
    --     ],
    Ledger.Tx.CardanoAPISpec.tests,
    testGroup "Signing" [
        testPropertyNamed "signed payload verifies with public key" "signAndVerifyTest" signAndVerifyTest
        ]
    ]

initialTxnValid :: Property
initialTxnValid = property $ do
    (i, _) <- forAll . pure $ Gen.genInitialTransaction Gen.generatorModel
    Gen.assertValid i Gen.emptyChain

splitVal :: Property
splitVal = property $ do
    i <- forAll $ Gen.integral $ Range.linear 1 (100000 :: Integer)
    n <- forAll $ Gen.integral $ Range.linear 1 100
    vs <- forAll $ Gen.splitVal n i
    Hedgehog.assert $ sum vs == i
    Hedgehog.assert $ length vs <= n

splitValMinAda :: Property
splitValMinAda = property $ do
    let minAda = Ada.getLovelace $ Ledger.minAdaTxOut + Ledger.maxFee
    i <- forAll $ Gen.integral $ Range.linear minAda (100_000_000 :: Integer)
    n <- forAll $ Gen.integral $ Range.linear 1 100
    vs <- forAll $ Gen.splitVal n i
    Hedgehog.assert $ all (\v -> v >= minAda) vs

valueAddIdentity :: Property
valueAddIdentity = property $ do
    vl1 <- forAll Gen.genValue
    Hedgehog.assert $ vl1 == (vl1 PlutusTx.+ PlutusTx.zero)
    Hedgehog.assert $ vl1 == (PlutusTx.zero PlutusTx.+ vl1)

valueAddInverse :: Property
valueAddInverse = property $ do
    vl1 <- forAll Gen.genValue
    let vl1' = PlutusTx.negate vl1
    Hedgehog.assert $ PlutusTx.zero == (vl1 PlutusTx.+ vl1')

valueScalarIdentity :: Property
valueScalarIdentity = property $ do
    vl1 <- forAll Gen.genValue
    Hedgehog.assert $ vl1 == PlutusTx.scale 1 vl1

valueScalarDistrib :: Property
valueScalarDistrib = property $ do
    vl1 <- forAll Gen.genValue
    vl2 <- forAll Gen.genValue
    scalar <- forAll (Gen.integral (fromIntegral <$> Range.linearBounded @Int))
    let r1 = PlutusTx.scale scalar (vl1 PlutusTx.+ vl2)
        r2 = PlutusTx.scale scalar vl1 PlutusTx.+ PlutusTx.scale scalar vl2
    Hedgehog.assert $ r1 == r2

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

    Hedgehog.assert $ result == Aeson.ISuccess bts

ledgerBytesShowFromHexProp :: Property
ledgerBytesShowFromHexProp = property $ do
    bts <- forAll $ LedgerBytes . PlutusTx.toBuiltin <$> Gen.genSizedByteString 32
    let result = Bytes.fromHex $ fromString $ show bts

    Hedgehog.assert $ result == Right bts

ledgerBytesToJSONProp :: Property
ledgerBytesToJSONProp = property $ do
    bts <- forAll $ LedgerBytes . PlutusTx.toBuiltin <$> Gen.genSizedByteString 32
    let enc    = JSON.toJSON bts
        result = Aeson.iparse JSON.parseJSON enc

    Hedgehog.assert $ result == Aeson.ISuccess bts

currencySymbolIsStringShow :: Property
currencySymbolIsStringShow = property $ do
    cs <- forAll $ Value.currencySymbol <$> Gen.genSizedByteStringExact 32
    let cs' = fromString (show cs)
    Hedgehog.assert $ cs' == cs

byteStringJson :: (Show a, Eq a, JSON.ToJSON a, JSON.FromJSON a) => BSL.ByteString -> a -> [TestTree]
byteStringJson jsonString value =
    [ testCase "decoding" $
        HUnit.assertEqual "Simple Decode" (Right value) (JSON.eitherDecode jsonString)
    , testCase "encoding" $ HUnit.assertEqual "Simple Encode" jsonString (JSON.encode value)
    ]

-- | Validate inverse property between 'fromTxOut' and 'toTxOut given a 'TxOut'.
ciTxOutRoundTrip :: Property
ciTxOutRoundTrip = property $ do
  txOuts <- Map.elems . Gen.mockchainUtxo <$> forAll Gen.genMockchain
  forM_ txOuts $ \txOut -> do
    Hedgehog.assert $ Tx.toTxOut (fromJust $ Tx.fromTxOut txOut) == txOut

-- | Asserting that time range of 'scSlotZeroTime' to 'scSlotZeroTime + scSlotLength'
-- is 'Slot 0' and the time after that is 'Slot 1'.
initialSlotToTimeProp :: Property
initialSlotToTimeProp = property $ do
    sc <- forAll Gen.genSlotConfig
    n <- forAll $ Gen.int (fromInteger <$> Range.linear 0 (scSlotLength sc))
    let diff = DiffMilliSeconds $ toInteger n
    let time = TimeSlot.scSlotZeroTime sc + fromMilliSeconds diff
    if diff >= fromIntegral (scSlotLength sc)
        then Hedgehog.assert $ TimeSlot.posixTimeToEnclosingSlot sc time == Slot 1
        else Hedgehog.assert $ TimeSlot.posixTimeToEnclosingSlot sc time == Slot 0

-- | Property that the interval time of 'Slot 0' goes from 'scSlotZeroTime' to
-- 'scSlotZeroTime + scSlotLength - 1'
initialTimeToSlotProp :: Property
initialTimeToSlotProp = property $ do
    sc <- forAll Gen.genSlotConfig
    let beginTime = TimeSlot.scSlotZeroTime sc
        endTime = TimeSlot.scSlotZeroTime sc + fromIntegral (TimeSlot.scSlotLength sc) - 1
        expectedTimeRange = interval beginTime endTime
    Hedgehog.assert $ TimeSlot.slotToPOSIXTimeRange sc 0 == expectedTimeRange

-- | Converting from POSIXTime to Slot should always produce a non negative
-- slot number.
slotIsPositiveProp :: Property
slotIsPositiveProp = property $ do
    sc <- forAll Gen.genSlotConfig
    posixTime <- forAll $ Gen.genPOSIXTime sc
    Hedgehog.assert $ TimeSlot.posixTimeToEnclosingSlot sc posixTime >= 0

-- | Inverse property between 'slotRangeToPOSIXTimeRange' and
-- 'posixTimeRangeToContainedSlotRange' from a 'SlotRange'.
slotToTimeInverseProp :: Property
slotToTimeInverseProp = property $ do
    sc <- forAll Gen.genSlotConfig
    slotRange <- forAll Gen.genSlotRange
    let slotRange' = TimeSlot.posixTimeRangeToContainedSlotRange sc (TimeSlot.slotRangeToPOSIXTimeRange sc slotRange)
    Hedgehog.footnoteShow (slotRange, slotRange')
    Hedgehog.assert $ slotRange == slotRange'

-- | Inverse property between 'posixTimeRangeToContainedSlotRange' and
-- 'slotRangeToPOSIXTimeRange' from a 'POSIXTimeRange'.
timeToSlotInverseProp :: Property
timeToSlotInverseProp = property $ do
    sc <- forAll Gen.genSlotConfig
    timeRange <- forAll $ Gen.genTimeRange sc
    Hedgehog.assert $
        Interval.contains
            timeRange
            (TimeSlot.slotRangeToPOSIXTimeRange sc (TimeSlot.posixTimeRangeToContainedSlotRange sc timeRange))

-- | Left inverse property between 'posixTimeToUTCTime' and
-- 'utcTimeToPOSIXTime' from a 'POSIXTime'.
posixTimeToUTCTimeInverseProp :: Property
posixTimeToUTCTimeInverseProp = property $ do
    sc <- forAll Gen.genSlotConfig
    posixTime <- forAll $ Gen.genPOSIXTime sc
    let posixTime' = TimeSlot.utcTimeToPOSIXTime (TimeSlot.posixTimeToUTCTime posixTime)
    Hedgehog.footnoteShow (posixTime, posixTime')
    Hedgehog.assert $ posixTime' == posixTime

-- | 'POSIXTimeRange' from 'Slot' should have lower bound lower or equal than upper bound
slotToTimeRangeHasLowerAndUpperBoundsProp :: Property
slotToTimeRangeHasLowerAndUpperBoundsProp = property $ do
    sc <- forAll Gen.genSlotConfig
    slot <- forAll Gen.genSlot
    let (Interval (LowerBound t1 _) (UpperBound t2 _)) = TimeSlot.slotToPOSIXTimeRange sc slot
    Hedgehog.assert $ t1 <= t2

-- | Inverse property between 'slotToPOSIXTimeRange and 'posixTimeSlot'.
--
-- Given a slot 's', and the resulting time range [a,b] from
-- 'slotToPOSIXTimeRange s', verify that 'posixTimeSlot a == s' and
-- 'posixTimeSlot b == s'.
slotToTimeRangeBoundsInverseProp :: Property
slotToTimeRangeBoundsInverseProp = property $ do
    sc <- forAll Gen.genSlotConfig
    slot <- forAll Gen.genSlot
    let slotRange = PlutusTx.fmap (TimeSlot.posixTimeToEnclosingSlot sc)
                                  (TimeSlot.slotToPOSIXTimeRange sc slot)
    Hedgehog.assert $ interval slot slot == slotRange

signAndVerifyTest :: Property
signAndVerifyTest = property $ do
  seed <- forAll Gen.genSeed
  pass <- forAll Gen.genPassphrase
  let
    privKey = Ledger.generateFromSeed seed pass
    pubKey = Ledger.toPublicKey privKey
  payload <- forAll $ Gen.bytes $ Range.singleton 128
  Hedgehog.assert $ (\x -> Ledger.signedBy x pubKey payload) $ Ledger.sign payload privKey pass

-- | Check that `txInfoMint` and `txInfoFee` contain ada symbol.
--
-- See note [Mint and Fee fields must have ada symbol].
txInfoNonEmptyAda :: Property
txInfoNonEmptyAda = property $ do
    mockChain <- forAll Gen.genMockchain
    txInfo <- forAll $ Gen.genTxInfo mockChain
    Hedgehog.assert $ (AMap.member Ada.adaSymbol . Value.getValue) $ Ledger.txInfoMint txInfo
    Hedgehog.assert $ (AMap.member Ada.adaSymbol . Value.getValue) $ Ledger.txInfoFee txInfo

-- | Check that Ord instances of cardano-api's 'TxIn' and plutus-ledger-api's 'TxIn' match.
txInOrdInstanceEquivalenceTest :: Property
txInOrdInstanceEquivalenceTest = property $ do
    txIns <- sort <$> forAll (Gen.list (Range.singleton 10) genTxIn)
    let toPlutus = map ((`Tx.TxIn` Nothing) . CardanoAPI.fromCardanoTxIn)
    let plutusTxIns = sort $ toPlutus txIns
    Hedgehog.assert $ (toPlutus txIns) == plutusTxIns

genTxIn :: Hedgehog.MonadGen m => m Api.TxIn
genTxIn = do
    txId <- (\t -> Api.TxId $ Crypto.castHash $ Crypto.hashWith (const t) ()) <$> (Gen.utf8 (Range.singleton 5) Gen.unicode)
    txIx <- Api.TxIx <$> (Gen.integral (Range.linear 0 maxBound))
    return $ Api.TxIn txId txIx
