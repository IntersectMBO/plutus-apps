{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wmissing-import-lists #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE TypeApplications   #-}
module Cardano.Node.Emulator.GeneratorsSpec (tests) where

import Hedgehog (Property, forAll, property)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

import Cardano.Node.Emulator.Generators qualified as Gen
import Cardano.Node.Emulator.TimeSlot (SlotConfig (scSlotLength))
import Cardano.Node.Emulator.TimeSlot qualified as TimeSlot
import Data.Aeson qualified as JSON
import Data.Aeson.Internal qualified as Aeson
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Ledger (DiffMilliSeconds (DiffMilliSeconds), Interval (Interval), LowerBound (LowerBound), Slot (Slot),
               UpperBound (UpperBound), fromMilliSeconds, interval)
import Ledger qualified
import Ledger.Bytes qualified as Bytes
import Ledger.Interval qualified as Interval
import Ledger.Value.CardanoAPI qualified as C
import Plutus.Script.Utils.Ada qualified as Ada
import Plutus.Script.Utils.Value qualified as Value
import PlutusTx.Prelude qualified as PlutusTx

import Data.String (fromString)
import Hedgehog (Gen)


tests :: TestTree
tests = testGroup "Cardano.Node.Emulator.Generators" [
  testGroup "UTXO model" [
    testPropertyNamed "initial transaction is valid" "initialTxnValid" initialTxnValid
  ],
  testGroup "values" [
    testPropertyNamed "additive identity" "valueAddIdentity," valueAddIdentity,
    testPropertyNamed "additive inverse" "valueAddInverse," valueAddInverse,
    testPropertyNamed "scalar identity" "valueScalarIdentity," valueScalarIdentity,
    testPropertyNamed "scalar distributivity" "valueScalarDistrib" valueScalarDistrib
  ],
  testGroup "Etc." [
    testPropertyNamed "splitVal" "splitVal," splitVal,
    testPropertyNamed "splitVal should respect min Ada per tx output" "splitValMinAda," splitValMinAda
  ],
  testGroup "LedgerBytes" [
    testPropertyNamed "show-fromHex" "ledgerBytesShowFromHexProp," ledgerBytesShowFromHexProp,
    testPropertyNamed "toJSON-fromJSON" "ledgerBytesToJSONProp" ledgerBytesToJSONProp
  ],
  testGroup "Value" [
    testPropertyNamed "Value ToJSON/FromJSON" "value_json_roundtrip" (jsonRoundTrip Gen.genValue),
    testPropertyNamed "CurrencySymbol ToJSON/FromJSON" "currency_symbol_json_roundtrip" (jsonRoundTrip $ Value.currencySymbol <$> Gen.genSizedByteStringExact 32),
    testPropertyNamed "CurrencySymbol IsString/Show" "currencySymbolIsStringShow" currencySymbolIsStringShow,
    testPropertyNamed "Old split equals the new split" "valueSplit" valueSplit
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
    let minAda = Ada.getLovelace $ Ledger.minAdaTxOutEstimated + Ledger.maxFee
    i <- forAll $ Gen.integral $ Range.linear minAda (100_000_000 :: Integer)
    n <- forAll $ Gen.integral $ Range.linear 1 100
    vs <- forAll $ Gen.splitVal n i
    Hedgehog.assert $ all (\v -> v >= minAda) vs

ledgerBytesShowFromHexProp :: Property
ledgerBytesShowFromHexProp = property $ do
    bts <- forAll $ Bytes.LedgerBytes . PlutusTx.toBuiltin <$> Gen.genSizedByteString 32
    let result = Bytes.fromHex $ fromString $ show bts

    Hedgehog.assert $ result == Right bts

ledgerBytesToJSONProp :: Property
ledgerBytesToJSONProp = property $ do
    bts <- forAll $ Bytes.LedgerBytes . PlutusTx.toBuiltin <$> Gen.genSizedByteString 32
    let enc    = JSON.toJSON bts
        result = Aeson.iparse JSON.parseJSON enc

    Hedgehog.assert $ result == Aeson.ISuccess bts

currencySymbolIsStringShow :: Property
currencySymbolIsStringShow = property $ do
    cs <- forAll $ Value.currencySymbol <$> Gen.genSizedByteStringExact 32
    let cs' = fromString (show cs)
    Hedgehog.assert $ cs' == cs

valueAddIdentity :: Property
valueAddIdentity = property $ do
    vl1 <- forAll Gen.genValue
    Hedgehog.assert $ vl1 == vl1 <> mempty
    Hedgehog.assert $ vl1 == mempty <> vl1

valueAddInverse :: Property
valueAddInverse = property $ do
    vl1 <- forAll Gen.genValue
    let vl1' = C.negateValue vl1
    Hedgehog.assert $ mempty == (vl1 <> vl1')

valueScalarIdentity :: Property
valueScalarIdentity = property $ do
    vl1 <- forAll Gen.genValue
    Hedgehog.assert $ vl1 == C.scale 1 vl1

valueScalarDistrib :: Property
valueScalarDistrib = property $ do
    vl1 <- forAll Gen.genValue
    vl2 <- forAll Gen.genValue
    scalar <- forAll (Gen.integral (fromIntegral <$> Range.linearBounded @Int))
    let r1 = C.scale scalar (vl1 <> vl2)
        r2 = C.scale scalar vl1 <> C.scale scalar vl2
    Hedgehog.assert $ r1 == r2

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

jsonRoundTrip :: (Show a, Eq a, JSON.FromJSON a, JSON.ToJSON a) => Hedgehog.Gen a -> Property
jsonRoundTrip gen = property $ do
    bts <- forAll gen
    let enc    = JSON.toJSON bts
        result = Aeson.iparse JSON.parseJSON enc

    Hedgehog.assert $ result == Aeson.ISuccess bts

valueSplit :: Property
valueSplit = property $ do
    vl <- forAll Gen.genValue
    let (pvl1, pvl2) = Value.split (C.fromCardanoValue vl)
        (vl1, vl2) = C.split vl
    Hedgehog.assert $ (pvl1, pvl2) == (C.fromCardanoValue vl1, C.fromCardanoValue vl2)
