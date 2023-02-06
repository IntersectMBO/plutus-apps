{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

module Spec.Marconi.Index.AddressDatum.AddressDatumIndexEvent
    ( tests
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash.Class qualified as CRYPTO
import Cardano.Ledger.SafeHash (unsafeMakeSafeHash)
import Control.Monad (forM)
import Data.Coerce (coerce)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe (catMaybes, fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import GHC.Int (Int64)
import GHC.Natural (Natural)
import GHC.Real (Ratio, (%))
import Gen.Cardano.Api.Typed qualified as CGen
import PlutusCore (defaultCostModelParams)
import RewindableIndex.Storable qualified as Storable
import Test.Tasty (TestTree, localOption, testGroup)
import Test.Tasty.Hedgehog (HedgehogTestLimit (HedgehogTestLimit), testPropertyNamed)

import Hedgehog (Gen, Property, forAll, property, (===))
import Hedgehog qualified
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Marconi.Index.AddressDatum (AddressDatumHandle, StorableEvent (AddressDatumIndexEvent))
import Marconi.Index.AddressDatum qualified as AddressDatum
import Spec.Marconi.Index.AddressDatum.Generators (genAddressInEra, genChainPoint, genSimpleScriptData)
import Spec.Marconi.Index.AddressDatum.Utils (addressInEraToAddressAny)

tests :: TestTree
tests = localOption (HedgehogTestLimit $ Just 200) $
    testGroup "Spec.Marconi.Index.AddressDatum.AddressDatumIndexEvent.toAddressDatumIndexEvent"
    [ testPropertyNamed
          "should track the addresses with datum in a transaction output (datum hash, datum in tx body and inline datum)"
          "propShouldIndexAddressWithTxOutDatum"
          propShouldIndexAddressWithTxOutDatum
    -- TODO Very slow test case. There seems to be a performance issue with creating transactions
    -- with cardano-api when Plutus scripts are included in the witness set.
    , testPropertyNamed
          "should track the addresses with datums that are part of a Plutus datum witness set"
          "propShouldAlwaysIndexPlutusDatumWitness"
          propShouldAlwaysIndexPlutusDatumWitness
    , testPropertyNamed
          "should not track addresses that are not linked to any datum"
          "propShouldNotIndexAddressWithoutDatum"
          propShouldNotIndexAddressWithoutDatum
    , testPropertyNamed
          "should track addresses based on provided address filter"
          "propShouldIndexAddressBasedOnFilter"
          propShouldIndexAddressBasedOnFilter
    ]

propShouldIndexAddressWithTxOutDatum :: Property
propShouldIndexAddressWithTxOutDatum = property $ do
    cp <- forAll genChainPoint
    let datGen =
            Gen.choice
                [ fmap (\d -> TxOutDatumHashLocation (C.hashScriptData d) d) genSimpleScriptData
                , fmap (\d -> TxOutDatumInTxLocation (C.hashScriptData d) d) genSimpleScriptData
                , fmap (\d -> TxOutDatumInlineLocation (C.hashScriptData d) d) genSimpleScriptData
                ]
    addressesDatum <- forAll $ genAddressesWithDatum datGen
    Hedgehog.cover 30 "At least one address with datum hash in tx out"
        $ isJust
        $ List.find (\(_, dat) -> case dat of { TxOutDatumHashLocation {} -> True; _ -> False })
        addressesDatum
    Hedgehog.cover 30 "At least one address with datum hash in tx out and datum in tx body"
        $ isJust
        $ List.find (\(_, dat) -> case dat of { TxOutDatumInTxLocation {} -> True; _ -> False })
        addressesDatum
    Hedgehog.cover 30 "At least one address with inline datum in tx out"
        $ isJust
        $ List.find (\(_, dat) -> case dat of { TxOutDatumInlineLocation {} -> True; _ -> False })
        addressesDatum
    Hedgehog.cover 10 "At least one address with multiple datums"
        $ isJust
        $ List.find (\xs -> length xs > 1)
        $ List.groupBy (\x y -> fst x == fst y)
        $ List.sortOn (addressInEraToAddressAny . fst)
        addressesDatum

    txs <- forAll $ Gen.list (Range.constant 1 5)
                  $ C.makeSignedTransaction [] <$> genTxBodyWithAddresses addressesDatum

    let actualAddressDatumIndexEvent = AddressDatum.toAddressDatumIndexEvent Nothing txs cp
    let expectedAddressDatumIndexEvent =
            addressesDatumToAddressDatumIndexEvent Nothing cp addressesDatum
    expectedAddressDatumIndexEvent === actualAddressDatumIndexEvent

propShouldNotIndexAddressWithoutDatum :: Property
propShouldNotIndexAddressWithoutDatum = property $ do
    cp <- forAll genChainPoint
    let datGen = pure NoDatumLocation
    addresses <- forAll $ genAddressesWithDatum datGen
    txs <- forAll $ Gen.list (Range.constant 1 5)
                  $ C.makeSignedTransaction [] <$> genTxBodyWithAddresses addresses
    let (AddressDatumIndexEvent addressDats datums _) = AddressDatum.toAddressDatumIndexEvent Nothing txs cp

    Hedgehog.assert $ List.null addressDats
    Hedgehog.assert $ List.null datums

-- Having a Plutus Script datum witness implies that there is an UTxO with the hash of that datum.
propShouldAlwaysIndexPlutusDatumWitness :: Property
propShouldAlwaysIndexPlutusDatumWitness = property $ do
    cp <- forAll genChainPoint

    let txOutDatGen =
            Gen.choice [ fmap (\d -> TxOutDatumHashLocation (C.hashScriptData d) d) genSimpleScriptData
                       , fmap (\d -> TxOutDatumInTxLocation (C.hashScriptData d) d) genSimpleScriptData
                       , fmap (\d -> TxOutDatumInlineLocation (C.hashScriptData d) d) genSimpleScriptData
                       ]
    addressesDatum1 <- forAll $ genAddressesWithDatum txOutDatGen
    txs1 <- forAll $ Gen.list (Range.constant 1 5)
                   $ C.makeSignedTransaction [] <$> genTxBodyWithAddresses addressesDatum1

    let plutusWitDatGen =
            fmap (\d -> PlutusScriptDatumLocation (C.hashScriptData d) d) genSimpleScriptData
    addressesDatum2 <- forAll $ genAddressesWithDatum plutusWitDatGen
    txs2 <- forAll $ Gen.list (Range.constant 1 5)
                   $ C.makeSignedTransaction [] <$> genTxBodyWithAddresses addressesDatum2
    let txs = txs2 <> txs1
        addressesDatum = addressesDatum1 <> addressesDatum2

    let actualAddressDatumIndexEvent = AddressDatum.toAddressDatumIndexEvent Nothing txs cp
    let expectedAddressDatumIndexEvent =
            addressesDatumToAddressDatumIndexEvent Nothing cp addressesDatum
    expectedAddressDatumIndexEvent === actualAddressDatumIndexEvent

propShouldIndexAddressBasedOnFilter :: Property
propShouldIndexAddressBasedOnFilter = property $ do
    cp <- forAll genChainPoint
    let datGen = fmap (\d -> TxOutDatumInlineLocation (C.hashScriptData d) d) genSimpleScriptData
    addressesWithDatum <- forAll $ genAddressesWithDatum datGen
    let filteredAddresses =
            List.nub
          $ fmap (fst . snd)
          $ filter (\(i, (addr, _)) -> even i || not (isShelleyAddressInEra addr))
          $ zip [(0::Integer)..] addressesWithDatum
    let filterF addr =
            C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr `elem` filteredAddresses
    txs <- forAll $ Gen.list (Range.constant 1 5)
                  $ C.makeSignedTransaction [] <$> genTxBodyWithAddresses addressesWithDatum

    let actualAddressDatumIndexEvent = AddressDatum.toAddressDatumIndexEvent (Just filterF) txs cp
    let expectedAddressDatumIndexEvent =
            addressesDatumToAddressDatumIndexEvent (Just filterF) cp addressesWithDatum
    expectedAddressDatumIndexEvent === actualAddressDatumIndexEvent

addressesDatumToAddressDatumIndexEvent
    :: Maybe (C.Address C.ShelleyAddr -> Bool)
    -> C.ChainPoint
    -> [(C.AddressInEra C.BabbageEra, DatumLocation)]
    -> Storable.StorableEvent AddressDatumHandle
addressesDatumToAddressDatumIndexEvent filterF cp addressDatums =
    let addressesWithDatumFilter (C.AddressInEra C.ByronAddressInAnyEra _, _) =
            True
        addressesWithDatumFilter (C.AddressInEra (C.ShelleyAddressInEra C.ShelleyBasedEraBabbage) addr, _) =
            case filterF of
              Nothing -> True
              Just f  -> f addr
        filteredAddressDatums = filter addressesWithDatumFilter addressDatums

        addressDatumsMap =
            Map.fromListWith (<>)
                $ mapMaybe (\(addr, datLoc) ->
                    fmap (\(dh, _) -> (addressInEraToAddressAny addr, Set.singleton dh))
                        $ getDatumFromTxOutLocation datLoc) filteredAddressDatums

        datums = Map.fromList
               $ mapMaybe (\(h, d) -> fmap (h,) d)
               $ mapMaybe (\(_ , datLoc) -> getDatumFromAnyLocation datLoc) filteredAddressDatums
     in AddressDatumIndexEvent addressDatumsMap datums cp
 where
    getDatumFromAnyLocation :: DatumLocation -> Maybe (C.Hash C.ScriptData, Maybe C.ScriptData)
    getDatumFromAnyLocation NoDatumLocation                  = Nothing
    getDatumFromAnyLocation (TxOutDatumHashLocation hd _)    = Just (hd, Nothing)
    getDatumFromAnyLocation (TxOutDatumInTxLocation hd d)    = Just (hd, Just d)
    getDatumFromAnyLocation (TxOutDatumInlineLocation hd d)  = Just (hd, Just d)
    getDatumFromAnyLocation (PlutusScriptDatumLocation hd d) = Just (hd, Just d)

    getDatumFromTxOutLocation :: DatumLocation -> Maybe (C.Hash C.ScriptData, Maybe C.ScriptData)
    getDatumFromTxOutLocation NoDatumLocation                 = Nothing
    getDatumFromTxOutLocation (TxOutDatumHashLocation hd _)   = Just (hd, Nothing)
    getDatumFromTxOutLocation (TxOutDatumInTxLocation hd d)   = Just (hd, Just d)
    getDatumFromTxOutLocation (TxOutDatumInlineLocation hd d) = Just (hd, Just d)
    getDatumFromTxOutLocation (PlutusScriptDatumLocation _ _) = Nothing

-- | TxOutDatumInScriptWitness C.ScriptData
data DatumLocation
    = NoDatumLocation
    | TxOutDatumHashLocation (C.Hash C.ScriptData) C.ScriptData
    | TxOutDatumInTxLocation (C.Hash C.ScriptData) C.ScriptData
    | TxOutDatumInlineLocation (C.Hash C.ScriptData) C.ScriptData
    | PlutusScriptDatumLocation (C.Hash C.ScriptData) C.ScriptData
    deriving (Show)

genAddressesWithDatum :: Gen DatumLocation -> Gen [(C.AddressInEra C.BabbageEra, DatumLocation)]
genAddressesWithDatum genDatumLocation = do
    addresses <- Gen.list (Range.constant 1 5) $ genAddressInEra C.BabbageEra
    -- We do 'addresses ++ addresses' to generate duplicate addresses so that we can test that we
    -- correctly index different datums for the same address.
    forM (addresses ++ addresses) $ \addr -> do
        datLocation <- genDatumLocation
        pure (addr, datLocation)

genTxBodyWithAddresses :: [(C.AddressInEra C.BabbageEra, DatumLocation)] -> Gen (C.TxBody C.BabbageEra)
genTxBodyWithAddresses addresses = do
  res <- C.makeTransactionBody <$> genTxBodyContentWithAddresses addresses
  case res of
    Left err     -> fail (C.displayError err)
    Right txBody -> pure txBody

genTxBodyContentWithAddresses
    :: [(C.AddressInEra C.BabbageEra, DatumLocation)]
    -> Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentWithAddresses addressesDatumLocation = do
    exUnits <- genExecutionUnits
    scriptTxIns <- fmap catMaybes <$> forM addressesDatumLocation
        $ \case
            (_, PlutusScriptDatumLocation _ d) -> do
                txIn <- CGen.genTxIn
                let witness = C.ScriptWitness C.ScriptWitnessForSpending
                            $ C.PlutusScriptWitness
                                C.PlutusScriptV1InBabbage
                                C.PlutusScriptV1
                                (C.PScript $ C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn)
                                (C.ScriptDatumForTxIn d)
                                d
                                exUnits
                pure $ Just (txIn, C.BuildTxWith witness)
            (_, _) -> pure Nothing

    txOuts <- forM addressesDatumLocation
        $ \case
            (addr, TxOutDatumHashLocation hd _) -> do
                let txOutGen =
                        C.TxOut addr <$> genTxOutValue C.BabbageEra
                                     <*> pure (C.TxOutDatumHash C.ScriptDataInBabbageEra hd)
                                     <*> pure C.ReferenceScriptNone
                Gen.list (Range.constant 1 2) txOutGen
            (addr, TxOutDatumInTxLocation _ d) -> do
                let txOutGen =
                        C.TxOut addr <$> genTxOutValue C.BabbageEra
                                     <*> pure (C.TxOutDatumInTx C.ScriptDataInBabbageEra d)
                                     <*> pure C.ReferenceScriptNone
                Gen.list (Range.constant 1 2) txOutGen
            (addr, TxOutDatumInlineLocation _ d) -> do
                let txOutGen =
                        C.TxOut addr <$> genTxOutValue C.BabbageEra
                                     <*> pure (C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra d)
                                     <*> pure C.ReferenceScriptNone
                Gen.list (Range.constant 1 2) txOutGen
            (_, _) -> pure []

    txBody <- genTxBodyContentWithPlutusScripts
    pure $ txBody
        { C.txIns = C.txIns txBody <> scriptTxIns
        , C.txOuts = concat txOuts
        }

genTxBodyContentWithPlutusScripts :: Gen (C.TxBodyContent C.BuildTx C.BabbageEra)
genTxBodyContentWithPlutusScripts = do
  txIns <- map (, C.BuildTxWith (C.KeyWitness C.KeyWitnessForSpending)) <$> Gen.list (Range.constant 1 10) CGen.genTxIn
  txInsCollateral <- C.TxInsCollateral C.CollateralInBabbageEra <$> Gen.list (Range.linear 1 10) CGen.genTxIn
  let txInsReference = C.TxInsReferenceNone
  txOuts <- Gen.list (Range.constant 1 10) (genTxOutTxContext C.BabbageEra)
  let txTotalCollateral = C.TxTotalCollateralNone
  let txReturnCollateral = C.TxReturnCollateralNone
  txFee <- genTxFee C.BabbageEra
  let txValidityRange = (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound C.ValidityNoUpperBoundInBabbageEra)
  let txMetadata = C.TxMetadataNone
  let txAuxScripts = C.TxAuxScriptsNone
  let txExtraKeyWits = C.TxExtraKeyWitnessesNone
  txProtocolParams <- C.BuildTxWith . Just <$> genProtocolParametersForPlutusScripts
  let txWithdrawals = C.TxWithdrawalsNone
  let txCertificates = C.TxCertificatesNone
  let txUpdateProposal = C.TxUpdateProposalNone
  let txMintValue = C.TxMintNone
  let txScriptValidity = C.TxScriptValidity C.TxScriptValiditySupportedInBabbageEra C.ScriptValid

  pure $ C.TxBodyContent
    { C.txIns
    , C.txInsCollateral
    , C.txInsReference
    , C.txOuts
    , C.txTotalCollateral
    , C.txReturnCollateral
    , C.txFee
    , C.txValidityRange
    , C.txMetadata
    , C.txAuxScripts
    , C.txExtraKeyWits
    , C.txProtocolParams
    , C.txWithdrawals
    , C.txCertificates
    , C.txUpdateProposal
    , C.txMintValue
    , C.txScriptValidity
    }
 where
    genTxOutTxContext :: C.CardanoEra era -> Gen (C.TxOut C.CtxTx era)
    genTxOutTxContext era =
      C.TxOut <$> genAddressInEra era
              <*> genTxOutValue era
              <*> genSimpleTxOutDatumHashTxContext era
              <*> constantReferenceScript era
    constantReferenceScript :: C.CardanoEra era -> Gen (C.ReferenceScript era)
    constantReferenceScript era =
      case C.refInsScriptsAndInlineDatsSupportedInEra era of
        Nothing -> return C.ReferenceScriptNone
        Just supp -> pure
                   $ C.ReferenceScript supp
                   $ C.ScriptInAnyLang (C.PlutusScriptLanguage C.PlutusScriptV1)
                   $ C.PlutusScript C.PlutusScriptV1
                   $ C.examplePlutusScriptAlwaysSucceeds C.WitCtxTxIn

    genSimpleTxOutDatumHashTxContext :: C.CardanoEra era -> Gen (C.TxOutDatum C.CtxTx era)
    genSimpleTxOutDatumHashTxContext era = case era of
        C.ByronEra   -> pure C.TxOutDatumNone
        C.ShelleyEra -> pure C.TxOutDatumNone
        C.AllegraEra -> pure C.TxOutDatumNone
        C.MaryEra    -> pure C.TxOutDatumNone
        C.AlonzoEra  -> Gen.choice
                        [ pure C.TxOutDatumNone
                        , C.TxOutDatumHash C.ScriptDataInAlonzoEra <$> genHashScriptData
                        , C.TxOutDatumInTx C.ScriptDataInAlonzoEra <$> genSimpleScriptData
                        ]
        C.BabbageEra -> Gen.choice
                        [ pure C.TxOutDatumNone
                        , C.TxOutDatumHash C.ScriptDataInBabbageEra <$> genHashScriptData
                        , C.TxOutDatumInTx C.ScriptDataInBabbageEra <$> genSimpleScriptData
                        , C.TxOutDatumInline C.ReferenceTxInsScriptsInlineDatumsInBabbageEra <$> genSimpleScriptData
                        ]

    -- Copied from cardano-api. Delete when this function is reexported
    genHashScriptData :: Gen (C.Hash C.ScriptData)
    genHashScriptData = C.ScriptDataHash . unsafeMakeSafeHash . mkDummyHash <$> Gen.int (Range.linear 0 10)

    mkDummyHash :: forall h a. CRYPTO.HashAlgorithm h => Int -> CRYPTO.Hash h a
    mkDummyHash = coerce . CRYPTO.hashWithSerialiser @h CBOR.toCBOR

    -- Copied from cardano-api. Delete when this function is reexported
    genTxFee :: C.CardanoEra era -> Gen (C.TxFee era)
    genTxFee era =
      case C.txFeesExplicitInEra era of
        Left supported  -> pure (C.TxFeeImplicit supported)
        Right supported -> C.TxFeeExplicit supported <$> CGen.genLovelace

genProtocolParametersForPlutusScripts :: Gen C.ProtocolParameters
genProtocolParametersForPlutusScripts =
  C.ProtocolParameters
    <$> ((,) <$> genNat <*> genNat)
    <*> Gen.maybe CGen.genRational
    <*> CGen.genMaybePraosNonce
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> genNat
    <*> Gen.maybe CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> CGen.genLovelace
    <*> genEpochNo
    <*> genNat
    <*> genRationalInt64
    <*> CGen.genRational
    <*> CGen.genRational
    <*> pure Nothing -- Obsolete from babbage onwards
    <*> pure (Map.fromList
      [ (C.AnyPlutusScriptVersion C.PlutusScriptV1, C.CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams)
      , (C.AnyPlutusScriptVersion C.PlutusScriptV2, C.CostModel $ fromMaybe (error "Ledger.Params: defaultCostModelParams is broken") defaultCostModelParams) ])
    <*> (Just <$> genExecutionUnitPrices)
    <*> (Just <$> genExecutionUnits)
    <*> (Just <$> genExecutionUnits)
    <*> (Just <$> genNat)
    <*> (Just <$> genNat)
    <*> (Just <$> genNat)
    <*> (Just <$> CGen.genLovelace)
 where
    -- Copied from cardano-api. Delete when this function is reexported
    genRationalInt64 :: Gen Rational
    genRationalInt64 =
        (\d -> ratioToRational (1 % d)) <$> genDenominator
      where
        genDenominator :: Gen Int64
        genDenominator = Gen.integral (Range.linear 1 maxBound)

        ratioToRational :: Ratio Int64 -> Rational
        ratioToRational = toRational

    -- Copied from cardano-api. Delete when this function is reexported
    genEpochNo :: Gen C.EpochNo
    genEpochNo = C.EpochNo <$> Gen.word64 (Range.linear 0 10)

    -- Copied from cardano-api. Delete when this function is reexported
    genNat :: Gen Natural
    genNat = Gen.integral (Range.linear 0 10)

    -- Copied from cardano-api. Delete when this function is reexported
    genExecutionUnitPrices :: Gen C.ExecutionUnitPrices
    genExecutionUnitPrices = C.ExecutionUnitPrices <$> CGen.genRational <*> CGen.genRational

-- Copied from cardano-api. Delete when this function is reexported
genExecutionUnits :: Gen C.ExecutionUnits
genExecutionUnits = C.ExecutionUnits <$> Gen.integral (Range.constant 0 1000)
                                     <*> Gen.integral (Range.constant 0 1000)

genTxOutValue :: C.CardanoEra era -> Gen (C.TxOutValue era)
genTxOutValue era =
  case C.multiAssetSupportedInEra era of
    Left adaOnlyInEra     -> C.TxOutAdaOnly adaOnlyInEra <$> CGen.genLovelace
    Right multiAssetInEra -> C.TxOutValue multiAssetInEra . C.lovelaceToValue <$> CGen.genLovelace

isShelleyAddressInEra :: C.AddressInEra era -> Bool
isShelleyAddressInEra (C.AddressInEra _ C.ShelleyAddress {}) = True
isShelleyAddressInEra _                                      = False
