module Spec.Marconi.Index.AddressDatum.Generators
    ( genAddressInEra
    , genSimpleScriptData
    , genChainPoint
    , genHashBlockHeader
    )
where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Word (Word64)

import Gen.Cardano.Api.Typed qualified as CGen
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

-- Copied from cardano-api. Delete when this function is reexported
genAddressInEra :: C.CardanoEra era -> Gen (C.AddressInEra era)
genAddressInEra era =
  case C.cardanoEraStyle era of
    C.LegacyByronEra ->
      C.byronAddressInEra <$> CGen.genAddressByron

    C.ShelleyBasedEra _ ->
      Gen.choice
        [ C.byronAddressInEra   <$> CGen.genAddressByron
        , C.shelleyAddressInEra <$> CGen.genAddressShelley
        ]

-- Copied from cardano-api, but removed the recursive construction because it is time consuming ,
-- about a factor of 20 when compared to this simple generator.
genSimpleScriptData :: Gen C.ScriptData
genSimpleScriptData =
    Gen.choice
        [ C.ScriptDataNumber <$> genInteger
        , C.ScriptDataBytes  <$> genByteString
        , C.ScriptDataConstructor <$> genInteger <*> pure []
        , pure $ C.ScriptDataList []
        , pure $ C.ScriptDataMap []
        ]
  where
    genInteger :: Gen Integer
    genInteger = Gen.integral
                  (Range.linear
                    0
                    (fromIntegral (maxBound :: Word64) :: Integer))

    genByteString :: Gen ByteString
    genByteString = BS.pack <$> Gen.list (Range.linear 0 64)
                                         (Gen.word8 Range.constantBounded)

genChainPoint :: Gen C.ChainPoint
genChainPoint = do
    C.ChainPoint <$> CGen.genSlotNo <*> genHashBlockHeader

genHashBlockHeader :: Gen (C.Hash C.BlockHeader)
genHashBlockHeader = C.HeaderHash . BSS.toShort <$> Gen.bytes (Range.singleton 32)
