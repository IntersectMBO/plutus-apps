module Helpers.Common where

import Cardano.Api qualified as C

-- | Any CardanoEra with CardanoMode
toEraInCardanoMode :: C.CardanoEra era -> (C.EraInMode era C.CardanoMode)
toEraInCardanoMode era = fromMaybe $ C.toEraInMode era C.CardanoMode
  where
    fromMaybe Nothing    = error $ "No mode for this era " ++ show era ++ " in CardanoMode"
    fromMaybe (Just eim) = eim

-- | Converts a C.CardanoEra to a C.ShelleyBasedEra. Used for querying in Shelley-based era.
cardanoEraToShelleyBasedEra :: C.CardanoEra era -> C.ShelleyBasedEra era
cardanoEraToShelleyBasedEra cEra = case cEra of
  C.AlonzoEra  -> C.ShelleyBasedEraAlonzo
  C.BabbageEra -> C.ShelleyBasedEraBabbage
  _            -> error "Must use Alonzo or Babbage era"

-- | Make a payment or script address
makeAddress ::
  (Either (C.VerificationKey C.PaymentKey) C.ScriptHash) ->
  C.NetworkId ->
  C.Address C.ShelleyAddr
makeAddress (Left paymentKey) nId =
  C.makeShelleyAddress nId (C.PaymentCredentialByKey $ C.verificationKeyHash paymentKey) C.NoStakeAddress
makeAddress (Right scriptHash) nId =
  C.makeShelleyAddress nId (C.PaymentCredentialByScript scriptHash) C.NoStakeAddress
