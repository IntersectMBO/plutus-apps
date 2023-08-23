{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}


module Ledger.Tx.Orphans where

import Codec.Serialise.Class (Serialise (..))
import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value (Object), object, (.:))
import Data.Aeson.Types (parseFail, prependFailure, typeMismatch)
import Data.String (fromString)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), hang, viaShow, vsep, (<+>))

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C

import Ledger.Address (toPlutusAddress)
import Ledger.Address.Orphans ()
import Ledger.Builtins.Orphans ()
import Ledger.Credential.Orphans ()
import Ledger.Scripts.Orphans ()
import Ledger.Tx.Orphans.V1 ()
import Ledger.Tx.Orphans.V2 ()
import Ledger.Value.Orphans ()

instance ToJSON (C.Tx C.BabbageEra) where
  toJSON tx =
    object [ "tx" .= C.serialiseToTextEnvelope Nothing tx ]

instance FromJSON (C.Tx C.BabbageEra) where
  parseJSON (Object v) = do
   envelope <- v .: "tx"
   either (const $ parseFail "Failed to parse BabbageEra 'tx' field from CardanoTx")
          pure
          $ C.deserialiseFromTextEnvelope (C.AsTx C.AsBabbageEra) envelope
  parseJSON invalid =
    prependFailure "parsing CardanoTx failed, " (typeMismatch "Object" invalid)

instance Pretty (C.TxOutDatum ctx era) => Pretty (C.TxOut ctx era) where
  pretty (C.TxOut addr v d rs) =
    hang 2 $ vsep $
      ["-" <+> pretty (C.txOutValueToValue v) <+> "addressed to"
      , pretty (toPlutusAddress addr)
      ]
      <> case d of
          C.TxOutDatumNone -> []
          _                -> [pretty d]
      <> case rs of
          C.ReferenceScript _ (C.ScriptInAnyLang _ s) ->
            ["with reference script hash" <+> viaShow (C.hashScript s)]
          C.ReferenceScriptNone -> []

instance Pretty (C.TxOutDatum C.CtxTx era) where
  pretty C.TxOutDatumNone          = "no datum"
  pretty (C.TxOutDatumInline _ dv) = "with inline datum" <+> viaShow dv
  pretty (C.TxOutDatumInTx _ dv)   = "with datum in tx" <+> viaShow dv
  pretty (C.TxOutDatumHash _ dh)   = "with datum hash" <+> fromString (init . tail $ show dh)

instance Pretty (C.TxOutDatum C.CtxUTxO era) where
  pretty C.TxOutDatumNone          = "no datum"
  pretty (C.TxOutDatumInline _ dv) = "with inline datum" <+> viaShow dv
  pretty (C.TxOutDatumHash _ dh)   = "with datum hash" <+> fromString (init . tail $ show dh)

instance Pretty C.TxId where
  pretty (C.TxId h) = fromString (init $ tail $ show h)
instance Serialise C.TxId where
  encode = encode . C.serialiseToRawBytes
  decode = do
    bs <- decode
    either (fail . show)
      pure
      $ C.deserialiseFromRawBytes C.AsTxId bs

instance Pretty C.TxIn where
  pretty (C.TxIn txId (C.TxIx txIx)) = pretty txId <> "!" <> viaShow txIx

deriving instance Generic C.TxIn
deriving instance Generic C.TxId
deriving instance Generic C.TxIx
deriving instance Serialise C.TxIn
deriving newtype instance Serialise C.TxIx
