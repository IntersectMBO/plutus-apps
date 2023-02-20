{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE MonoLocalBinds     #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
-- | Turn 'UnbalancedTx' values into transactions using the
--   wallet API.
module Cardano.Wallet.LocalClient.ExportTx(
      balanceTx
    , handleTx
    , yieldUnbalancedTx
    , WAPI.signTxAndSubmit
    -- * Exporting transactions
    , ExportTx(..)
    , ExportTxInput(..)
    , ExportTxRedeemer(..)
    , export
    ) where

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Params (Params)
import Cardano.Node.Emulator.Validation (CardanoLedgerError, makeTransactionBody)
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error, throwError)
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (String), object, withObject, (.:), (.=))
import Data.Aeson.Extras qualified as JSON
import Data.Aeson.Types (Parser, parseFail)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Ledger (DCert, StakingCredential)
import Ledger qualified as P
import Ledger.Tx (CardanoTx, TxId (TxId), TxOutRef)
import Ledger.Tx.CardanoAPI (fromPlutusIndex)
import Ledger.Tx.Constraints (UnbalancedTx (UnbalancedCardanoTx))
import Plutus.Contract.CardanoAPI qualified as CardanoAPI
import Plutus.V1.Ledger.Api qualified as Plutus
import Plutus.V1.Ledger.Scripts (MintingPolicyHash)
import PlutusTx qualified
import Wallet.API qualified as WAPI
import Wallet.Effects (WalletEffect, balanceTx, yieldUnbalancedTx)
import Wallet.Emulator.Error (WalletAPIError)

{- Note [Submitting transactions from Plutus contracts]

'UnbalancedTx' is the type of transactions that meet some set of constraints
(produced by 'Ledger.Constraints.OffChain.mkTx'), but can't be submitted to
the ledger yet because they may not be balanced and they lack signatures and
fee payments. To turn an 'UnbalancedTx' value into a valid transaction that can
be submitted to the network, the contract backend needs to

* Balance it.
  If the total value of 'txInputs' + the 'txMint' field is
  greater than the total value of 'txOutputs', then one or more public key
  outputs need to be added. How many and what addresses they are is up
  to the wallet (probably configurable).
  If the total balance 'txInputs' + the 'txMint' field is less than
  the total value of 'txOutputs', then one or more public key inputs need
  to be added (and potentially some outputs for the change).

* Compute fees.
  Once the final size of the transaction is known, the fees for the transaction
  can be computed. The transaction fee needs to be paid for with additional
  inputs so I assume that this step and the previous step will be combined.

  Also note that even if the 'UnbalancedTx' that we get from the contract
  endpoint happens to be balanced already, we still need to add fees to it. So
  we can't skip the balancing & fee computation step.

  Balancing and coin selection will eventually be performed by the wallet
  backend.

* Sign it.
  The signing process needs to provide signatures for all public key
  inputs in the balanced transaction.

-}

-- | Balance an unabalanced transaction, sign it, and submit
--   it to the chain in the context of a wallet.
handleTx ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    )
    => UnbalancedTx -> Eff effs CardanoTx
handleTx = balanceTx >=> either throwError WAPI.signTxAndSubmit

data ExportTxRedeemerPurpose = Spending | Minting | Rewarding | Certifying

instance ToJSON ExportTxRedeemerPurpose where
    toJSON = \case
        Spending   -> String "spending"
        Minting    -> String "minting"
        Rewarding  -> String "rewarding"
        Certifying -> String "certifying"

data ExportTxRedeemer =
    SpendingRedeemer{ redeemer:: Plutus.Redeemer, redeemerOutRef :: TxOutRef }
    | MintingRedeemer { redeemer:: Plutus.Redeemer, redeemerPolicyId :: MintingPolicyHash }
    | RewardingRedeemer { redeemer:: Plutus.Redeemer, redeemerStakingCredential :: StakingCredential}
    | CertifyingRedeemer { redeemer:: Plutus.Redeemer, redeemerDCert :: DCert }
    deriving stock (Eq, Show, Generic, Typeable)

instance FromJSON ExportTxRedeemer where
    parseJSON v = parseSpendingRedeemer v <|> parseMintingRedeemer v <|> parseRewardingRedeemer v <|> parseCertifyingRedeemer v

parseSpendingRedeemer :: Value -> Parser ExportTxRedeemer
parseSpendingRedeemer =
    withObject "Redeemer" $ \o -> do
        inputObj <- o .: "input" :: Parser Object
        let txOutRefParse = Plutus.TxOutRef <$> (TxId <$> (inputObj .: "id"))
                                            <*> inputObj .: "index"
        SpendingRedeemer <$> parseRedeemerData o <*> txOutRefParse

parseMintingRedeemer :: Value -> Parser ExportTxRedeemer
parseMintingRedeemer =
    withObject "Redeemer" $ \o -> MintingRedeemer
        <$> parseRedeemerData o
        <*> o .: "policy_id"

-- TODO
parseRewardingRedeemer :: Value -> Parser ExportTxRedeemer
parseRewardingRedeemer = error "Unimplemented rewarding redeemer parsing."

-- TODO
parseCertifyingRedeemer :: Value -> Parser ExportTxRedeemer
parseCertifyingRedeemer = error "Unimplemented certifying redeemer parsing."

parseRedeemerData :: Object -> Parser Plutus.Redeemer
parseRedeemerData o =
    fmap (\(JSON.JSONViaSerialise d) -> Plutus.Redeemer $ PlutusTx.dataToBuiltinData d)
         (o .: "data")

instance ToJSON ExportTxRedeemer where
    toJSON SpendingRedeemer{redeemer=Plutus.Redeemer dt, redeemerOutRef=Plutus.TxOutRef{Plutus.txOutRefId, Plutus.txOutRefIdx}} =
        object ["purpose" .= Spending, "data" .= JSON.JSONViaSerialise (PlutusTx.builtinDataToData dt), "input" .= object ["id" .= Plutus.getTxId txOutRefId, "index" .= txOutRefIdx]]
    toJSON MintingRedeemer{redeemer=Plutus.Redeemer dt, redeemerPolicyId} =
        object ["purpose" .= Minting, "data" .= JSON.JSONViaSerialise (PlutusTx.builtinDataToData dt), "policy_id" .= redeemerPolicyId]
    -- TODO
    toJSON RewardingRedeemer{} = error "Unimplemented rewarding redeemer encoding."
    toJSON CertifyingRedeemer{} = error "Unimplemented certifying redeemer encoding."

-- | Partial transaction that can be balanced by the wallet backend.
data ExportTx =
        ExportTx
            { partialTx :: C.Tx C.BabbageEra -- ^ The transaction itself
            , lookups   :: [ExportTxInput] -- ^ The tx outputs for all inputs spent by the partial tx
            , redeemers :: [ExportTxRedeemer]
            }
    deriving stock (Eq, Show, Generic, Typeable)

instance FromJSON ExportTx where
  parseJSON = withObject "ExportTx" $ \v -> ExportTx
      <$> parsePartialTx v
      <*> v .: "inputs"
      <*> v .: "redeemers"
    where
      parsePartialTx v =
        v .: "transaction" >>= \t ->
          either parseFail pure $ JSON.tryDecode t
                              >>= (first show . C.deserialiseFromCBOR (C.AsTx C.AsBabbageEra))

-- IMPORTANT: The JSON produced here needs to match the schema expected by
-- https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/balanceTransaction
instance ToJSON ExportTx where
    toJSON ExportTx{partialTx, lookups, redeemers} =
        object
            [ "transaction" .= JSON.encodeByteString (C.serialiseToCBOR partialTx)
            , "inputs"      .= lookups
            , "redeemers"   .= redeemers
            ]

data ExportTxInput =
    ExportTxInput
        { etxiId               :: C.TxId
        , etxiTxIx             :: C.TxIx
        , etxiAddress          :: C.AddressInEra C.BabbageEra
        , etxiLovelaceQuantity :: C.Lovelace
        , etxiDatumHash        :: Maybe (C.Hash C.ScriptData)
        , etxiAssets           :: [(C.PolicyId, C.AssetName, C.Quantity)]
        }
    deriving stock (Eq, Show, Generic)

instance FromJSON ExportTxInput where
    parseJSON = withObject "ExportTxInput" $ \o -> ExportTxInput
        <$> o .: "id"
        <*> o .: "index"
        <*> parseAddress o
        <*> (o .: "amount" >>= \amountField -> amountField .: "quantity")
        <*> o .: "datum"
        <*> (o .: "assets" >>= mapM parseAsset)
      where
          parseAddress o = do
              addressField <- o .: "address"
              let deserialisedAddr = C.deserialiseAddress (C.AsAddressInEra C.AsBabbageEra) addressField
              maybe (parseFail "Failed to deserialise address field") pure deserialisedAddr
          parseAsset :: Object -> Parser (C.PolicyId, C.AssetName, C.Quantity)
          parseAsset o = do
              policyId <- o .: "policy_id"
              assetName <- o .: "asset_name"
              qty <- o .: "quantity"
              pure (policyId, assetName, qty)

instance ToJSON ExportTxInput where
    toJSON ExportTxInput{etxiId, etxiTxIx, etxiLovelaceQuantity, etxiDatumHash, etxiAssets, etxiAddress} =
        object
            [ "id" .= etxiId
            , "index" .= etxiTxIx
            , "address" .= C.serialiseAddress etxiAddress
            , "amount" .= object ["quantity" .= etxiLovelaceQuantity, "unit" .= ("lovelace" :: String)]
            , "datum" .= etxiDatumHash
            , "assets" .= fmap (\(p, a, q) -> object ["policy_id" .= p, "asset_name" .= a, "quantity" .= q]) etxiAssets
            ]

export
    :: Params
    -> UnbalancedTx
    -> Either CardanoLedgerError ExportTx
export params (UnbalancedCardanoTx tx utxos) =
    let fromCardanoTx ctx = do
            utxo <- fromPlutusIndex $ P.UtxoIndex utxos
            makeTransactionBody params utxo ctx
     in ExportTx
        <$> fmap (C.makeSignedTransaction []) (fromCardanoTx tx)
        <*> first Right (mkInputs utxos)
        <*> pure []

mkInputs :: Map Plutus.TxOutRef P.TxOut -> Either CardanoAPI.ToCardanoError [ExportTxInput]
mkInputs = traverse (uncurry toExportTxInput) . Map.toList

toExportTxInput :: Plutus.TxOutRef -> P.TxOut -> Either CardanoAPI.ToCardanoError ExportTxInput
toExportTxInput Plutus.TxOutRef{Plutus.txOutRefId, Plutus.txOutRefIdx} txOut = do
    let cardanoValue = P.txOutValue txOut
    let otherQuantities = mapMaybe (\case { (C.AssetId policyId assetName, quantity) -> Just (policyId, assetName, quantity); _ -> Nothing }) $ C.valueToList cardanoValue
    ExportTxInput
        <$> CardanoAPI.toCardanoTxId txOutRefId
        <*> pure (C.TxIx $ fromInteger txOutRefIdx)
        <*> pure (P.txOutAddress txOut)
        <*> pure (C.selectLovelace cardanoValue)
        <*> sequence (CardanoAPI.toCardanoScriptDataHash <$> P.txOutDatumHash txOut)
        <*> pure otherQuantities
