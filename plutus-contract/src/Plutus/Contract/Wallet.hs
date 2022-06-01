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
{-# LANGUAGE TypeApplications   #-}
-- | Turn 'UnbalancedTx' values into transactions using the
--   wallet API.
module Plutus.Contract.Wallet(
      balanceTx
    , handleTx
    , yieldUnbalancedTx
    , getUnspentOutput
    , WAPI.signTxAndSubmit
    -- * Exporting transactions
    , ExportTx(..)
    , ExportTxInput(..)
    , ExportTxRedeemer(..)
    , export
    , finalize
    ) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Applicative ((<|>))
import Control.Lens ((&), (.~), (^.))
import Control.Monad (join, (>=>))
import Control.Monad.Error.Lens (throwing)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error, throwError)
import Data.Aeson (FromJSON (parseJSON), Object, ToJSON (toJSON), Value (String), object, withObject, (.:), (.=))
import Data.Aeson.Extras qualified as JSON
import Data.Aeson.Types (Parser, parseFail)
import Data.Bifunctor (first)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.OpenApi qualified as OpenApi
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger qualified as Plutus
import Ledger.Ada qualified as Ada
import Ledger.Constraints (mustPayToPubKey)
import Ledger.Constraints.OffChain (UnbalancedTx (UnbalancedTx, unBalancedTxRequiredSignatories, unBalancedTxTx, unBalancedTxUtxoIndex),
                                    mkTx)
import Ledger.Constraints.OffChain qualified as U
import Ledger.TimeSlot (SlotConfig, posixTimeRangeToContainedSlotRange)
import Ledger.Tx (CardanoTx, TxOutRef, getCardanoTxInputs, txInRef)
import Plutus.Contract.CardanoAPI qualified as CardanoAPI
import Plutus.Contract.Error (AsContractError (_ConstraintResolutionContractError, _OtherContractError))
import Plutus.Contract.Request qualified as Contract
import Plutus.Contract.Types (Contract)
import Plutus.Script.Utils.V1.Scripts qualified as Plutus
import Plutus.V1.Ledger.Api qualified as Plutus
import Plutus.V1.Ledger.Scripts (MintingPolicyHash)
import Plutus.V1.Ledger.TxId (TxId (TxId))
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

-- | Get an unspent output belonging to the wallet.
getUnspentOutput :: AsContractError e => Contract w s e TxOutRef
getUnspentOutput = do
    ownPkh <- Contract.ownPaymentPubKeyHash
    let constraints = mustPayToPubKey ownPkh (Ada.lovelaceValueOf 1)
    utx <- either (throwing _ConstraintResolutionContractError) pure (mkTx @Void mempty constraints)
    tx <- Contract.adjustUnbalancedTx utx >>= Contract.balanceTx
    case Set.lookupMin (getCardanoTxInputs tx) of
        Just inp -> pure $ txInRef inp
        Nothing  -> throwing _OtherContractError "Balanced transaction has no inputs"

data ExportTxRedeemerPurpose = Spending | Minting | Rewarding

instance ToJSON ExportTxRedeemerPurpose where
    toJSON = \case
        Spending  -> String "spending"
        Minting   -> String "minting"
        Rewarding -> String "rewarding"

data ExportTxRedeemer =
    SpendingRedeemer{ redeemer:: Plutus.Redeemer, redeemerOutRef :: TxOutRef }
    | MintingRedeemer { redeemer:: Plutus.Redeemer, redeemerPolicyId :: MintingPolicyHash }
    deriving stock (Eq, Show, Generic, Typeable)
    deriving anyclass (OpenApi.ToSchema)

instance FromJSON ExportTxRedeemer where
    parseJSON v = parseSpendingRedeemer v <|> parseMintingRedeemer v

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

parseRedeemerData :: Object -> Parser Plutus.Redeemer
parseRedeemerData o =
    fmap (\(JSON.JSONViaSerialise d) -> Plutus.Redeemer $ PlutusTx.dataToBuiltinData d)
         (o .: "data")

instance ToJSON ExportTxRedeemer where
    toJSON SpendingRedeemer{redeemer=Plutus.Redeemer dt, redeemerOutRef=Plutus.TxOutRef{Plutus.txOutRefId, Plutus.txOutRefIdx}} =
        object ["purpose" .= Spending, "data" .= JSON.JSONViaSerialise (PlutusTx.builtinDataToData dt), "input" .= object ["id" .= Plutus.getTxId txOutRefId, "index" .= txOutRefIdx]]
    toJSON MintingRedeemer{redeemer=Plutus.Redeemer dt, redeemerPolicyId} =
        object ["purpose" .= Minting, "data" .= JSON.JSONViaSerialise (PlutusTx.builtinDataToData dt), "policy_id" .= redeemerPolicyId]

-- | Partial transaction that can be balanced by the wallet backend.
data ExportTx =
        ExportTx
            { partialTx :: C.Tx C.AlonzoEra -- ^ The transaction itself
            , lookups   :: [ExportTxInput] -- ^ The tx outputs for all inputs spent by the partial tx
            , redeemers :: [ExportTxRedeemer]
            }
    deriving stock (Eq, Show, Generic, Typeable)
    deriving anyclass (OpenApi.ToSchema)

instance FromJSON ExportTx where
  parseJSON = withObject "ExportTx" $ \v -> ExportTx
      <$> parsePartialTx v
      <*> v .: "inputs"
      <*> v .: "redeemers"
    where
      parsePartialTx v =
        v .: "transaction" >>= \t ->
          either parseFail pure $ JSON.tryDecode t
                              >>= (first show . C.deserialiseFromCBOR (C.AsTx C.AsAlonzoEra))

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
        , etxiAddress          :: C.AddressInEra C.AlonzoEra
        , etxiLovelaceQuantity :: C.Lovelace
        , etxiDatumHash        :: Maybe (C.Hash C.ScriptData)
        , etxiAssets           :: [(C.PolicyId, C.AssetName, C.Quantity)]
        }
    deriving stock (Eq, Show, Generic)
    deriving anyclass (OpenApi.ToSchema)

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
              let deserialisedAddr = C.deserialiseAddress (C.AsAddressInEra C.AsAlonzoEra) addressField
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
    :: C.ProtocolParameters
    -> C.NetworkId
    -> SlotConfig
    -> UnbalancedTx
    -> Either CardanoAPI.ToCardanoError ExportTx
export params networkId slotConfig utx =
    let UnbalancedTx
            { unBalancedTxTx
            , unBalancedTxUtxoIndex
            , unBalancedTxRequiredSignatories
            } = finalize slotConfig utx
        requiredSigners = Set.toList unBalancedTxRequiredSignatories
     in ExportTx
        <$> mkPartialTx requiredSigners params networkId unBalancedTxTx
        <*> mkInputs networkId unBalancedTxUtxoIndex
        <*> mkRedeemers unBalancedTxTx

finalize :: SlotConfig -> UnbalancedTx -> UnbalancedTx
finalize slotConfig utx =
     utx & U.tx
         . Plutus.validRange
         .~ posixTimeRangeToContainedSlotRange slotConfig (utx ^. U.validityTimeRange)

mkPartialTx
    :: [Plutus.PaymentPubKeyHash]
    -> C.ProtocolParameters
    -> C.NetworkId
    -> Plutus.Tx
    -> Either CardanoAPI.ToCardanoError (C.Tx C.AlonzoEra)
mkPartialTx requiredSigners params networkId =
      fmap (C.makeSignedTransaction [])
    . CardanoAPI.toCardanoTxBody requiredSigners (Just params) networkId

mkInputs :: C.NetworkId -> Map Plutus.TxOutRef Plutus.TxOut -> Either CardanoAPI.ToCardanoError [ExportTxInput]
mkInputs networkId = traverse (uncurry (toExportTxInput networkId)) . Map.toList

toExportTxInput :: C.NetworkId -> Plutus.TxOutRef -> Plutus.TxOut -> Either CardanoAPI.ToCardanoError ExportTxInput
toExportTxInput networkId Plutus.TxOutRef{Plutus.txOutRefId, Plutus.txOutRefIdx} Plutus.TxOut{Plutus.txOutAddress, Plutus.txOutValue, Plutus.txOutDatumHash} = do
    cardanoValue <- CardanoAPI.toCardanoValue txOutValue
    let otherQuantities = mapMaybe (\case { (C.AssetId policyId assetName, quantity) -> Just (policyId, assetName, quantity); _ -> Nothing }) $ C.valueToList cardanoValue
    ExportTxInput
        <$> CardanoAPI.toCardanoTxId txOutRefId
        <*> pure (C.TxIx $ fromInteger txOutRefIdx)
        <*> CardanoAPI.toCardanoAddress networkId txOutAddress
        <*> pure (C.selectLovelace cardanoValue)
        <*> sequence (CardanoAPI.toCardanoScriptDataHash <$> txOutDatumHash)
        <*> pure otherQuantities

mkRedeemers :: Plutus.Tx -> Either CardanoAPI.ToCardanoError [ExportTxRedeemer]
mkRedeemers tx = (++) <$> mkSpendingRedeemers tx <*> mkMintingRedeemers tx

mkSpendingRedeemers :: Plutus.Tx -> Either CardanoAPI.ToCardanoError [ExportTxRedeemer]
mkSpendingRedeemers Plutus.Tx{Plutus.txInputs} = fmap join (traverse extract $ Set.toList txInputs) where
    extract Plutus.TxIn{Plutus.txInType=Just (Plutus.ConsumeScriptAddress _ redeemer _), Plutus.txInRef} =
        pure [SpendingRedeemer{redeemer, redeemerOutRef=txInRef}]
    extract _ = pure []

mkMintingRedeemers :: Plutus.Tx -> Either CardanoAPI.ToCardanoError [ExportTxRedeemer]
mkMintingRedeemers Plutus.Tx{Plutus.txRedeemers, Plutus.txMintScripts} = traverse extract $ Map.toList txRedeemers where
    indexedMintScripts = Map.fromList $ zip [0..] $ Set.toList txMintScripts
    extract (Plutus.RedeemerPtr Plutus.Mint idx, redeemer) = do
        redeemerPolicyId <- maybe (Left CardanoAPI.MissingMintingPolicy) (Right . Plutus.mintingPolicyHash) (Map.lookup idx indexedMintScripts)
        pure MintingRedeemer{redeemer, redeemerPolicyId}
    extract (Plutus.RedeemerPtr tag _, _) = Left (CardanoAPI.ScriptPurposeNotSupported tag)
