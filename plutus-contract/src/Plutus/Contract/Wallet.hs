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
    ) where

import Cardano.Api qualified as C
import Control.Applicative ((<|>))
import Control.Monad ((>=>))
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
import Ledger (DCert, Redeemer, StakingCredential, txRedeemers)
import Ledger qualified (ScriptPurpose (..))
import Ledger qualified as P
import Ledger.Ada qualified as Ada
import Ledger.Constraints (mustPayToPubKey)
import Ledger.Constraints.OffChain (UnbalancedTx (unBalancedTxRequiredSignatories, unBalancedTxUtxoIndex),
                                    unBalancedTxTx)
import Ledger.Tx (CardanoTx, TxId (TxId), TxIn (..), TxOutRef, getCardanoTxInputs, txInRef)
import Ledger.Validation (CardanoLedgerError, fromPlutusIndex, makeTransactionBody)
import Ledger.Value (currencyMPSHash)
import Plutus.Contract.CardanoAPI qualified as CardanoAPI
import Plutus.Contract.Error (AsContractError (_OtherContractError))
import Plutus.Contract.Request qualified as Contract
import Plutus.Contract.Types (Contract)
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

-- | Get an unspent output belonging to the wallet.
getUnspentOutput :: AsContractError e => Contract w s e TxOutRef
getUnspentOutput = do
    ownPkh <- Contract.ownFirstPaymentPubKeyHash
    let constraints = mustPayToPubKey ownPkh (Ada.lovelaceValueOf 1)
    utx <- Contract.mkTxConstraints @Void mempty constraints
    tx <- Contract.adjustUnbalancedTx utx >>= Contract.balanceTx
    case getCardanoTxInputs tx of
        inp : _ -> pure $ txInRef inp
        []      -> throwing _OtherContractError "Balanced transaction has no inputs"

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
    deriving anyclass (OpenApi.ToSchema)

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
    :: P.Params
    -> UnbalancedTx
    -> Either CardanoLedgerError ExportTx
export params utx =
    let requiredSigners = Set.toList (unBalancedTxRequiredSignatories utx)
        fromCardanoTx ctx = do
            utxo <- fromPlutusIndex $ P.UtxoIndex (unBalancedTxUtxoIndex utx)
            makeTransactionBody params utxo ctx
     in ExportTx
        <$> fmap (C.makeSignedTransaction [])
                 (either
                     fromCardanoTx
                     (first Right . CardanoAPI.toCardanoTxBody params requiredSigners)
                     (unBalancedTxTx utx))
        <*> first Right (mkInputs (P.pNetworkId params) (unBalancedTxUtxoIndex utx))
        <*> either (const $ Right []) (Right . mkRedeemers) (unBalancedTxTx utx)

mkInputs :: C.NetworkId -> Map Plutus.TxOutRef P.TxOut -> Either CardanoAPI.ToCardanoError [ExportTxInput]
mkInputs networkId = traverse (uncurry (toExportTxInput networkId)) . Map.toList

toExportTxInput :: C.NetworkId -> Plutus.TxOutRef -> P.TxOut -> Either CardanoAPI.ToCardanoError ExportTxInput
toExportTxInput networkId Plutus.TxOutRef{Plutus.txOutRefId, Plutus.txOutRefIdx} txOut = do
    cardanoValue <- CardanoAPI.toCardanoValue (P.txOutValue txOut)
    let otherQuantities = mapMaybe (\case { (C.AssetId policyId assetName, quantity) -> Just (policyId, assetName, quantity); _ -> Nothing }) $ C.valueToList cardanoValue
    ExportTxInput
        <$> CardanoAPI.toCardanoTxId txOutRefId
        <*> pure (C.TxIx $ fromInteger txOutRefIdx)
        <*> CardanoAPI.toCardanoAddressInEra networkId (P.txOutAddress txOut)
        <*> pure (C.selectLovelace cardanoValue)
        <*> sequence (CardanoAPI.toCardanoScriptDataHash <$> P.txOutDatumHash txOut)
        <*> pure otherQuantities

-- TODO: Here there's hidden error of script DCert missing its redeemer - this just counts as no DCert. Don't know if bad.
mkRedeemers :: P.Tx -> [ExportTxRedeemer]
mkRedeemers = map (uncurry scriptPurposeToExportRedeemer) . Map.assocs . txRedeemers

scriptPurposeToExportRedeemer :: Ledger.ScriptPurpose -> Redeemer -> ExportTxRedeemer
scriptPurposeToExportRedeemer (Ledger.Spending ref)     rd = SpendingRedeemer {redeemerOutRef = ref, redeemer=rd}
scriptPurposeToExportRedeemer (Ledger.Minting cs)       rd = MintingRedeemer {redeemerPolicyId = currencyMPSHash cs, redeemer=rd}
scriptPurposeToExportRedeemer (Ledger.Rewarding cred)   rd = RewardingRedeemer {redeemerStakingCredential = cred, redeemer=rd}
scriptPurposeToExportRedeemer (Ledger.Certifying dcert) rd = CertifyingRedeemer {redeemerDCert = dcert, redeemer=rd}
