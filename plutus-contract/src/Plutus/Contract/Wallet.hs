{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts   #-}
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
    , getUnspentOutput
    , WAPI.signTxAndSubmit
    -- * Exporting transactions
    , ExportTx(..)
    , ExportTxInput(..)
    , export
    ) where

import qualified Cardano.Api                 as C
import qualified Cardano.Api.Shelley         as C
import           Control.Monad               (join, (>=>))
import           Control.Monad.Error.Lens    (throwing)
import           Control.Monad.Freer         (Eff, Member)
import           Control.Monad.Freer.Error   (Error, throwError)
import           Data.Aeson                  (ToJSON (..), Value (String), object, (.=))
import qualified Data.Aeson.Extras           as Aeson.Extras
import qualified Data.Aeson.Extras           as JSON
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (mapMaybe)
import qualified Data.Set                    as Set
import           Data.Typeable               (Typeable)
import           Data.Void                   (Void)
import           GHC.Generics                (Generic)
import qualified Ledger                      as Plutus
import qualified Ledger.Ada                  as Ada
import           Ledger.Constraints          (mustPayToPubKey)
import           Ledger.Constraints.OffChain (UnbalancedTx (..), mkTx)
import           Ledger.Tx                   (CardanoTx, TxOutRef, getCardanoTxInputs, txInRef)
import qualified Plutus.Contract.CardanoAPI  as CardanoAPI
import qualified Plutus.Contract.Request     as Contract
import           Plutus.Contract.Types       (Contract (..))
import           Plutus.V1.Ledger.Scripts    (MintingPolicyHash)
import qualified PlutusTx
import qualified Wallet.API                  as WAPI
import           Wallet.Effects              (WalletEffect, balanceTx)
import           Wallet.Emulator.Error       (WalletAPIError)
import           Wallet.Types                (AsContractError (_ConstraintResolutionError, _OtherError))

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
    ownPK <- Contract.ownPubKeyHash
    let constraints = mustPayToPubKey ownPK (Ada.lovelaceValueOf 1)
    utx <- either (throwing _ConstraintResolutionError) pure (mkTx @Void mempty constraints)
    tx <- Contract.balanceTx utx
    case Set.lookupMin (getCardanoTxInputs tx) of
        Just inp -> pure $ txInRef inp
        Nothing  -> throwing _OtherError "Balanced transaction has no inputs"

data ExportTxRedeemerPurpose = Spending | Minting | Rewarding

instance ToJSON ExportTxRedeemerPurpose where
    toJSON = \case
        Spending  -> String "spending"
        Minting   -> String "minting"
        Rewarding -> String "rewarding"

data ExportTxRedeemer =
    SpendingRedeemer{ redeemer:: Plutus.Redeemer, redeemerOutRef :: TxOutRef }
    | MintingRedeemer { redeemer:: Plutus.Redeemer, redeemerPolicyId :: MintingPolicyHash }
    deriving stock (Generic, Typeable)

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
    deriving stock (Generic, Typeable)

data ExportTxInput =
    ExportTxInput
        { etxiId               :: C.TxId
        , etxiTxIx             :: C.TxIx
        , etxiAddress          :: C.AddressInEra C.AlonzoEra
        , etxiLovelaceQuantity :: C.Lovelace
        , etxiDatumHash        :: C.Hash C.ScriptData
        , etxiAssets           :: [(C.PolicyId, C.AssetName, C.Quantity)]
        }

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

-- IMPORTANT: The JSON produced here needs to match the schema expected by
-- https://input-output-hk.github.io/cardano-wallet/api/edge/#operation/balanceTransaction
instance ToJSON ExportTx where
    toJSON ExportTx{partialTx, lookups, redeemers} =
        object
            [ "transaction" .= Aeson.Extras.encodeByteString (C.serialiseToCBOR partialTx)
            , "inputs"      .= lookups
            , "redeemers"   .= redeemers
            ]

export :: C.ProtocolParameters -> C.NetworkId -> UnbalancedTx -> Either CardanoAPI.ToCardanoError ExportTx
export params networkId UnbalancedTx{unBalancedTxTx, unBalancedTxUtxoIndex, unBalancedTxRequiredSignatories} =
    let requiredSigners = fst <$> Map.toList unBalancedTxRequiredSignatories in
    ExportTx
        <$> mkPartialTx requiredSigners params networkId unBalancedTxTx
        <*> mkInputs networkId unBalancedTxUtxoIndex
        <*> mkRedeemers unBalancedTxTx

mkPartialTx :: [WAPI.PubKeyHash] -> C.ProtocolParameters -> C.NetworkId -> Plutus.Tx -> Either CardanoAPI.ToCardanoError (C.Tx C.AlonzoEra)
mkPartialTx requiredSigners params networkId = fmap (C.makeSignedTransaction []) . CardanoAPI.toCardanoTxBody requiredSigners (Just params) networkId

mkInputs :: C.NetworkId -> Map Plutus.TxOutRef Plutus.TxOut -> Either CardanoAPI.ToCardanoError [ExportTxInput]
mkInputs networkId = traverse (uncurry (toExportTxInput networkId)) . Map.toList

toExportTxInput :: C.NetworkId -> Plutus.TxOutRef -> Plutus.TxOut -> Either CardanoAPI.ToCardanoError ExportTxInput
toExportTxInput networkId Plutus.TxOutRef{Plutus.txOutRefId, Plutus.txOutRefIdx} Plutus.TxOut{Plutus.txOutAddress, Plutus.txOutValue, Plutus.txOutDatumHash=Just dh} = do
    cardanoValue <- CardanoAPI.toCardanoValue txOutValue
    let otherQuantities = mapMaybe (\case { (C.AssetId policyId assetName, quantity) -> Just (policyId, assetName, quantity); _ -> Nothing }) $ C.valueToList cardanoValue
    ExportTxInput
        <$> CardanoAPI.toCardanoTxId txOutRefId
        <*> pure (C.TxIx $ fromInteger txOutRefIdx)
        <*> CardanoAPI.toCardanoAddress networkId txOutAddress
        <*> pure (C.selectLovelace cardanoValue)
        <*> CardanoAPI.toCardanoScriptDataHash dh
        <*> pure otherQuantities
toExportTxInput _ _ _ = Left CardanoAPI.PublicKeyInputsNotSupported

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
