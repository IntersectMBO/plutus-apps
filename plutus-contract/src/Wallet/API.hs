{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

{-|

Mock wallet implementation

-}
module Wallet.API(
    WalletEffect,
    submitTxn,
    ownPaymentPubKeyHash,
    ownPaymentPubKeyHashes,
    ownFirstPaymentPubKeyHash,
    ownAddresses,
    balanceTx,
    yieldUnbalancedTx,
    NodeClientEffect,
    publishTx,
    getClientSlot,
    getClientParams,
    PubKey(..),
    PubKeyHash(..),
    signTxAndSubmit,
    signTxAndSubmit_,
    payToAddress,
    payToAddress_,
    payToPaymentPublicKeyHash,
    payToPaymentPublicKeyHash_,
    Params(..),
    -- * Slot ranges
    Interval(..),
    Slot,
    SlotRange,
    width,
    defaultSlotRange,
    interval,
    singleton,
    isEmpty,
    always,
    member,
    before,
    after,
    contains,
    -- * Error handling
    Wallet.Error.WalletAPIError(..),
    Wallet.Error.throwInsufficientFundsError,
    Wallet.Error.throwOtherError,
    ) where

import Cardano.Node.Emulator.Internal.Node.Params (Params (..))
import Control.Monad (unless, void)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logWarn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Void (Void)
import Ledger (Address, CardanoTx, Interval (Interval, ivFrom, ivTo), PaymentPubKeyHash (PaymentPubKeyHash),
               PubKey (PubKey, getPubKey), PubKeyHash (PubKeyHash, getPubKeyHash), Slot, SlotRange, after, always,
               before, cardanoPubKeyHash, contains, interval, isEmpty, member, pubKeyHashAddress, singleton, width)
import Ledger.Tx.Constraints qualified as Constraints
import Ledger.Tx.Constraints.OffChain (adjustUnbalancedTx)
import Ledger.Tx.Constraints.ValidityInterval qualified as Interval
import Plutus.V1.Ledger.Value (Value)
import Wallet.Effects (NodeClientEffect, WalletEffect, balanceTx, getClientParams, getClientSlot, ownAddresses,
                       publishTx, submitTxn, walletAddSignature, yieldUnbalancedTx)
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg (AdjustingUnbalancedTx))
import Wallet.Error (WalletAPIError (NoPaymentPubKeyHashError, PaymentMkTxError, ToCardanoError))
import Wallet.Error qualified

{-# DEPRECATED ownPaymentPubKeyHash "Use ownFirstPaymentPubKeyHash, ownPaymentPubKeyHashes or ownAddresses instead" #-}

ownPaymentPubKeyHash ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    )
    => Eff effs PaymentPubKeyHash
ownPaymentPubKeyHash = ownFirstPaymentPubKeyHash

ownPaymentPubKeyHashes ::
    ( Member WalletEffect effs
    )
    => Eff effs [PaymentPubKeyHash]
ownPaymentPubKeyHashes = do
    addrs <- ownAddresses
    pure $ fmap PaymentPubKeyHash $ mapMaybe cardanoPubKeyHash $ NonEmpty.toList addrs

ownFirstPaymentPubKeyHash ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    )
    => Eff effs PaymentPubKeyHash
ownFirstPaymentPubKeyHash = do
    pkhs <- ownPaymentPubKeyHashes
    case pkhs of
      []      -> throwError NoPaymentPubKeyHashError
      (pkh:_) -> pure pkh

-- | Transfer some funds to an address, returning the transaction that was submitted.
--
--  Note: Due to a constraint in the Cardano ledger, each tx output must have a
--  minimum amount of Ada. Therefore, the funds to transfer will be adjusted
--  to satisfy that constraint. See 'adjustUnbalancedTx'.
payToAddress ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    , Member (LogMsg Text) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => Params -> SlotRange -> Value -> Address -> Eff effs CardanoTx
payToAddress params range v addr = do
    pkh <- ownFirstPaymentPubKeyHash
    let constraints = Constraints.mustPayToAddress addr v
                   <> Constraints.mustValidateInSlotRange (Interval.fromPlutusInterval range)
                   <> Constraints.mustBeSignedBy pkh
    utx <- either (throwError . PaymentMkTxError)
                  pure
                  (Constraints.mkTxWithParams @Void params mempty constraints)
    let (missingAdaCosts, adjustedUtx) = adjustUnbalancedTx (emulatorPParams params) utx
    logDebug $ AdjustingUnbalancedTx missingAdaCosts
    unless (utx == adjustedUtx) $
      logWarn @Text $ "Wallet.API.payToPublicKeyHash: "
                   <> "Adjusted a transaction output value which has less than the minimum amount of Ada."
    balancedTx <- balanceTx adjustedUtx
    either throwError signTxAndSubmit balancedTx

-- | Transfer some funds to an address.
payToAddress_ ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    , Member (LogMsg Text) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => Params -> SlotRange -> Value -> Address -> Eff effs ()
payToAddress_ params range v addr = void $ payToAddress params range v addr

-- | Transfer some funds to an address locked by a public key, returning the
--   transaction that was submitted.
--
--  Note: Due to a constraint in the Cardano ledger, each tx output must have a
--  minimum amount of Ada. Therefore, the funds to transfer will be adjusted
--  to satisfy that constraint. See 'adjustUnbalancedTx'.
payToPaymentPublicKeyHash ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    , Member (LogMsg Text) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => Params -> SlotRange -> Value -> PaymentPubKeyHash -> Eff effs CardanoTx
payToPaymentPublicKeyHash params range v pkh = payToAddress params range v (pubKeyHashAddress pkh Nothing)

-- | Transfer some funds to an address locked by a public key.
payToPaymentPublicKeyHash_ ::
    ( Member WalletEffect effs
    , Member (Error WalletAPIError) effs
    , Member (LogMsg Text) effs
    , Member (LogMsg RequestHandlerLogMsg) effs
    )
    => Params -> SlotRange -> Value -> PaymentPubKeyHash -> Eff effs ()
payToPaymentPublicKeyHash_ params r v = void . payToPaymentPublicKeyHash params r v

-- | Add the wallet's signature to the transaction and submit it. Returns
--   the transaction with the wallet's signature.
signTxAndSubmit ::
    ( Member WalletEffect effs
    )
    => CardanoTx -> Eff effs CardanoTx
signTxAndSubmit t = do
    tx' <- walletAddSignature t
    submitTxn tx'
    pure tx'

-- | A version of 'signTxAndSubmit' that discards the result.
signTxAndSubmit_ ::
    ( Member WalletEffect effs
    )
    => CardanoTx -> Eff effs ()
signTxAndSubmit_ = void . signTxAndSubmit

-- | The default slot validity range for transactions.
defaultSlotRange :: SlotRange
defaultSlotRange = always
