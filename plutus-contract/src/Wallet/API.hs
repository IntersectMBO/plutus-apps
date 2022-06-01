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

import Control.Monad (unless, void)
import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.Error (Error, throwError)
import Control.Monad.Freer.Extras.Log (LogMsg, logDebug, logWarn)
import Data.Default (Default (def))
import Data.Text (Text)
import Data.Void (Void)
import Ledger (CardanoTx, Interval (Interval, ivFrom, ivTo), Params (..), PaymentPubKeyHash, PubKey (PubKey, getPubKey),
               PubKeyHash (PubKeyHash, getPubKeyHash), Slot, SlotRange, Value, after, always, before, contains,
               interval, isEmpty, member, singleton, width)
import Ledger.Constraints qualified as Constraints
import Ledger.Constraints.OffChain (adjustUnbalancedTx)
import Ledger.TimeSlot qualified as TimeSlot
import Wallet.Effects (NodeClientEffect, WalletEffect, balanceTx, getClientParams, getClientSlot, ownPaymentPubKeyHash,
                       publishTx, submitTxn, walletAddSignature, yieldUnbalancedTx)
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg (AdjustingUnbalancedTx))
import Wallet.Error (WalletAPIError (PaymentMkTxError, ToCardanoError))
import Wallet.Error qualified

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
payToPaymentPublicKeyHash params range v pk = do
    let constraints = Constraints.mustPayToPubKey pk v
                   <> Constraints.mustValidateIn (TimeSlot.slotRangeToPOSIXTimeRange def range)
    utx <- either (throwError . PaymentMkTxError)
                  pure
                  (Constraints.mkTx @Void mempty constraints)
    (missingAdaCosts, adjustedUtx) <- either (throwError . ToCardanoError) pure (adjustUnbalancedTx params utx)
    logDebug $ AdjustingUnbalancedTx missingAdaCosts
    unless (utx == adjustedUtx) $
      logWarn @Text $ "Wallet.API.payToPublicKeyHash: "
                   <> "Adjusted a transaction output value which has less than the minimum amount of Ada."
    balancedTx <- balanceTx adjustedUtx
    either throwError signTxAndSubmit balancedTx

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
