{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
-- | Interfacing with the wallet (for making payments)
module Plutus.Trace.Effects.EmulatedWalletAPI(
    EmulatedWalletAPI(..)
    , liftWallet
    , payToWallet
    , handleEmulatedWalletAPI
    ) where

import Control.Monad.Freer (Eff, Member, subsume, type (~>))
import Control.Monad.Freer.Error (Error)
import Control.Monad.Freer.Extras (raiseEnd)
import Control.Monad.Freer.Extras.Log (LogMsg)
import Control.Monad.Freer.TH (makeEffect)
import Data.Default (def)
import Data.Text (Text)
import Ledger qualified
import Ledger.Tx (TxId, getCardanoTxId)
import Plutus.Script.Utils.Value (Value)
import Wallet.API (WalletAPIError, defaultSlotRange, payToAddress)
import Wallet.Effects (WalletEffect)
import Wallet.Emulator qualified as EM
import Wallet.Emulator.LogMessages (RequestHandlerLogMsg)
import Wallet.Emulator.MultiAgent (MultiAgentEffect, walletAction)
import Wallet.Emulator.Wallet (Wallet)

data EmulatedWalletAPI r where
    LiftWallet :: Wallet -> Eff '[WalletEffect, Error WalletAPIError, LogMsg Text, LogMsg RequestHandlerLogMsg] a -> EmulatedWalletAPI a

makeEffect ''EmulatedWalletAPI

-- | Make a payment from one wallet to another
payToWallet ::
    forall effs.
    ( Member EmulatedWalletAPI effs
    )
    => Wallet
    -> Wallet
    -> Value
    -> Eff effs TxId
payToWallet source target amount = do
    ctx <- liftWallet source
         $ payToAddress def defaultSlotRange amount (Ledger.toPlutusAddress $ EM.mockWalletAddress target)
    pure $ getCardanoTxId ctx

-- | Handle the 'EmulatedWalletAPI' effect using the emulator's
--   'MultiAgent' effect.
handleEmulatedWalletAPI ::
    ( Member MultiAgentEffect effs
    )
    => EmulatedWalletAPI
    ~> Eff effs
handleEmulatedWalletAPI = \case
    LiftWallet w action ->
        walletAction w
            $ subsume
            $ subsume
            $ subsume
            $ subsume
            $ raiseEnd action
