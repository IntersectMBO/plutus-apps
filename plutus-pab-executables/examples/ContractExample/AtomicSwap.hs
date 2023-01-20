{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StrictData         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module ContractExample.AtomicSwap(
    AtomicSwapParams(..),
    AtomicSwapError(..),
    AsAtomicSwapError(..),
    AtomicSwapSchema,
    atomicSwap
    ) where

import Control.Lens (makeClassyPrisms)
import Control.Monad (void)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

import Ledger (POSIXTime, PaymentPubKeyHash)
import Plutus.Contract (AsContractError (_ContractError), ContractError, Endpoint, Promise, awaitTxConfirmed, endpoint,
                        mapError, ownFirstPaymentPubKeyHash, throwError)
import Plutus.Contracts.Escrow (EscrowParams (..))
import Plutus.Contracts.Escrow qualified as Escrow
import Plutus.Script.Utils.Value (CurrencySymbol, TokenName, Value)
import Plutus.Script.Utils.Value qualified as Value
import Wallet.Emulator.Wallet (Wallet, mockWalletPaymentPubKeyHash)

-- | Describes an exchange of two
--   'Value' amounts between two parties
--   identified by public keys
data AtomicSwapParams =
    AtomicSwapParams
        { ada          :: Value -- ^ The amount paid to the hash of 'party1'
        , currencyHash :: CurrencySymbol
        , tokenName    :: TokenName
        , amount       :: Integer
        , party1       :: Wallet -- ^ The first party in the atomic swap
        , party2       :: Wallet -- ^ The second party in the atomic swap
        , deadline     :: POSIXTime -- ^ Last time in which the swap can be executed.
        }
        deriving stock (Eq, Show, Generic)
        deriving anyclass (ToJSON, FromJSON)

mkValue1 :: AtomicSwapParams -> Value
mkValue1 = ada

mkValue2 :: AtomicSwapParams -> Value
mkValue2 AtomicSwapParams{currencyHash, tokenName, amount} =
    Value.singleton currencyHash tokenName amount

mkEscrowParams :: AtomicSwapParams -> EscrowParams t
mkEscrowParams p@AtomicSwapParams{party1,party2,deadline} =
    let pubKey1 = mockWalletPaymentPubKeyHash party1
        pubKey2 = mockWalletPaymentPubKeyHash party2
        value1 = mkValue1 p
        value2 = mkValue2 p
    in EscrowParams
        { escrowDeadline = deadline
        , escrowTargets =
                [ Escrow.payToPaymentPubKeyTarget pubKey1 value1
                , Escrow.payToPaymentPubKeyTarget pubKey2 value2
                ]
        }

type AtomicSwapSchema = Endpoint "Atomic swap" AtomicSwapParams

data AtomicSwapError =
    EscrowError Escrow.EscrowError
    | OtherAtomicSwapError ContractError
    | NotInvolvedError PaymentPubKeyHash AtomicSwapParams
    -- ^ When the wallet's public key doesn't match either of the two keys specified in the 'AtomicSwapParams'
    deriving (Show, Generic, ToJSON, FromJSON)

makeClassyPrisms ''AtomicSwapError
instance AsContractError AtomicSwapError where
    _ContractError = _OtherAtomicSwapError

-- | Perform the atomic swap. Needs to be called by both of the two parties
--   involved.
atomicSwap :: Promise () AtomicSwapSchema AtomicSwapError ()
atomicSwap = endpoint @"Atomic swap" $ \p -> do
    let value1 = mkValue1 p
        value2 = mkValue2 p
        params = mkEscrowParams p

        go pkh
            | pkh == mockWalletPaymentPubKeyHash (party1 p) =
                -- there are two paying transactions and one redeeming transaction.
                -- The redeeming tx is submitted by party 1.
                -- TODO: Change 'payRedeemRefund' to check before paying into the
                -- address, so that the last paying transaction can also be the
                -- redeeming transaction.
                void $ mapError EscrowError (Escrow.payRedeemRefund params value2)
            | pkh == mockWalletPaymentPubKeyHash (party2 p) =
                void $ mapError EscrowError (Escrow.pay (Escrow.typedValidator params) params value1) >>= awaitTxConfirmed
            | otherwise = throwError (NotInvolvedError pkh p)

    ownFirstPaymentPubKeyHash >>= go
