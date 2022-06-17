{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Spec.Prism (tests, prismTrace, prop_Prism, prop_NoLock) where

import Control.Lens
import Control.Monad
import Data.Data
import Data.Map (Map)
import Data.Map qualified as Map
import Ledger.Ada qualified as Ada
import Ledger.Value (TokenName)
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.ContractModel as ContractModel

import Test.QuickCheck as QC hiding ((.&&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Plutus.Contracts.Prism hiding (mirror)
import Plutus.Contracts.Prism.Credential qualified as Credential
import Plutus.Contracts.Prism.Mirror qualified as C
import Plutus.Contracts.Prism.STO (STOData (..))
import Plutus.Contracts.Prism.STO qualified as STO
import Plutus.Contracts.Prism.Unlock qualified as C
import Plutus.Trace.Emulator qualified as Trace

user, mirror, issuer :: Wallet
user = w1
mirror = w2
issuer = w3

kyc :: TokenName
kyc = "KYC"

sto :: TokenName
sto = "STO token"

numTokens :: Integer
numTokens = 10_000_000

credential :: Credential
credential =
    Credential
        { credName = kyc
        , credAuthority = CredentialAuthority (mockWalletPaymentPubKeyHash mirror)
        }

stoSubscriber :: STOSubscriber
stoSubscriber =
    STOSubscriber
        { wCredential = credential
        , wSTOIssuer = mockWalletPaymentPubKeyHash issuer
        , wSTOTokenName = sto
        , wSTOAmount = numTokens
        }

stoData :: STOData
stoData =
    STOData
        { stoIssuer = mockWalletPaymentPubKeyHash issuer
        , stoTokenName = sto
        , stoCredentialToken = Credential.token credential
        }

-- | 'mirror' issues a KYC token to 'user', who then uses it in an STO transaction
prismTrace :: Trace.EmulatorTrace ()
prismTrace = do
    uhandle <- Trace.activateContractWallet user contract
    mhandle <- Trace.activateContractWallet mirror contract

    Trace.callEndpoint @"role" uhandle UnlockSTO
    Trace.callEndpoint @"role" mhandle Mirror
    _ <- Trace.waitNSlots 2

    -- issue a KYC credential to a user
    Trace.callEndpoint @"issue" mhandle CredentialOwnerReference{coTokenName=kyc, coOwner=user}
    _ <- Trace.waitNSlots 2

    -- participate in STO presenting the token
    Trace.callEndpoint @"sto" uhandle stoSubscriber
    void $ Trace.waitNSlots 2

-- * QuickCheck model

data STOState = STOReady | STOPending | STODone
    deriving (Eq, Ord, Show, Data)

data IssueState = NoIssue | Revoked | Issued
    deriving (Eq, Ord, Show, Data)

newtype PrismModel = PrismModel
    { _walletState :: Map Wallet (IssueState, STOState)
    }
    deriving (Show, Data)

makeLenses 'PrismModel

walletStatus :: Wallet -> Lens' PrismModel (IssueState, STOState)
walletStatus w = walletState . at w . non (NoIssue, STOReady)

isIssued :: Wallet -> Lens' PrismModel IssueState
isIssued w = walletStatus w . _1

stoState :: Wallet -> Lens' PrismModel STOState
stoState w = walletStatus w . _2

doRevoke :: IssueState -> IssueState
doRevoke NoIssue = NoIssue
doRevoke Revoked = Revoked
doRevoke Issued  = Revoked

waitSlots :: Integer
waitSlots = 9

users :: [Wallet]
users = [user, w4]

deriving instance Eq   (ContractInstanceKey PrismModel w s e params)
deriving instance Show (ContractInstanceKey PrismModel w s e params)

instance ContractModel PrismModel where

    data Action PrismModel = Issue Wallet | Revoke Wallet | Call Wallet
        deriving (Eq, Show, Data)

    data ContractInstanceKey PrismModel w s e params where
        MirrorH  ::           ContractInstanceKey PrismModel () C.MirrorSchema            C.MirrorError ()
        UserH    :: Wallet -> ContractInstanceKey PrismModel () C.STOSubscriberSchema     C.UnlockError ()

    arbitraryAction _ = QC.oneof [genUser Revoke, genUser Issue,
                                  genUser Call]
        where genUser f = f <$> QC.elements users

    initialState = PrismModel { _walletState = Map.empty }

    initialInstances = StartContract MirrorH () : ((`StartContract` ()) . UserH <$> users)

    instanceWallet MirrorH   = mirror
    instanceWallet (UserH w) = w

    instanceContract _ MirrorH _ = C.mirror
    instanceContract _ UserH{} _ = C.subscribeSTO

    precondition s (Issue w) = (s ^. contractState . isIssued w) /= Issued  -- Multiple Issue (without Revoke) breaks the contract
    precondition _ _         = True

    nextState cmd = do
        wait waitSlots
        case cmd of
            Revoke w  -> do
              isIssued w %= doRevoke
            Issue w   -> do
              wait 1
              isIssued w .= Issued
            Call w    -> do
              iss  <- (== Issued)   <$> viewContractState (isIssued w)
              pend <- (== STOReady) <$> viewContractState (stoState w)
              when (iss && pend) $ do
                transfer w issuer (Ada.lovelaceValueOf numTokens)
                let stoValue = STO.coins stoData numTokens
                mint stoValue
                deposit w stoValue

    perform handle _ _ cmd = case cmd of
        Issue w   -> wrap $ delay 1 >> Trace.callEndpoint @"issue"   (handle MirrorH) CredentialOwnerReference{coTokenName=kyc, coOwner=w}
        Revoke w  -> wrap $ Trace.callEndpoint @"revoke"             (handle MirrorH) CredentialOwnerReference{coTokenName=kyc, coOwner=w}
        Call w    -> wrap $ Trace.callEndpoint @"sto"                (handle $ UserH w) stoSubscriber
        where                     -- v Wait a generous amount of blocks between calls
            wrap m   = () <$ m <* delay waitSlots

    shrinkAction _ _ = []

    monitoring (_, s) _ = counterexample (show s)

finalPredicate :: ModelState PrismModel -> TracePredicate
finalPredicate _ =
    assertNotDone @_ @() @C.STOSubscriberSchema     C.subscribeSTO      (Trace.walletInstanceTag user)              "User stopped"               .&&.
    assertNotDone @_ @() @C.MirrorSchema            C.mirror            (Trace.walletInstanceTag mirror)            "Mirror stopped"



prop_Prism :: Actions PrismModel -> Property
prop_Prism = propRunActions @PrismModel finalPredicate

-- | The Prism contract does not lock any funds.
noLockProof :: NoLockedFundsProof PrismModel
noLockProof = defaultNLFP

prop_NoLock :: Property
prop_NoLock = checkNoLockedFundsProof noLockProof

tests :: TestTree
tests = testGroup "PRISM"
    [ checkPredicate "withdraw"
        (assertNotDone contract (Trace.walletInstanceTag user) "User stopped"
        .&&. walletFundsChange issuer (Ada.lovelaceValueOf numTokens)
        .&&. walletFundsChange user (Ada.lovelaceValueOf (negate numTokens) <> STO.coins stoData numTokens)
        )
        prismTrace
    , testProperty "QuickCheck property" $
        withMaxSuccess 15 prop_Prism
    ]
