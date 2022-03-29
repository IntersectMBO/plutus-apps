{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-redundant-constraints #-}
module Spec.Uniswap where

import Control.Arrow
import Control.Exception hiding (assert)
import Control.Lens hiding (elements)
import Control.Monad
import Plutus.Contract
import Plutus.Contract as Contract hiding (throwError)
import Plutus.Contract.Test hiding (not)
import Plutus.Contract.Test.Certification
import Plutus.Contract.Test.ContractModel
import Plutus.Contract.Test.ContractModel.Symbolics
import Plutus.Contract.Test.Coverage
import Plutus.Contracts.Currency qualified as Currency
import Plutus.Contracts.Uniswap hiding (pools, setupTokens, tokenNames, wallets)
import Plutus.Contracts.Uniswap.Trace qualified as Uniswap
import Plutus.Trace.Emulator (EmulatorRuntimeError (GenericError))
import Plutus.Trace.Emulator qualified as Trace
import PlutusTx.Coverage

import Ledger qualified as Ledger
import Ledger.Ada qualified as Ada
import Ledger.Value qualified as Value

import Data.Data
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Text qualified as Text

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String

import Data.Void

import Test.QuickCheck hiding ((.&&.))
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

import Data.Monoid (Last (..))
import Data.Semigroup qualified as Semigroup

import Ledger.Constraints

data PoolIndex = PoolIndex SymToken SymToken deriving (Show, Data)

poolIndex :: SymToken -> SymToken -> PoolIndex
poolIndex t1 t2 = PoolIndex (min t1 t2) (max t1 t2)

instance Eq PoolIndex where
  PoolIndex t1 t2 == PoolIndex t1' t2' = (min t1 t2, max t1 t2) == (min t1' t2', max t1' t2')

instance Ord PoolIndex where
  compare (PoolIndex t1 t2) (PoolIndex t1' t2') = compare (min t1 t2, max t1 t2) (min t1' t2', max t1' t2')

data PoolModel = PoolModel { _coinAAmount    :: Amount A
                           , _coinBAmount    :: Amount B
                           , _liquidities    :: Map Wallet (Amount Liquidity)
                           , _liquidityToken :: SymToken
                           } deriving (Ord, Eq, Show, Data)

data UniswapModel = UniswapModel { _uniswapToken       :: Maybe SymToken
                                 , _exchangeableTokens :: Set SymToken
                                 , _pools              :: Map PoolIndex PoolModel
                                 , _startedUserCode    :: Set Wallet
                                 } deriving (Show, Data)

makeLenses ''UniswapModel
makeLenses ''PoolModel

open :: Getter PoolModel Bool
open = to $ \ p -> p ^. coinAAmount > 0 -- If one is bigger than zero the other one is too

prop_Uniswap :: Actions UniswapModel -> Property
prop_Uniswap = propRunActions_

deriving instance Eq (ContractInstanceKey UniswapModel w s e params)
deriving instance Show (ContractInstanceKey UniswapModel w s e params)

walletOf :: Action UniswapModel -> Wallet
walletOf a = case a of
  SetupTokens             -> w1
  Start                   -> w1
  CreatePool w _ _ _ _    -> w
  AddLiquidity w _ _ _ _  -> w
  RemoveLiquidity w _ _ _ -> w
  PerformSwap w _ _ _     -> w
  ClosePool w _ _         -> w

hasPool :: ModelState UniswapModel -> SymToken -> SymToken -> Bool
hasPool s t1 t2 = isJust (s ^. contractState . pools . at (poolIndex t1 t2))

hasOpenPool :: ModelState UniswapModel -> SymToken -> SymToken -> Bool
hasOpenPool s t1 t2 = hasPool s t1 t2
                      && s ^. contractState . pools . at (poolIndex t1 t2) . to fromJust . open

liquidityOf :: ModelState UniswapModel -> Wallet -> SymToken -> SymToken -> Amount Liquidity
liquidityOf s w t1 t2 = sum $ s ^? contractState . pools . at (poolIndex t1 t2) . _Just . liquidities . at w . to sum

totalLiquidity :: ModelState UniswapModel -> SymToken -> SymToken -> Amount Liquidity
totalLiquidity s t1 t2 = sum $ s ^? contractState . pools . at (poolIndex t1 t2) . _Just . liquidities . to sum

hasUniswapToken :: ModelState UniswapModel -> Bool
hasUniswapToken s = isJust $ s ^. contractState . uniswapToken

swapUnless :: Bool -> (a, a) -> (a, a)
swapUnless b (a, a') = if b then (a, a') else (a', a)

mkAmounts :: SymToken -> SymToken -> Integer -> Integer -> (Amount A, Amount B)
mkAmounts t1 t2 a1 a2 = Amount *** Amount $ swapUnless (t1 < t2) (a1, a2)

getAToken, getBToken :: SymToken -> SymToken -> SymToken
getAToken = min
getBToken = max

-- | Create some sample tokens and distribute them to
--   the emulated wallets
setupTokens :: Contract (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
setupTokens = do
    ownPK <- Contract.ownPaymentPubKeyHash
    cur   <- Currency.mintContract ownPK [(fromString tn, fromIntegral (length wallets) * amount) | tn <- tokenNames]
    let cs = Currency.currencySymbol cur
        v  = mconcat [Value.singleton cs (fromString tn) amount | tn <- tokenNames]

    forM_ wallets $ \w -> do
        let pkh = mockWalletPaymentPubKeyHash w
        when (pkh /= ownPK) $ do
            cs <- mkTxConstraints @Void mempty (mustPayToPubKey pkh v)
            submitTxConfirmed . adjustUnbalancedTx $ cs

    tell $ Just $ Semigroup.Last cur
  where
    amount = 1000000

wallets :: [Wallet]
wallets = take 9 knownWallets

tokenNames :: [String]
tokenNames = ["A", "B", "C", "D"]

instance ContractModel UniswapModel where
  data Action UniswapModel = SetupTokens
                           -- ^ Give some tokens to wallets `w1..w4`
                           | Start
                           -- ^ Start the system by creating a pool-factory
                           | CreatePool Wallet SymToken Integer SymToken Integer
                           -- ^ Create a liquidity pool
                           | AddLiquidity Wallet SymToken Integer SymToken Integer
                           -- ^ Amount of liquidity added for each token by the wallet
                           | PerformSwap Wallet SymToken SymToken Integer
                           -- ^ Swap the first token for the second token
                           | RemoveLiquidity Wallet SymToken SymToken Integer
                           -- ^ Amount of liquidity to cash in
                           | ClosePool Wallet SymToken SymToken
                           -- ^ Close a liquidity pool
                           deriving (Eq, Show, Data)

  data ContractInstanceKey UniswapModel w s e params where
    OwnerKey :: ContractInstanceKey UniswapModel (Last (Either Text.Text Uniswap)) EmptySchema ContractError ()
    SetupKey :: ContractInstanceKey UniswapModel (Maybe (Semigroup.Last Currency.OneShotCurrency)) Currency.CurrencySchema Currency.CurrencyError ()
    WalletKey :: Wallet -> ContractInstanceKey UniswapModel (Last (Either Text.Text UserContractState)) UniswapUserSchema Void SymToken

  initialInstances = []

  instanceWallet OwnerKey      = w1
  instanceWallet SetupKey      = w1
  instanceWallet (WalletKey w) = w

  instanceContract tokenSem key token = case key of
    OwnerKey    -> ownerEndpoint
    SetupKey    -> setupTokens
    WalletKey _ -> toContract . userEndpoints . Uniswap . Coin . tokenSem  $ token

  initialState = UniswapModel Nothing mempty mempty mempty

  arbitraryAction s =
    frequency $ [ (1, pure Start)
                , (1, pure SetupTokens) ] ++
                [ (3, createPool) | not . null $ s ^. contractState . exchangeableTokens ] ++
                [ (10, gen) | gen <- [createPool, add, swap, remove, close]
                           , not . null $ s ^. contractState . exchangeableTokens
                           , not . null $ s ^. contractState . pools ]
    where
      createPool = do
        w <- elements $ wallets \\ [w1]
        t1 <- elements $ s ^. contractState . exchangeableTokens . to Set.toList
        t2 <- elements $ s ^. contractState . exchangeableTokens . to (Set.delete t1) . to Set.toList
        a1 <- choose (1, 100)
        a2 <- choose (1, 100)
        return $ CreatePool w (getAToken t1 t2) a1 (getBToken t1 t2) a2

      add = do
        w <- elements $ wallets \\ [w1]
        PoolIndex t1 t2 <- elements $ s ^. contractState . pools . to Map.keys
        a1 <- choose (1, 100)
        a2 <- choose (1, 100)
        return $ AddLiquidity w (getAToken t1 t2) a1 (getBToken t1 t2) a2

      swap = do
        PoolIndex t1 t2 <- elements $ s ^. contractState . pools . to Map.keys
        w <- elements $ s ^. contractState . pools . at (poolIndex t1 t2) . to fromJust . liquidities . to Map.keys
        a <- choose (1, 100)
        (tA, tB) <- elements [(t1, t2), (t2, t1)]
        return $ PerformSwap w tA tB a

      remove = do
        idx@(PoolIndex t1 t2) <- elements $ s ^. contractState . pools . to Map.keys
        w <- elements . fold $ s ^? contractState . pools . at idx . _Just . liquidities . to (Map.filter (0<)) . to Map.keys
        a <- choose (1, sum $ s ^? contractState . pools . at idx . _Just . liquidities . at w . _Just . to unAmount)
        return $ RemoveLiquidity w (getAToken t1 t2) (getBToken t1 t2) a

      close = do
        w <- elements $ wallets \\ [w1]
        PoolIndex t1 t2 <- elements $ s ^. contractState . pools . to Map.keys
        return $ ClosePool w (getAToken t1 t2) (getBToken t1 t2)

  startInstances s act = case act of
    Start       -> [ StartContract OwnerKey () ]
    SetupTokens -> [ StartContract SetupKey () ]
    _           -> [ StartContract (WalletKey $ walletOf act) (fromJust $ s ^. contractState . uniswapToken)
                   | walletOf act `notElem` s ^. contractState . startedUserCode ]

  precondition s Start                        = not $ hasUniswapToken s
  precondition _ SetupTokens                  = True
  precondition s (CreatePool _ t1 a1 t2 a2)   = hasUniswapToken s
                                                && not (hasOpenPool s t1 t2)
                                                && t1 /= t2
                                                && 0 < a1
                                                && 0 < a2
  precondition s (AddLiquidity _ t1 a1 t2 a2) = hasOpenPool s t1 t2
                                                && t1 /= t2
                                                && 0 < a1
                                                && 0 < a2
  precondition s (PerformSwap _ t1 t2 a)      = hasOpenPool s t1 t2
                                                && t1 /= t2
                                                && 0 < a
  precondition s (RemoveLiquidity w t1 t2 a)  = hasOpenPool s t1 t2
                                                && t1 /= t2
                                                && Amount a <= liquidityOf s w t1 t2
                                                && Amount a < totalLiquidity s t1 t2
                                                && 0 < a
  precondition s (ClosePool w t1 t2)          = hasOpenPool s t1 t2
                                                && t1 /= t2
                                                && liquidityOf s w t1 t2 == totalLiquidity s t1 t2

  nextState act = case act of
    SetupTokens -> do
      -- Give 1000000 A, B, C, and D token to w1, w2, w3, w4
      -- The tokens will be given to each wallet in a UTxO that needs
      -- to have minAdaTxOut
      withdraw w1 $ Ada.toValue ((fromInteger . toInteger . length $ wallets) * Ledger.minAdaTxOut)
      -- Create the tokens
      ts <- forM tokenNames $ \t -> do
        tok <- createToken t
        mint (symAssetClassValue tok (toInteger $ length wallets * 1000000))
        return tok
      -- Give the tokens to the wallets
      forM_ wallets $ \ w -> do
        deposit w $ Ada.toValue Ledger.minAdaTxOut
        deposit w $ mconcat [ symAssetClassValue t 1000000 | t <- ts ]
      exchangeableTokens %= (Set.fromList ts <>)
      wait 21

    Start -> do
      -- Create the uniswap token
      us <- createToken "Uniswap"
      uniswapToken .= Just us
      -- Pay to the UTxO for the uniswap factory
      withdraw w1 (Ada.toValue Ledger.minAdaTxOut)
      wait 6

    CreatePool w t1 a1 t2 a2 -> do
      startedUserCode %= Set.insert w
      -- Create a new liquidity token unless we
      -- have already created the token in a previous
      -- version of this pool
      mp <- use $ pools . at (poolIndex t1 t2)
      ltok <- case mp of
        Nothing -> createToken "Liquidity"
        Just p  -> pure $ p ^. liquidityToken
      -- Compute the amount of liquidity and token
      let (tokAAmount, tokBAmount) = mkAmounts t1 t2 a1 a2
          liq = calculateInitialLiquidity tokAAmount tokBAmount
      when (liq > 0) $ do
        -- Create the pool
        pools %= Map.insert (poolIndex t1 t2) (PoolModel tokAAmount tokBAmount (Map.singleton w liq) ltok)
        -- Give `w` the liquidity tokens
        let liqVal = symAssetClassValue ltok (unAmount liq)
        deposit w liqVal
        mint liqVal
        -- Pay to the pool
        withdraw w $ Ada.toValue Ledger.minAdaTxOut
        withdraw w $ symAssetClassValue t1 a1
                  <> symAssetClassValue t2 a2
      wait 5

    AddLiquidity w t1 a1 t2 a2 -> do
      startedUserCode %= Set.insert w
      p <- use $ pools . at (poolIndex t1 t2) . to fromJust
      -- Compute the amount of liqiudity token we get
      let (deltaA, deltaB) = mkAmounts t1 t2 a1 a2
          deltaL = calculateAdditionalLiquidity (p ^. coinAAmount)
                                                (p ^. coinBAmount)
                                                (p ^. liquidities . to sum)
                                                deltaA
                                                deltaB
      when (deltaL > 0) $ do
        -- Update the pool with the new token and the new liquidity
        let p' = p
               & liquidities . at w %~ Just . (+deltaL) . sum
               & coinAAmount +~ deltaA
               & coinBAmount +~ deltaB
        pools . at (poolIndex t1 t2) .= Just p'
        -- Give the tokens to the pool
        withdraw w $ symAssetClassValue t1 a1 <> symAssetClassValue t2 a2
        -- Create liquidity token and give it to `w`
        let liqVal = symAssetClassValue (p ^. liquidityToken) (unAmount deltaL)
        mint liqVal
        deposit w liqVal
        -- Make sure product increases
        assertSpec "AddLiquidity increases total product" $
          unAmount (p ^. coinAAmount) * unAmount (p ^. coinBAmount) <
            unAmount (p' ^. coinAAmount) * unAmount (p' ^. coinBAmount)
      wait 5

    PerformSwap w t1 t2 a -> do
      startedUserCode %= Set.insert w
      p <- use $ pools . at (poolIndex t1 t2) . to fromJust
      let oldA = p ^. coinAAmount
          oldB = p ^. coinBAmount
          -- Depending on whether t1 < t2
          -- we are either swapping A-token for B-token
          -- or the other way around
          (a', p', lens) = if t1 < t2
                           then let a' = findSwapA oldA oldB (Amount a)
                                    p' = p & coinAAmount +~ Amount a
                                           & coinBAmount -~ Amount a'
                                in (a', p', coinBAmount . to unAmount)
                           else let a' = findSwapB oldA oldB (Amount a)
                                    p' = p & coinBAmount +~ Amount a
                                           & coinAAmount -~ Amount a'
                                in (a', p', coinAAmount . to unAmount)
      when (0 < a' && a' < p ^. lens) $ do
        -- Swap the coins
        withdraw w $ symAssetClassValue t1 a
        deposit w $ symAssetClassValue t2 a'
        -- Update the pool
        pools . at (poolIndex t1 t2) .= Just p'
        -- Make sure product increases
        assertSpec "PerformSwap increases total product" $
            unAmount (p ^. coinAAmount) * unAmount (p ^. coinBAmount) <
              unAmount (p' ^. coinAAmount) * unAmount (p' ^. coinBAmount)
      wait 5

    RemoveLiquidity w t1 t2 a -> do
      startedUserCode %= Set.insert w
      -- Update the pool
      p <- use $ pools . at (poolIndex t1 t2) . to fromJust
      let inA = p ^. coinAAmount
          inB = p ^. coinBAmount
          liquidity = sum $ p ^. liquidities
          (outA, outB) = calculateRemoval inA inB liquidity (Amount a)
          p' = p
             & coinAAmount .~ outA
             & coinBAmount .~ outB
             & liquidities . at w . _Just -~ Amount a
      pools . at (poolIndex t1 t2) .= Just p'
      -- Take the requisite amount of coin out of the contract
      deposit w $ symAssetClassValue (getAToken t1 t2) (unAmount $ inA - outA)
                <> symAssetClassValue (getBToken t1 t2) (unAmount $ inB - outB)
      -- Burn the liquidity tokens
      let liqVal = symAssetClassValue (p ^. liquidityToken) a
      withdraw w liqVal
      mint $ inv liqVal
      wait 5

    ClosePool w t1 t2 -> do
      startedUserCode %= Set.insert w
      p <- use $ pools . at (poolIndex t1 t2) . to fromJust
      -- Reset the pool
      let p' = p
             & coinAAmount .~ 0
             & coinBAmount .~ 0
             & liquidities .~ Map.empty
      pools . at (poolIndex t1 t2) .= Just p'
      -- Take the rest of the money out of the contract
      deposit w $ symAssetClassValue (getAToken t1 t2) (p ^. coinAAmount . to unAmount)
                <> symAssetClassValue (getBToken t1 t2) (p ^. coinBAmount . to unAmount)
      let liqVal = symAssetClassValue (p ^. liquidityToken) (p ^. liquidities . at w . to fromJust . to unAmount)
      -- Burn the remaining liquidity tokens
      withdraw w liqVal
      mint $ inv liqVal
      -- Return the 2 ada at the script to the wallet
      deposit w $ Ada.toValue Ledger.minAdaTxOut
      wait 5

  perform h tokenSem s act = case act of
    SetupTokens -> do
      delay 20
      Trace.observableState (h SetupKey) >>= \case
        Just (Semigroup.Last cur) -> sequence_ [ registerToken tn (Value.assetClass (Currency.currencySymbol cur) $ fromString tn) | tn <- ["A", "B", "C", "D"]]
        _                         -> Trace.throwError $ GenericError "failed to create currency"

    Start -> do
      delay 5
      Trace.observableState (h OwnerKey) >>= \case
        Last (Just (Right (Uniswap (Coin v)))) -> registerToken "Uniswap" v
        _                                      -> Trace.throwError $ GenericError "initialisation failed"

    CreatePool w t1 a1 t2 a2 -> do
      let us = s ^. contractState . uniswapToken . to fromJust
          c1 = Coin (tokenSem $ getAToken t1 t2)
          c2 = Coin (tokenSem $ getBToken t1 t2)
          Coin ac = liquidityCoin (fst . Value.unAssetClass . tokenSem $ us) c1 c2
      Trace.callEndpoint @"create" (h (WalletKey w)) $ CreateParams c1 c2 (Amount a1) (Amount a2)
      delay 5
      when (not $ hasPool s t1 t2) $ do
        registerToken "Liquidity" ac

    AddLiquidity w t1 a1 t2 a2 -> do
      let c1 = Coin (tokenSem $ getAToken t1 t2)
          c2 = Coin (tokenSem $ getBToken t1 t2)
      Trace.callEndpoint @"add" (h (WalletKey w)) $ AddParams c1 c2 (Amount a1) (Amount a2)
      delay 5

    PerformSwap w t1 t2 a -> do
      let c1 = Coin (tokenSem t1)
          c2 = Coin (tokenSem t2)
      Trace.callEndpoint @"swap" (h (WalletKey w)) $ SwapParams c1 c2 (Amount a) 0
      delay 5

    RemoveLiquidity w t1 t2 a -> do
      let c1 = Coin (tokenSem $ getAToken t1 t2)
          c2 = Coin (tokenSem $ getBToken t1 t2)
      Trace.callEndpoint @"remove" (h (WalletKey w)) $ RemoveParams c1 c2 (Amount a)
      delay 5

    ClosePool w t1 t2 -> do
      let c1 = Coin (tokenSem $ getAToken t1 t2)
          c2 = Coin (tokenSem $ getBToken t1 t2)
      Trace.callEndpoint @"close" (h (WalletKey w)) $ CloseParams c1 c2
      delay 5

  shrinkAction _ a = case a of
    CreatePool w t1 a1 t2 a2   -> [ CreatePool w t1 a1' t2 a2'   | (a1', a2') <- shrink (a1, a2), a1' >= 0, a2' >= 0 ]
    AddLiquidity w t1 a1 t2 a2 -> [ AddLiquidity w t1 a1' t2 a2' | (a1', a2') <- shrink (a1, a2), a1' >= 0, a2' >= 0 ]
    RemoveLiquidity w t1 t2 a  -> [ RemoveLiquidity w t1 t2 a'   | a'         <- shrink a, a' >= 0 ]
    PerformSwap w t1 t2 a      -> [ PerformSwap w t1 t2 a'       | a'         <- shrink a, a' >= 0 ]
    _                          -> []

-- This doesn't hold
prop_liquidityValue :: Property
prop_liquidityValue = forAllDL liquidityValue (const True)
  where
    liquidityValue :: DL UniswapModel ()
    liquidityValue = do
      anyActions_
      pools <- viewContractState pools
      forM_ (Map.filter (view open) pools) $ \ p -> do
        let cond  = calculateRemoval (p ^. coinAAmount) (p ^. coinBAmount) (p ^. liquidities . to sum) (Amount 1)
                          /= (p ^. coinAAmount, p ^. coinBAmount)
        assert ("Pool\n  " ++ show p ++ "\nforgets single liquidities") cond

noLockProof :: NoLockedFundsProof UniswapModel
noLockProof = defaultNLFP {
      nlfpMainStrategy   = mainStrat,
      nlfpWalletStrategy = walletStrat,
      nlfpOverhead       = const $ toSymValue Ledger.minAdaTxOut,
      nlfpErrorMargin    = wiggle }
    where
        wiggle s = fold [symAssetClassValue t1 (toInteger m) <>
                         symAssetClassValue t2 (toInteger m) <>
                         toSymValue Ledger.minAdaTxOut
                        | (PoolIndex t1 t2, p) <- Map.toList (s ^. contractState . pools)
                        , let numLiqs = length $ p ^. liquidities
                              m = max 0 (numLiqs - 1) ]
        mainStrat = do
            pools <- viewContractState pools
            forM_ (Map.toList pools) $ \ (PoolIndex t1 t2, p) -> do
              let liqs = Map.toList . Map.filter (>0) $ p ^. liquidities
              forM_ (take (length liqs - 1) liqs) $ \ (w, l) ->
                action $ RemoveLiquidity w t1 t2 (unAmount l)
              when (not . null $ liqs) $ do
                action $ ClosePool (fst . last $ liqs) t1 t2

        walletStrat w = do
            pools <- viewContractState pools
            forM_ (Map.toList pools) $ \ (PoolIndex t1 t2, p) -> do
              let liqs = p ^. liquidities . to (Map.filter (>0))
              when (w `Map.member` liqs) $ do
                if Map.keys liqs == [w]
                then action $ ClosePool w t1 t2
                else action $ RemoveLiquidity w t1 t2 (unAmount . sum $ Map.lookup w liqs)

noLockProofLight :: NoLockedFundsProofLight UniswapModel
noLockProofLight = NoLockedFundsProofLight{nlfplMainStrategy = nlfpMainStrategy noLockProof}

prop_CheckNoLockedFundsProof :: Property
prop_CheckNoLockedFundsProof = checkNoLockedFundsProof defaultCheckOptionsContractModel noLockProof

prop_CheckNoLockedFundsProofFast :: Property
prop_CheckNoLockedFundsProofFast = checkNoLockedFundsProofFast defaultCheckOptionsContractModel noLockProof

check_propUniswapWithCoverage :: IO ()
check_propUniswapWithCoverage = void $
  quickCheckWithCoverage (stdArgs { maxSuccess = 1000 })
                         (set endpointCoverageReq epReqs $ set coverageIndex covIdx $ defaultCoverageOptions)
                         $ \covopts -> propRunActionsWithOptions @UniswapModel
                                          defaultCheckOptionsContractModel
                                          covopts
                                          (const (pure True))
  where
    epReqs t ep
      | t == Trace.walletInstanceTag w1 = 0
      | ep == "create"                  = 20
      | ep == "swap"                    = 15
      | ep == "close"                   = 0.5
      | ep == "remove"                  = 15
      | ep == "add"                     = 20
      | otherwise                       = 0

prop_Whitelist :: Actions UniswapModel -> Property
prop_Whitelist = checkErrorWhitelist defaultWhitelist

tests :: TestTree
tests = testGroup "uniswap" [
    checkPredicate "can create a liquidity pool and add liquidity"
        (assertNotDone Uniswap.setupTokens
                       (Trace.walletInstanceTag w1)
                       "setupTokens contract should be still running"
        .&&. assertNoFailedTransactions)
        Uniswap.uniswapTrace
    -- TODO: turned off until there is an option to turn off cardano-ledger validation
    -- , testProperty "prop_Uniswap" $ withMaxSuccess 20 prop_Uniswap
    , testProperty "prop_UniswapAssertions" $ withMaxSuccess 1000 (propSanityCheckAssertions @UniswapModel)
    , testProperty "prop_NLFP" $ withMaxSuccess 250 prop_CheckNoLockedFundsProofFast
    ]

runTestsWithCoverage :: IO ()
runTestsWithCoverage = do
  ref <- newCoverageRef
  defaultMain (coverageTests ref)
    `catch` \(e :: SomeException) -> do
                report <- readCoverageRef ref
                putStrLn . show $ pprCoverageReport covIdx report
                throwIO e
  where
    coverageTests ref = testGroup "game state machine tests"
                         [ checkPredicateCoverage "can create a liquidity pool and add liquidity"
                            ref
                            (assertNotDone Uniswap.setupTokens
                                           (Trace.walletInstanceTag w1)
                                           "setupTokens contract should be still running"
                            .&&. assertNoFailedTransactions)
                            Uniswap.uniswapTrace
                          ]

-- | Certification.
certification :: Certification UniswapModel
certification = defaultCertification {
    certNoLockedFundsLight = Just noLockProofLight,
    certCoverageIndex      = covIdx
  }
