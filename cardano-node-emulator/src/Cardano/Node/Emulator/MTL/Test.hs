{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE TupleSections       #-}
module Cardano.Node.Emulator.MTL.Test (
  -- * Basic testing
    hasValidatedTransactionCountOfTotal
  , renderLogs
  -- * Testing with `quickcheck-contractmodel`
  , propSanityCheckModel
  , propSanityCheckAssertions
  , propRunActions_
  , propRunActions
  , propRunActionsWithOptions
) where

import Control.Lens (use, (^.))
import Control.Monad.Except (runExceptT)
import Control.Monad.RWS.Strict (evalRWS)
import Control.Monad.Writer (runWriterT)
import Data.Default (def)
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid (Sum (..))
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as Text
import Prettyprinter qualified as Pretty
import Prettyprinter.Render.Text qualified as Pretty
import Test.QuickCheck as QC hiding (total)
import Test.QuickCheck.ContractModel as CM
import Test.QuickCheck.ContractModel.Internal (ContractModelResult)
import Test.QuickCheck.Monadic
import Test.QuickCheck.StateModel (Realized)

import Cardano.Api qualified as C
import Cardano.Api qualified as CardanoAPI
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Node.Emulator qualified as E
import Cardano.Node.Emulator.Chain as E (ChainState, _chainNewestFirst)
import Cardano.Node.Emulator.MTL
import Cardano.Node.Emulator.Params (pNetworkId, pProtocolParams)
import Data.Maybe (fromMaybe)
import Ledger (CardanoAddress, CardanoTx (..), OnChainTx, TxId (..), TxIn, TxOut (..), TxOutRef (..), UtxoIndex (..),
               consumableInputs, onChainTxIsValid, outputsProduced, txInRef, unOnChain)
import Ledger.Index qualified as Index
import Ledger.Tx.CardanoAPI (fromCardanoSlotNo)
import Ledger.Value.CardanoAPI qualified as Value
import PlutusTx.Builtins qualified as Builtins


-- | Test the number of validated transactions and the total number of transactions.
-- Returns a failure message if the numbers don't match up.
hasValidatedTransactionCountOfTotal :: Int -> Int -> Seq E.ChainEvent -> Maybe String
hasValidatedTransactionCountOfTotal valid total lg =
  let count = \case
        E.TxnValidate{}       -> (Sum 1, Sum 0)
        E.TxnValidationFail{} -> (Sum 0, Sum 1)
        _                     -> mempty
      (Sum validCount, Sum invalidCount) = foldMap count lg
  in
    if valid /= validCount then Just $ "Unexpected number of valid transactions: " ++ show validCount
    else if total - valid /= invalidCount then Just $ "Unexpected number of invalid transactions: " ++ show invalidCount
    else Nothing

-- | Render the logs in a format useful for debugging why a test failed.
renderLogs :: Seq E.ChainEvent -> String
renderLogs = Text.unpack . Pretty.renderStrict . Pretty.layoutPretty Pretty.defaultLayoutOptions . Pretty.vsep . toList . fmap Pretty.pretty


type instance Realized EmulatorM a = a

instance IsRunnable EmulatorM where
  awaitSlot = Cardano.Node.Emulator.MTL.awaitSlot . fromCardanoSlotNo

instance HasChainIndex EmulatorM where
  getChainIndex = do
    nid <- pNetworkId <$> getParams
    chainStateToChainIndex nid <$> use esChainState

-- | Sanity check a `ContractModel`. Ensures that wallet balances are not always unchanged.
propSanityCheckModel :: forall state. ContractModel state => QC.Property
propSanityCheckModel =
  QC.expectFailure (noBalanceChanges . stateAfter @state)
  where
    noBalanceChanges s = all symIsZero (s ^. balanceChanges)

-- | Sanity check a `ContractModel`. Ensures that all assertions in
-- the property generation succeed.
propSanityCheckAssertions :: forall state. ContractModel state => Actions state -> QC.Property
propSanityCheckAssertions as = asserts $ stateAfter as



-- | Run `Actions` in the emulator and check that the model and the emulator agree on the final
--   wallet balance changes. Starts with 100.000.000 Ada for each wallet and the default parameters.
propRunActions_ :: forall state.
    RunModel state EmulatorM
    => Actions state                           -- ^ The actions to run
    -> Property
propRunActions_ = propRunActions (\_ _ -> Nothing)

propRunActions :: forall state.
    RunModel state EmulatorM
    => (ModelState state -> Seq E.ChainEvent -> Maybe String) -- ^ Predicate to check at the end of execution
    -> Actions state                           -- ^ The actions to run
    -> Property
propRunActions = propRunActionsWithOptions (Map.fromList $ (, Value.adaValueOf 100_000_000) <$> E.knownAddresses) def

propRunActionsWithOptions :: forall state.
    RunModel state EmulatorM
    => Map CardanoAddress C.Value              -- ^ Initial distribution of funds
    -> E.Params                                -- ^ Node parameters
    -> (ModelState state -> Seq E.ChainEvent -> Maybe String) -- ^ Predicate to check at the end of execution
    -> Actions state                           -- ^ The actions to run
    -> Property
propRunActionsWithOptions initialDist params predicate actions =
    asserts finalState QC..&&.
    monadic runFinalPredicate monadicPredicate
    where
        finalState = stateAfter actions
        ps = pProtocolParams params

        monadicPredicate :: PropertyM (RunMonad EmulatorM) Property
        monadicPredicate = do
            result <- runContractModel actions
            pure $ balanceChangePredicate result

        runFinalPredicate :: RunMonad EmulatorM Property
                          -> Property
        runFinalPredicate contract =
          let (res, lg) = (\m -> evalRWS m params (emptyEmulatorStateWithInitialDist initialDist))
                              . runExceptT
                              . fmap fst
                              . runWriterT
                              . unRunMonad
                              $ contract
          in monadicIO $
              case (res, predicate finalState lg) of
                (Left err, _) -> return $ counterexample (renderLogs lg ++ "\n" ++ show err)
                                        $ property False
                (Right prop, Just msg) -> return $ counterexample (renderLogs lg ++ "\n" ++ show msg) prop
                (Right prop, Nothing) -> return prop

        balanceChangePredicate :: ContractModelResult state -> Property
        balanceChangePredicate result =
          let prettyAddr a = fromMaybe (show a) $ lookup (show a) prettyWalletNames
          in assertBalanceChangesMatch (BalanceChangeOptions False signerPaysFees ps prettyAddr) result

prettyWalletNames :: [(String, String)]
prettyWalletNames = [ (show addr, "Wallet " ++ show nr) | (addr, nr) <- zip E.knownAddresses [1..10::Int]]

-- Note `chainStateToChainIndex` below is moved from `Plutus.Contract.Test.ContractModel.Internal`
-- and could use some serious clean up. Mostly to get rid of the conversions to/from plutus types.

-- Note, we don't store the genesis transaction in the index but put it in the before state
-- instead to avoid showing that as a balance change in the models.
chainStateToChainIndex :: CardanoAPI.NetworkId -> E.ChainState -> ChainIndex
chainStateToChainIndex nid cs =
            ChainIndex { -- The Backwards order
                         transactions = fst $ foldr addBlock ([], beforeState)
                                                             ( reverse
                                                             . drop 1
                                                             . reverse
                                                             . _chainNewestFirst
                                                             $ cs)
                       , networkId = nid
                       }
    where beforeState = CM.ChainState { slot = 0
                                      , utxo = makeUTxOs
                                              $ Index.initialise (take 1 $ reverse (_chainNewestFirst cs))
                                      }
          addBlock block (txs, state) =
            ( txs ++ [ TxInState ((\(CardanoEmulatorEraTx tx') -> tx') . unOnChain $ tx)
                                  state
                                  (onChainTxIsValid tx)
                      | tx <- block ]
            , updateState block state )

          updateState :: [OnChainTx] -> CM.ChainState -> CM.ChainState
          updateState block state =
            CM.ChainState{ slot = slot state + 1
                          , utxo = foldr addTx (utxo state) block
                          }

          addTx :: OnChainTx -> CardanoAPI.UTxO CM.Era -> CardanoAPI.UTxO CM.Era
          addTx tx (CardanoAPI.UTxO utxos) =
              CardanoAPI.UTxO $ outputs <> Map.withoutKeys utxos consumed
            where
              consumed :: Set CardanoAPI.TxIn
              consumed = Set.fromList $ map mkTxIn $ consumableInputs tx

              CardanoAPI.UTxO outputs = makeUTxOs $ UtxoIndex $ outputsProduced tx

          mkTxIn :: TxIn -> CardanoAPI.TxIn
          mkTxIn = mkRef . txInRef

          makeUTxOs :: UtxoIndex -> CardanoAPI.UTxO CM.Era
          makeUTxOs (UtxoIndex i) = CardanoAPI.UTxO $ Map.fromList [ (mkRef ref, mkTxOut utxo)
                                                                   | (ref, utxo) <- Map.toList i ]

          mkRef :: TxOutRef -> CardanoAPI.TxIn
          mkRef (TxOutRef (TxId bs) ix) = CardanoAPI.TxIn
                                            (CardanoAPI.TxId $ makeTheHash bs)
                                            (CardanoAPI.TxIx $ fromIntegral ix)

          mkTxOut :: TxOut -> CardanoAPI.TxOut CardanoAPI.CtxUTxO Era
          mkTxOut (TxOut o) = CardanoAPI.toCtxUTxOTxOut o

          makeTheHash :: Crypto.HashAlgorithm crypto => Builtins.BuiltinByteString -> Crypto.Hash crypto stuff
          makeTheHash bs =
            case Crypto.hashFromBytes $ Builtins.fromBuiltin bs of
              Nothing   -> error "Bad hash!"
              Just hash -> hash
