{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-
Extract validators and partial transactions from emulator traces
-}
module Plutus.Trace.Emulator.Extract(
  ValidatorMode(..),
  writeScriptsTo,
  showStats,
  ScriptsConfig(..),
  Command(..)
) where

import Cardano.Api qualified as C
import Cardano.Node.Emulator.Params (Params (..), networkIdL, protocolParamsL)
import Cardano.Node.Emulator.Validation (CardanoLedgerError, makeTransactionBody)
import Control.Foldl qualified as L
import Control.Lens ((&), (.~))
import Control.Monad.Freer (run)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Monoid (Sum (..))

import Ledger qualified
import Ledger.Tx.CardanoAPI (fromPlutusIndex)
import Ledger.Tx.Constraints.OffChain (UnbalancedTx (..))
import Plutus.Contract.Request (MkTxLog)
import Plutus.Trace.Emulator (EmulatorConfig (_params), EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Api (ExBudget (..))
import Prettyprinter (Pretty (..))
import Streaming.Prelude qualified as S
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Text.Printf (printf)
import Wallet.Emulator.Folds qualified as Folds
import Wallet.Emulator.Stream (foldEmulatorStreamM)

-- | Configuration for 'writeScriptsTo'
data ScriptsConfig =
    ScriptsConfig
        { scPath    :: FilePath -- ^ Folder the extracted scripts should be written to
        , scCommand :: Command -- ^ Whether to write out complete transactions or just the validator scripts
        }

data ValidatorMode = FullyAppliedValidators | UnappliedValidators
    deriving (Eq, Ord, Show)

-- | Command for 'writeScriptsTo'
data Command =
    Scripts -- ^ Write out validator scripts only (flat encoding)
        { unappliedValidators :: ValidatorMode -- ^ Whether to write fully applied or unapplied validators
        }
    | Transactions  -- ^ Write out partial transactions
        { networkId          :: C.NetworkId -- ^ Network ID to use when creating addresses
        , protocolParamsJSON :: FilePath -- ^ Location of a JSON file with protocol parameters
        }
    | MkTxLogs -- ^ Write out the arguments and results of 'mkTx' calls
    deriving stock (Show, Eq)

{-| Run an emulator trace and write the applied scripts to a file in Flat format
    using the name as a prefix.
-}
writeScriptsTo
    :: ScriptsConfig -- ^ Configuration
    -> String -- ^ Prefix to be used for file names
    -> EmulatorTrace a -- ^ Emulator trace to extract transactions from
    -> EmulatorConfig -- ^ Emulator config
    -> IO (Sum Int64, ExBudget) -- Total size and 'ExBudget' of extracted scripts
writeScriptsTo ScriptsConfig{scPath, scCommand} prefix trace emulatorCfg = do
    let stream = Trace.runEmulatorStream emulatorCfg trace
        getEvents :: Folds.EmulatorEventFold a -> a
        getEvents theFold = S.fst' $ run $ foldEmulatorStreamM (L.generalize theFold) stream
    createDirectoryIfMissing True scPath
    case scCommand of
        Scripts _ -> pure mempty
        Transactions{networkId, protocolParamsJSON} -> do
            bs <- BSL.readFile protocolParamsJSON
            case Aeson.eitherDecode bs of
                Left err -> putStrLn err
                Right pp ->
                    let params = _params emulatorCfg & protocolParamsL .~ pp
                                                     & networkIdL .~ networkId
                    in traverse_
                        (uncurry $ writeTransaction params scPath prefix)
                        (zip [1::Int ..] $ getEvents Folds.walletTxBalanceEvents)
            pure mempty
        MkTxLogs -> do
            traverse_
                (uncurry $ writeMkTxLog scPath prefix)
                (zip [1::Int ..] $ getEvents Folds.mkTxLogs)
            pure mempty

showStats :: Int64 -> ExBudget -> String
showStats byteSize (ExBudget exCPU exMemory) = "Size: " <> size <> "kB, Cost: " <> show exCPU <> ", " <> show exMemory
    where
        size = printf ("%.1f"::String) (fromIntegral byteSize / 1024.0 :: Double)

writeTransaction
    :: Params
    -> FilePath
    -> String
    -> Int
    -> UnbalancedTx
    -> IO ()
writeTransaction params fp prefix idx utx = do
    let filename1 = fp </> prefix <> "-" <> show idx <> ".json"
    case buildTx utx of
        Left err ->
            putStrLn $ "Export tx failed for " <> filename1 <> ". Reason: " <> show (pretty err)
        Right ctx -> do
            putStrLn $ "Writing partial transaction JSON: " <> filename1
            BSL.writeFile filename1 $ encodePretty ctx
    where
      buildTx :: UnbalancedTx -> Either CardanoLedgerError (C.Tx C.BabbageEra)
      buildTx (UnbalancedCardanoTx tx utxos) =
        let fromCardanoTx ctx = do
              utxo <- fromPlutusIndex $ Ledger.UtxoIndex utxos
              makeTransactionBody params utxo ctx
        in C.makeSignedTransaction [] <$> fromCardanoTx tx


writeMkTxLog :: FilePath -> String -> Int -> MkTxLog -> IO ()
writeMkTxLog fp prefix idx event = do
    let filename1 = fp </> prefix <> "-" <> show idx <> "-mkTx.json"
    putStrLn $ "Writing mkTxLog transaction JSON: " <> filename1
    BSL.writeFile filename1 $ encodePretty event
