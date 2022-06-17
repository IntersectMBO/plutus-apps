{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-
Extract validators and partial transactions from emulator traces
-}
module Plutus.Trace.Emulator.Extract(
  writeScriptsTo,
  showStats,
  ScriptsConfig(..),
  Command(..)
) where

import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as C
import Control.Foldl qualified as L
import Control.Monad.Freer (run)
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.Monoid (Sum (..))
import Flat (flat)
import Ledger.Constraints.OffChain (UnbalancedTx (..))
import Ledger.Index (ScriptValidationEvent (..), ValidatorMode (..), getScript)
import Ledger.Params (Params (..))
import Ledger.TimeSlot (SlotConfig)
import Plutus.Contract.Request (MkTxLog)
import Plutus.Contract.Wallet (export)
import Plutus.Trace.Emulator (EmulatorConfig (_params), EmulatorTrace)
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Api (ExBudget (..))
import Plutus.V1.Ledger.Scripts (Script (..))
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
        slotCfg = pSlotConfig $ _params emulatorCfg
        getEvents :: Folds.EmulatorEventFold a -> a
        getEvents theFold = S.fst' $ run $ foldEmulatorStreamM (L.generalize theFold) stream
    createDirectoryIfMissing True scPath
    case scCommand of
        Scripts mode -> do
            foldMap (uncurry $ writeScript scPath prefix mode) (zip [1::Int ..] $ getEvents Folds.scriptEvents)
        Transactions{networkId, protocolParamsJSON} -> do
            bs <- BSL.readFile protocolParamsJSON
            case Aeson.eitherDecode bs of
                Left err -> putStrLn err
                Right params ->
                    traverse_
                        (uncurry $ writeTransaction params networkId slotCfg scPath prefix)
                        (zip [1::Int ..] $ getEvents Folds.walletTxBalanceEvents)
            pure mempty
        MkTxLogs -> do
            traverse_
                (uncurry $ writeMkTxLog scPath prefix)
                (zip [1::Int ..] $ getEvents Folds.mkTxLogs)
            pure mempty

{- There's an instance of Codec.Serialise for
    Script in Scripts.hs (see Note [Using Flat inside CBOR instance of Script]),
    which wraps Flat-encoded bytestings in CBOR, but that's not used here: we
    just use unwrapped Flat because that's more convenient for use with the
    `plc` command, for example.
-}
writeScript :: FilePath -> String -> ValidatorMode -> Int -> ScriptValidationEvent -> IO (Sum Int64, ExBudget)
writeScript fp prefix mode idx event@ScriptValidationEvent{sveResult} = do
    let filename = fp </> prefix <> "-" <> show idx <> filenameSuffix mode <> ".flat"
        bytes = BSL.fromStrict . flat . unScript . getScript mode $ event
        byteSize = BSL.length bytes
    putStrLn $ "Writing script: " <> filename <> " (" <> either show (showStats byteSize . fst) sveResult <> ")"
    BSL.writeFile filename bytes
    pure (Sum byteSize, foldMap fst sveResult)
writeScript _ _ _ _ _ = pure mempty

showStats :: Int64 -> ExBudget -> String
showStats byteSize (ExBudget exCPU exMemory) = "Size: " <> size <> "kB, Cost: " <> show exCPU <> ", " <> show exMemory
    where
        size = printf ("%.1f"::String) (fromIntegral byteSize / 1024.0 :: Double)

writeTransaction
    :: C.ProtocolParameters
    -> C.NetworkId
    -> SlotConfig
    -> FilePath
    -> String
    -> Int
    -> UnbalancedTx
    -> IO ()
writeTransaction params networkId slotConfig fp prefix idx tx = do
    let filename1 = fp </> prefix <> "-" <> show idx <> ".json"
    case export params networkId slotConfig tx of
        Left err ->
            putStrLn $ "Export tx failed for " <> filename1 <> ". Reason: " <> show (pretty err)
        Right exportTx -> do
            putStrLn $ "Writing partial transaction JSON: " <> filename1
            BSL.writeFile filename1 $ encodePretty exportTx

writeMkTxLog :: FilePath -> String -> Int -> MkTxLog -> IO ()
writeMkTxLog fp prefix idx event = do
    let filename1 = fp </> prefix <> "-" <> show idx <> "-mkTx.json"
    putStrLn $ "Writing mkTxLog transaction JSON: " <> filename1
    BSL.writeFile filename1 $ encodePretty event

filenameSuffix :: ValidatorMode -> String
filenameSuffix FullyAppliedValidators = ""
filenameSuffix UnappliedValidators    = "-unapplied"
