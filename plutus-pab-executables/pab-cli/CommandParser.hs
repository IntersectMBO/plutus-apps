{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module CommandParser where

import Control.Monad (unless)
import Control.Monad.Catch (MonadThrow (throwM))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Ledger (Slot (Slot))
import Ledger.Blockchain (BlockId (BlockId))
import Plutus.ChainIndex.Types (Point (Point, PointAtGenesis))
import System.Console.Docopt (Arguments, Docopt, Option, command, docopt, exitWithUsage, getArg, getArgOrExitWith,
                              isPresent, longOption)
import System.Directory (Permissions (executable), doesDirectoryExist, doesFileExist, getHomeDirectory, getPermissions)
import System.FilePath (addTrailingPathSeparator, normalise, (</>))
import System.Path.NameManip (absolute_path, guess_dotdot)
import Text.Read (readMaybe)
import Types (AppError (ChainIndexPortError, DirectoryDoesNotExistError, NodePortError, PABDBPoolSizeError, PABExeDoesNotExistError, PABExeNotExecutableError, PABExeNotProvidedError, PABPortError, PABResumeFromBlockIdNotSpecifiedError, PABResumeFromSlotNotANumberError, PABResumeFromSlotNotSpecifiedError, PABRollbackHistoryNotNumberError, UnspecifiedNodeNetworkError, WalletPortError),
              AppOpts (AppOpts), ChainIndexOpts (ChainIndexOpts), ChainIndexPort (ChainIndexPort),
              ConfigCommand (MockNetCommand, NodeRemoteWalletCommand, NodeWBECommand), NetworkName (Mainnet, Testnet),
              NodeDirectory (NodeDirectory), NodeOpts (NodeOpts, nodeOptsOutputDir), NodePort (NodePort),
              PABDirectory (PABDirectory), PABExe (PABExe), PABOpts (PABOpts), PABPort (PABPort),
              WalletPort (WalletPort))

-- | WARNING: IF YOU MODIFY THIS, DON'T FORGET TO UPDATE THE REAMDE.
patterns :: Docopt
patterns = [docopt|
PAB CLI. This script allows the user to run the PAB in a hosted scenario, and provides good defaults for common usecases.

THIS IS AN EXPERIMENT! DO NOT USE IN A PRODUCTION ENVIRONMENT.

For any possible enhancements and suggestions, submit an issue on https://github.com/input-output-hk/plutus-apps/issues.

Usage: pab-cli
    pab-cli mocknet --pab-exe=<exe> [--pab-dir=<dir>] [--pab-db-pool-size=<size>] [--pab-port=<port>] [--pab-passphrase=<passphrase>] [--pab-rollback-history=<n>] ([--pab-resume-from-block-id=<blockid> --pab-resume-from-slot=<slot>]) [--chain-index-port=<port>] [--node-dir=<dir>] [--node-port=<port>] [--wallet-port=<port>]
    pab-cli (mainnet | testnet) wbe [(--pab-exe=<exe> [--pab-dir=<dir>] [--pab-db-pool-size=<size>] [--pab-port=<port>] [--pab-passphrase=<passphrase>] [--pab-rollback-history=<n>] ([--pab-resume-from-block-id=<blockid> --pab-resume-from-slot=<slot>]))] [--chain-index-port=<port>] [--node-dir=<dir>] [--node-port=<port>] [--wallet-port=<port>]
    pab-cli (mainnet | testnet) remotewallet [(--pab-exe=<exe> [--pab-dir=<dir>] [--pab-db-pool-size=<size>] [--pab-port=<port>] [--pab-passphrase=<passphrase>] [--pab-rollback-history=<n>] ([--pab-resume-from-block-id=<blockid> --pab-resume-from-slot=<slot>]))] [--chain-index-port=<port>] [--node-dir=<dir>] [--node-port=<port>]
    pab-cli -h|--help

Options:
    -h --help                             show this
    --pab-exe <exe>                       PAB executable with builtin contracts. Ex. "$(cabal list-bin plutus-pab-examples)"
    --pab-dir <dir>                       PAB output directory for config, logs, db, etc. If don't specify it, the pab directory will reside in '--node-dir'.
    --pab-db-pool-size <size>             PAB database pool size
                                          [default: 20]
    --pab-port <port>                     PAB webserver port number
                                          [default: 9080]
    --pab-passphrase <passphrase>         PAB wallet passphrase
    --pab-rollback-history <n>            PAB rollback history
    --pab-resume-from-block-id <blockid>  Block number to resume syncing from
    --pab-resume-from-slot <slot>         Slot number to resume syncing from
    --chain-index-port <port>             chain index port number
                                          [default: 9083]
    --node-dir <ndir>                     node output directory config, logs, db, etc.
                                          [default: /tmp/cardano-node]
    --node-port <port>                    node port number
                                          [default:9082]
    --wallet-port <port>                  wallet server port number
                                          [default:9081]
|]

getArgOrExit :: Arguments -> Option -> IO String
getArgOrExit = getArgOrExitWith patterns

argsToAppOpts :: (MonadIO m, MonadThrow m) => Arguments -> m AppOpts
argsToAppOpts args = do
    if args `isPresent` longOption "help"
    then
        liftIO $ exitWithUsage patterns
    else do
        chainIndexOpts <- parseChainIndexOpts args

        if args `isPresent` command "mocknet"
        then do
            nodeOpts <- parseNodeOpts "mock" args
            pabOpts <- parsePABOpts (nodeOptsOutputDir nodeOpts) "mock" args
                   >>= maybe (throwM PABExeNotProvidedError) pure
            walletPort <- parseWalletPort args
            pure $ AppOpts (MockNetCommand pabOpts walletPort)
                           nodeOpts
                           chainIndexOpts
        else if args `isPresent` command "wbe"
        then do
            network <- parseNetworkArgs args
            nodeOpts <- parseNodeOpts (map toLower $ show network) args
            pabOpts <- parsePABOpts (nodeOptsOutputDir nodeOpts)
                                    (map toLower $ show network)
                                    args
            walletPort <- parseWalletPort args
            pure $ AppOpts (NodeWBECommand network pabOpts walletPort)
                           nodeOpts
                           chainIndexOpts
        else if args `isPresent` command "remotewallet"
        then do
            network <- parseNetworkArgs args
            nodeOpts <- parseNodeOpts (map toLower $ show network) args
            pabOpts <- parsePABOpts (nodeOptsOutputDir nodeOpts)
                                    (map toLower $ show network)
                                    args
            pure $ AppOpts (NodeRemoteWalletCommand network pabOpts)
                           nodeOpts
                           chainIndexOpts
        else liftIO $ exitWithUsage patterns

parsePABOpts
    :: (MonadIO m, MonadThrow m)
    => NodeDirectory
    -> String
    -> Arguments
    -> m (Maybe PABOpts)
parsePABOpts nodeDir subdir args = do
    pabExeM <- parsePABExe args
    case pabExeM of
      Nothing -> pure Nothing
      Just pabExe -> do
          fmap Just
          $ PABOpts pabExe <$> parsePABOutputDir nodeDir subdir args
                           <*> parsePABPort args
                           <*> parsePABDbPoolSize args
                           <*> parsePABRollbackHistory args
                           <*> pure (parsePABPassphrase args)
                           <*> parsePABResumeFromPoint args

parsePABExe :: (MonadIO m, MonadThrow m) => Arguments -> m (Maybe PABExe)
parsePABExe args = do
    if args `isPresent` longOption "pab-exe"
    then do
        pabExe@(PABExe pe) <- PABExe <$> liftIO (args `getArgOrExit` longOption "pab-exe")
        liftIO (doesFileExist pe)
            >>= \fileExists -> unless fileExists $ throwM PABExeDoesNotExistError
        liftIO (getPermissions pe)
            >>= \filePermissions -> unless (executable filePermissions)
                                           $ throwM PABExeNotExecutableError
        pure $ Just pabExe
    else
        pure Nothing

parsePABOutputDir
    :: (MonadIO m, MonadThrow m)
    => NodeDirectory
    -> String
    -> Arguments
    -> m PABDirectory
parsePABOutputDir (NodeDirectory nd) subdir args = do
    let pabDirM = args `getArg` longOption "pab-dir"
    case pabDirM of
      Just pabDir -> do
          liftIO (doesDirectoryExist pabDir)
              >>= \dirExists -> unless dirExists $ throwM $ DirectoryDoesNotExistError pabDir
          pabDirAbsPath <- liftIO $ absolutize $ pabDir </> subdir
          pure $ PABDirectory pabDirAbsPath
      Nothing -> do
          pabDirAbsPath <- liftIO $ absolutize $ nd </> "pab"
          pure $ PABDirectory pabDirAbsPath

parsePABPort :: (MonadIO m, MonadThrow m) => Arguments -> m PABPort
parsePABPort args = do
    pabPortStr <- liftIO $ args `getArgOrExit` longOption "pab-port"
    maybe (throwM PABPortError) (pure . PABPort) $ readMaybe pabPortStr

parsePABDbPoolSize :: (MonadIO m, MonadThrow m) => Arguments -> m Int
parsePABDbPoolSize args = do
    pabDbPoolSizeStr <- liftIO $ args `getArgOrExit` longOption "pab-db-pool-size"
    maybe (throwM PABDBPoolSizeError) pure $ readMaybe pabDbPoolSizeStr

parsePABRollbackHistory :: (MonadIO m, MonadThrow m) => Arguments -> m (Maybe Int)
parsePABRollbackHistory args = do
    if args `isPresent` longOption "pab-rollback-history"
    then do
        pabRollbackHistory <-
            maybe (throwM PABRollbackHistoryNotNumberError)
                  pure
                  $ args `getArg` longOption "pab-rollback-history" >>= readMaybe
        pure $ Just pabRollbackHistory
    else
        pure Nothing

parsePABPassphrase :: Arguments -> Maybe String
parsePABPassphrase args = args `getArg` longOption "pab-passphrase"

parsePABResumeFromPoint :: (MonadIO m, MonadThrow m) => Arguments -> m Point
parsePABResumeFromPoint args = do
    if args `isPresent` longOption "pab-resume-from-block-id" && args `isPresent` longOption "pab-resume-from-slot"
    then do
        pabResumeFromBlockId <-
            maybe (throwM PABResumeFromBlockIdNotSpecifiedError)
                  (pure . BlockId . BSC.pack)
                  $ args `getArg` longOption "pab-resume-from-block-id"
        pabResumeFromSlotStr <-
            maybe (throwM PABResumeFromSlotNotSpecifiedError)
                  pure
                  $ args `getArg` longOption "pab-resume-from-slot"
        pabResumeFromSlot <-
            maybe (throwM PABResumeFromSlotNotANumberError)
                  (pure . Slot)
                  $ readMaybe pabResumeFromSlotStr
        pure $ Point pabResumeFromSlot pabResumeFromBlockId

    else
        pure PointAtGenesis

parseChainIndexOpts :: (MonadIO m, MonadThrow m) => Arguments -> m ChainIndexOpts
parseChainIndexOpts args = do
    chainIndexPortStr <- liftIO $ args `getArgOrExit` longOption "chain-index-port"
    port <- maybe (throwM ChainIndexPortError) (pure . ChainIndexPort) $ readMaybe chainIndexPortStr
    pure $ ChainIndexOpts port

parseNodeOpts :: (MonadIO m, MonadThrow m) => String -> Arguments -> m NodeOpts
parseNodeOpts subdir args = NodeOpts <$> parseNodeOutputDir subdir args <*> parseNodePort args

parseNodeOutputDir
    :: (MonadIO m, MonadThrow m)
    => String
    -> Arguments
    -> m NodeDirectory
parseNodeOutputDir subdir args = do
    nodeDir <- liftIO (args `getArgOrExit` longOption "node-dir")
    liftIO (doesDirectoryExist nodeDir)
        >>= \dirExists -> unless dirExists $ throwM $ DirectoryDoesNotExistError nodeDir
    nodeDirAbsPath <- liftIO $ absolutize $ nodeDir </> subdir
    pure $ NodeDirectory nodeDirAbsPath

parseNodePort :: (MonadIO m, MonadThrow m) => Arguments -> m NodePort
parseNodePort args = do
    nodePortStr <- liftIO $ args `getArgOrExit` longOption "node-port"
    maybe (throwM NodePortError) (pure . NodePort) $ readMaybe nodePortStr

parseWalletPort :: (MonadIO m, MonadThrow m) => Arguments -> m WalletPort
parseWalletPort args = do
    walletPortStr <- liftIO $ args `getArgOrExit` longOption "wallet-port"
    maybe (throwM WalletPortError) (pure . WalletPort) $ readMaybe walletPortStr

parseNetworkArgs :: (MonadIO m, MonadThrow m) => Arguments -> m NetworkName
parseNetworkArgs args
  | args `isPresent` command "mainnet" = pure Mainnet
  | args `isPresent` command "testnet" = pure Testnet
  | otherwise = throwM UnspecifiedNodeNetworkError

-- | Code copied from [https://www.schoolofhaskell.com/user/dshevchenko/cookbook/transform-relative-path-to-an-absolute-path].
--
-- Eventually, maybe using `completer (bashCompleter "file")` in
-- optparse-application will do the same thing?
--
-- >>> absolutize ~/dev/env
-- /home/username/dev/env
--
-- >>> absolutize /home/username/~/dev/env
-- /home/username/~/dev/env
--
-- >>> absolutize dev/../dev/env -- Currenth path is /home/username/documents
-- /home/username/documents/dev/env
absolutize :: FilePath -> IO FilePath
absolutize aPath
    | "~" `isPrefixOf` aPath = do
        homePath <- getHomeDirectory
        return $ normalise $ addTrailingPathSeparator homePath ++ tail aPath
    | otherwise = do
        pathMaybeWithDots <- absolute_path aPath
        pure $ maybe pathMaybeWithDots id $ guess_dotdot pathMaybeWithDots
