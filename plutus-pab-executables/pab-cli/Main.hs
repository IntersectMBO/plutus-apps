module Main (main) where

import Control.Monad (filterM, unless)
import Control.Monad.Catch (catch)
import Control.Monad.Freer (runM)
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (MonadIO (liftIO))
import System.Console.Docopt (exitWithUsageMessage, parseArgsOrExit)
import System.Directory (doesFileExist)
import System.Environment (getArgs)
import System.Exit (exitFailure)


import App (cardanoNodeCmd, cardanoWalletCmd, plutusChainIndexCmd, runApp)
import CommandParser (argsToAppOpts, patterns)
import Types (AppError)

main :: IO ()
main = do
    -- TODO: Temporary until we call the underlying Haskell functions instead
    -- of relying on the commands to be available in the current PATH.
    missingExecs <- missingExecutables [plutusChainIndexCmd, cardanoWalletCmd, cardanoNodeCmd]
    unless (null missingExecs) $ do
        putStrLn $ "The following executables are missing from your PATH: "
                <> show missingExecs
        exitFailure

    -- Parse command line arguments
    args <- parseArgsOrExit patterns =<< getArgs
    appOpts <- argsToAppOpts args `catch` \(e :: AppError) ->
        exitWithUsageMessage patterns (show e)

    runM $ runReader appOpts runApp

missingExecutables :: (MonadIO m) => [String] -> m [String]
missingExecutables = filterM isExecutableMissing
  where
    isExecutableMissing exec = liftIO (doesFileExist exec)
