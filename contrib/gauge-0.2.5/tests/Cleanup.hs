{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Gauge.Benchmark(Benchmark, bench, nfIO)
import Gauge.Main.Options (Config(..), Verbosity(Quiet))
import Control.Applicative
import Control.DeepSeq (NFData(..))
import Control.Exception (Exception, try, throwIO)
import Control.Monad (when)
import Data.Typeable (Typeable)
import System.Directory (doesFileExist, removeFile)
import System.IO ( Handle, IOMode(ReadWriteMode), SeekMode(AbsoluteSeek)
                 , hClose, hFileSize, hSeek, openFile)
import GHC.Exts (IsList(..))
import Foundation.Check
import Foundation.Check.Main
import qualified Gauge as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Prelude

instance NFData Handle where
    rnf !_ = ()

data CheckResult = ShouldThrow | WrongData deriving (Show, Typeable, Eq)

instance Exception CheckResult

type BenchmarkWithFile =
  String -> IO Handle -> (Handle -> IO ()) -> (Handle -> IO ()) -> Benchmark

perRun :: BenchmarkWithFile
perRun name alloc clean work =
    bench name $ C.perRunEnvWithCleanup alloc clean work

perBatch :: BenchmarkWithFile
perBatch name alloc clean work =
    bench name $ C.perBatchEnvWithCleanup (const alloc) (const clean) work

envWithCleanup :: BenchmarkWithFile
envWithCleanup name alloc clean work =
    C.envWithCleanup alloc clean $ bench name . nfIO . work

testCleanup :: Bool -> String -> BenchmarkWithFile -> Test
testCleanup shouldFail name withEnvClean = CheckPlan (fromList name) $ do
    existsBefore <- pick "file-exists" $ doesFileExist testFile

    validate "Temporary file not exists" $ existsBefore === False

    result <- runTest . withEnvClean name alloc clean $ \hnd -> do
        result <- hFileSize hnd >>= BS.hGet hnd . fromIntegral
        resetHandle hnd
        when (result /= testData) $ throwIO WrongData
        when shouldFail $ throwIO ShouldThrow

    validate "is-right" $ case result of
        Left WrongData       -> False -- failTest "Incorrect result read from file"
        Left ShouldThrow     -> True
        Right _ | shouldFail -> False -- failTest "Failed to throw exception"
                | otherwise  -> True

    failure <- pick "cleanup" $ do
        existsAfter <- doesFileExist testFile
        if existsAfter
            then removeFile testFile >> pure True
            else pure False
    validate "Suceed to delete temporary file" $ failure === False
  where
    testFile :: String
    testFile = "tmp"

    testData :: ByteString
    testData = "blah"

    runTest :: Benchmark -> Check (Either CheckResult ())
    runTest = pick "run-test" . try . C.defaultMainWith config . pure
      where
        config = C.defaultConfig { verbosity = Quiet , timeLimit = Just 1, iters = Just 1 }

    resetHandle :: Handle -> IO ()
    resetHandle hnd = hSeek hnd AbsoluteSeek 0

    alloc :: IO Handle
    alloc = do
        hnd <- openFile testFile ReadWriteMode
        BS.hPut hnd testData
        resetHandle hnd
        return hnd

    clean :: Handle -> IO ()
    clean hnd = do
        hClose hnd
        removeFile testFile

testSuccess :: String -> BenchmarkWithFile -> Test
testSuccess = testCleanup False

testFailure :: String -> BenchmarkWithFile -> Test
testFailure = testCleanup True

main :: IO ()
main = defaultMain $ Group "cleanup"
    [ testSuccess "perRun Success" perRun
    , testFailure "perRun Failure" perRun
    , testSuccess "perBatch Success" perBatch
    , testFailure "perBatch Failure" perBatch
    , testSuccess "envWithCleanup Success" envWithCleanup
    , testFailure "envWithCleanup Failure" envWithCleanup
    ]
