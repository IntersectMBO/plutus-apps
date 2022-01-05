{-# LANGUAGE LambdaCase #-}
import Control.Monad (filterM, forM, forM_)
import Data.List (intercalate, isPrefixOf, isSuffixOf)
import Distribution.Simple
import Distribution.Simple.BuildTarget (BuildTarget (..), readBuildTargets)
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program (Program, arProgram, gccProgram, ghcProgram, runDbProgram, simpleProgram)
import Distribution.Simple.Program.Db (knownPrograms, lookupKnownProgram, lookupProgram)
import Distribution.Simple.Program.Types (programPath)
import Distribution.Simple.Setup
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import Distribution.Types.BuildInfo (cSources, emptyBuildInfo, includeDirs, jsSources)
import Distribution.Types.ComponentName
import Distribution.Types.HookedBuildInfo
import Distribution.Types.InstalledPackageInfo hiding (includeDirs)
import Distribution.Types.InstalledPackageInfo qualified as IPI
import Distribution.Types.Library (libBuildInfo)
import Distribution.Types.LocalBuildInfo
import Distribution.Types.PackageDescription
import Distribution.Types.PackageName
import Distribution.Verbosity (silent, verbose)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.FilePath


emarProgram :: Program
emarProgram = simpleProgram "emar"

buildEMCCLib :: PackageDescription -> LocalBuildInfo -> IO ()
buildEMCCLib desc lbi = do
    let verbosity = verbose
    -- get build dir
    createDirectoryIfMissingVerbose verbosity True ((buildDir lbi) </> "emcc")
    --
    case library desc of
        Just lib -> do
            let depIncludeDirs = concatMap IPI.includeDirs (topologicalOrder $ installedPkgs lbi)
            -- alright, let's compile all .c files into .o files with emcc, which is the `gcc` program.
            forM_ (cSources . libBuildInfo $ lib) $ \src -> do
                let dst = (buildDir lbi) </> "emcc" </> (src -<.> "o")
                createDirectoryIfMissingVerbose verbosity True (takeDirectory dst)
                runDbProgram verbosity gccProgram (withPrograms lbi) $
                    ["-c", src, "-o", dst] ++ ["-I" <> incDir | incDir <- (includeDirs . libBuildInfo $ lib) ++ depIncludeDirs]

            -- and now construct a canonical `.js_a` file.
            let dstLib = (buildDir lbi) </> "libEMCC" <> (unPackageName . pkgName . package $ desc) <> ".js_a"
            runDbProgram verbosity emarProgram (withPrograms lbi) $
                [ "-r",  dstLib ] ++ [ (buildDir lbi) </> "emcc" </> (src -<.> "o") | src <- cSources . libBuildInfo $ lib ]

            let expLib = (buildDir lbi) </> "libEMCC" <> (unPackageName . pkgName . package $ desc) <> ".exported.js_a"
            names <- forM (jsSources . libBuildInfo $ lib) $ \src -> do
                unwords . concatMap (drop 2 . words) . filter (isPrefixOf "// EMCC:EXPORTED_FUNCTIONS") . lines <$> readFile src

            writeFile expLib (unwords names)

        -- if there's no lib, this is a fairly pointless exercise
        Nothing -> return ()

-- This is here so that we can link multiple libEMCC* libraries fromd ependencies together with emcc.
-- however we don't have figured out how to get the EXPORTED_FUNCTIONS from each dependency merged yet.
--
linkEMCCLib :: PackageDescription -> LocalBuildInfo -> IO ()
linkEMCCLib desc lbi = do
    let verbosity = verbose
    libs <- filterM doesFileExist $
            concatMap (\x -> [ libDir </> "libEMCC" <> (unPackageName . pkgName . sourcePackageId $ x) <> ".js_a"
                            | libDir <- libraryDirs x ])
                    (topologicalOrder $ installedPkgs lbi)
    exff <- filterM doesFileExist $
            concatMap (\x -> [ libDir </> "libEMCC" <> (unPackageName . pkgName . sourcePackageId $ x) <> ".exported.js_a"
                            | libDir <- libraryDirs x ])
                    (topologicalOrder $ installedPkgs lbi)
    exfns <- concat <$> forM exff (fmap words . readFile)

    createDirectoryIfMissingVerbose verbosity True ((buildDir lbi) </> "emcc")
    runDbProgram verbosity gccProgram (withPrograms lbi) $
        [ "-o", (buildDir lbi) </> "emcc" </> "lib.js"
        , "-s", "WASM=0"
        , "-s", "ALLOW_TABLE_GROWTH" -- we need this for addFunction/removeFunction
        -- addFunction, removeFunction are for dynamic functions.
        -- getTempRet0/setTempRet0 are for 64bit legalization.
        , "-s", "EXPORTED_RUNTIME_METHODS=['printErr','addFunction','removeFunction','getTempRet0','setTempRet0']"
        --
        , "-s", "EXPORTED_FUNCTIONS=['" <> intercalate "', '" exfns <> "']"
        ] ++ libs


postBuildHook :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postBuildHook args flags desc lbi = do
    case (takeFileName . programPath <$> lookupProgram ghcProgram (withPrograms lbi)) of
        Just "js-unknown-ghcjs-ghc" ->
            readBuildTargets silent desc (buildArgs flags) >>= \case
                [BuildTargetComponent (CLibName _)]   -> print "OK. Lib (Build)" >> buildEMCCLib desc lbi
                [BuildTargetComponent (CExeName _)]   -> print "OK. Exe"
                [BuildTargetComponent (CTestName _)]  -> print "OK. Test"
                [BuildTargetComponent (CBenchName _)] -> print "OK. Bench"
                _                                     -> print "EEk!"
        _ -> return ()

postConfHook :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
postConfHook args flags desc lbi = do
    case (takeFileName . programPath <$> lookupProgram ghcProgram (withPrograms lbi)) of
        Just "js-unknown-ghcjs-ghc" ->
            readBuildTargets silent desc (configArgs flags) >>= \case
                [BuildTargetComponent (CLibName _)]   -> print "OK. Lib"
                [BuildTargetComponent (CExeName _)]   -> print "OK. Exe (Link)" >> linkEMCCLib desc lbi
                [BuildTargetComponent (CTestName _)]  -> print "OK. Test (Link)" >> linkEMCCLib desc lbi
                [BuildTargetComponent (CBenchName _)] -> print "OK. Bench (Link)" >> linkEMCCLib desc lbi
                _                                     -> print "EEk!"
        _ -> return ()

-- we somehow need to inject the freshly build "emcc/lib.js" into each component.
-- I'm not sure w ecan abuse preBuildHooks for this.
preBuildHook :: Args -> BuildFlags -> IO HookedBuildInfo
preBuildHook args flags = do
    print flags
    pure (Just (emptyBuildInfo { jsSources = ["dist/build" </> "emcc" </> "lib.js"] }), [])

main :: IO ()
main = do
    args <- getArgs
    defaultMainWithHooksArgs emccHooks (injectEmar args)
  where
    injectEmar :: [String] -> [String]
    injectEmar [] = []
    injectEmar (x:xs) | "--with-gcc=" `isPrefixOf` x
                      , "emcc" `isSuffixOf` x
        = x:("--with-emar="<> (takeDirectory $ drop 11 $ x) </> "emar"):injectEmar xs
    injectEmar (x:xs) = x:injectEmar xs

    emccHooks :: UserHooks
    emccHooks = simpleUserHooks
        { postConf = postConfHook
        --    , preBuild = preBuildHook
        , postBuild = postBuildHook
        , hookedPrograms = [emarProgram]
        }
