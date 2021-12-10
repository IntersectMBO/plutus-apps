{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = { old-time = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "unix-compat"; version = "0.5.3"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Björn Bringert, Duncan Coutts, Jacob Stanley, Bryan O'Sullivan";
      homepage = "http://github.com/jacobstanley/unix-compat";
      url = "";
      synopsis = "Portable POSIX-compatibility layer.";
      description = "This package provides portable implementations of parts\nof the unix package. This package re-exports the unix\npackage when available. When it isn't available,\nportable implementations are used.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if system.isWindows
          then [
            (hsPkgs."Win32" or (errorHandler.buildDepError "Win32"))
            ] ++ (if flags.old-time
            then [
              (hsPkgs."old-time" or (errorHandler.buildDepError "old-time"))
              ] ++ [
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              ]
            else [
              (hsPkgs."time" or (errorHandler.buildDepError "time"))
              (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
              ])
          else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."msvcrt" or (errorHandler.sysDepError "msvcrt"));
        buildable = true;
        modules = [
          "System/PosixCompat"
          "System/PosixCompat/Extensions"
          "System/PosixCompat/Files"
          "System/PosixCompat/Temp"
          "System/PosixCompat/Time"
          "System/PosixCompat/Types"
          "System/PosixCompat/Unistd"
          "System/PosixCompat/User"
          ] ++ (pkgs.lib).optional (system.isWindows) "System/PosixCompat/Internal/Time";
        cSources = if system.isWindows
          then [ "cbits/HsUname.c" "cbits/mktemp.c" ]
          else (pkgs.lib).optional (!system.isGhcjs) "cbits/HsUnixCompat.c";
        hsSourceDirs = [ "src" ];
        includeDirs = (pkgs.lib).optional (!system.isWindows) "include";
        includes = (pkgs.lib).optional (!system.isWindows) "HsUnixCompat.h";
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../contrib/unix-compat-0.5.3; }