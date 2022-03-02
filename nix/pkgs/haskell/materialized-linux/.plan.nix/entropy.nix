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
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "entropy"; version = "0.4.1.7"; };
      license = "BSD-3-Clause";
      copyright = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      maintainer = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      author = "Thomas DuBuisson <thomas.dubuisson@gmail.com>";
      homepage = "https://github.com/TomMD/entropy";
      url = "";
      synopsis = "A platform independent entropy source";
      description = "A mostly platform independent method to obtain cryptographically strong entropy\n(RDRAND, urandom, CryptAPI, and patches welcome)\nUsers looking for cryptographically strong (number-theoretically\nsound) PRNGs should see the 'DRBG' package too.";
      buildType = "Custom";
      isLocal = true;
      setup-depends = [
        (hsPkgs.buildPackages.Cabal or (pkgs.buildPackages.Cabal or (errorHandler.setupDepError "Cabal")))
        (hsPkgs.buildPackages.base or (pkgs.buildPackages.base or (errorHandler.setupDepError "base")))
        (hsPkgs.buildPackages.filepath or (pkgs.buildPackages.filepath or (errorHandler.setupDepError "filepath")))
        (hsPkgs.buildPackages.directory or (pkgs.buildPackages.directory or (errorHandler.setupDepError "directory")))
        (hsPkgs.buildPackages.process or (pkgs.buildPackages.process or (errorHandler.setupDepError "process")))
        ];
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "./cbits/getrandom.c"
        "./cbits/random_initialized.c"
        "./cbits/rdrand.c"
        "./cbits/rdrand.h"
        "README.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          ] ++ (if compiler.isGhcjs && true || system.isGhcjs
          then [
            (hsPkgs."ghcjs-dom" or (errorHandler.buildDepError "ghcjs-dom"))
            (hsPkgs."jsaddle" or (errorHandler.buildDepError "jsaddle"))
            ]
          else if system.isWindows
            then [ (hsPkgs."Win32" or (errorHandler.buildDepError "Win32")) ]
            else [ (hsPkgs."unix" or (errorHandler.buildDepError "unix")) ]);
        libs = (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs)) ((pkgs.lib).optional (system.isWindows) (pkgs."advapi32" or (errorHandler.sysDepError "advapi32")));
        buildable = true;
        modules = [
          "System/Entropy"
          ] ++ (if compiler.isGhcjs && true || system.isGhcjs
          then [ "System/EntropyGhcjs" ]
          else if system.isWindows
            then [ "System/EntropyWindows" ]
            else [ "System/EntropyNix" ]);
        cSources = (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs)) ((pkgs.lib).optional (system.isX86_64) "cbits/rdrand.c" ++ (pkgs.lib).optionals (!system.isWindows) [
          "cbits/getrandom.c"
          "cbits/random_initialized.c"
          ]);
        includeDirs = (pkgs.lib).optionals (!(compiler.isGhcjs && true || system.isGhcjs)) ((pkgs.lib).optional (system.isX86_64) "cbits");
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "15";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "15";
      rev = "minimal";
      sha256 = "";
      };
    }