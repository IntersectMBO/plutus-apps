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
      specVersion = "1.6";
      identifier = { name = "network-info"; version = "0.2.0.10"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Jacob Stanley <jacob@stanley.io>";
      author = "Jacob Stanley";
      homepage = "http://github.com/jacobstanley/network-info";
      url = "";
      synopsis = "Access the local computer's basic network configuration";
      description = "This library provides simple read-only access to the\nlocal computer's networking configuration. It is\ncurrently capable of getting a list of all the network\ninterfaces and their respective IPv4, IPv6 and MAC\naddresses.\n\nnetwork-info has been tested and is known to work on\nUbuntu 10.10, FreeBSD 9.0, Mac OS X 10.6.4 and Windows\nXP/7. It probably works on other flavours of Linux,\nOS X, FreeBSD and Windows as well.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "cbits/common.h"
        "cbits/common.inc"
        "cbits/network.h"
        "test/src/Main.hs"
        "test/network-info-test.cabal"
        "test/run-tests.bat"
        "test/run-tests.sh"
        "README.mkd"
        "CHANGELOG.md"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        libs = (pkgs.lib).optional (system.isWindows) (pkgs."iphlpapi" or (errorHandler.sysDepError "iphlpapi")) ++ (pkgs.lib).optionals (system.isSolaris) [
          (pkgs."socket" or (errorHandler.sysDepError "socket"))
          (pkgs."nsl" or (errorHandler.sysDepError "nsl"))
          ];
        buildable = true;
        modules = [ "Network/Info" ];
        cSources = if system.isWindows
          then [ "cbits/network-windows.c" ]
          else (pkgs.lib).optional (!system.isGhcjs) "cbits/network-unix.c";
        hsSourceDirs = [ "src" ];
        includeDirs = [ "cbits" ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../contrib/network-info-0.2.0.10; }