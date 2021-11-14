{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.12";
      identifier = { name = "cardano-addresses-jsbits"; version = "3.6.0"; };
      license = "Apache-2.0";
      copyright = "2021 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "https://github.com/input-output-hk/cardano-addresses#readme";
      url = "";
      synopsis = "Javascript code for ghcjs build of cardano-addresses.";
      description = "This package supports ghcjs compilation of cardano-addresses with\nJavascript wrappers and Emscripten builds of the cryptonite C\nsources.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ];
        buildable = true;
        modules = [ "Cardano/Address/Jsbits" ];
        jsSources = (pkgs.lib).optional (compiler.isGhcjs && true || system.isGhcjs) "jsbits/cardano-crypto.js";
        hsSourceDirs = [ "lib" ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "6";
      rev = "minimal";
      sha256 = "";
      }) // {
      url = "6";
      rev = "minimal";
      sha256 = "";
      };
    postUnpack = "sourceRoot+=/jsbits; echo source root reset to \$sourceRoot";
    }) // { cabal-generator = "hpack"; }