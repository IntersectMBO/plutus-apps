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
    flags = { bytestring-in-base = false; };
    package = {
      specVersion = "1.8";
      identifier = { name = "digest"; version = "0.0.1.2"; };
      license = "BSD-3-Clause";
      copyright = "(c) 2009 Eugene Kirpichov";
      maintainer = "Eugene Kirpichov <ekirpichov@gmail.com>";
      author = "Eugene Kirpichov <ekirpichov@gmail.com>";
      homepage = "";
      url = "";
      synopsis = "Various cryptographic hashes for bytestrings; CRC32 and Adler32 for now.";
      description = "This package provides efficient cryptographic hash implementations for\nstrict and lazy bytestrings. For now, CRC32 and Adler32 are supported;\nthey are implemented as FFI bindings to efficient code from zlib.";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [
        "testing/trivial-reference.c"
        "testing/trivial.expected"
        "testing/trivial.hs"
        ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          ] ++ (if flags.bytestring-in-base
          then [ (hsPkgs."base" or (errorHandler.buildDepError "base")) ]
          else [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            ])) ++ (pkgs.lib).optional (!(!system.isWindows)) (hsPkgs."zlib" or (errorHandler.buildDepError "zlib"));
        libs = (pkgs.lib).optional (!system.isWindows) (pkgs."z" or (errorHandler.sysDepError "z"));
        buildable = true;
        modules = [ "Data/Digest/CRC32" "Data/Digest/Adler32" ];
        jsSources = (pkgs.lib).optional (system.isGhcjs) "jsbits/bindings.js";
        includes = [ "zlib.h" ];
        };
      exes = {
        "trivial" = {
          depends = [
            (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            ];
          buildable = true;
          mainPath = [ "Test.hs" ];
          };
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."digest" or (errorHandler.buildDepError "digest"))
            ];
          buildable = true;
          hsSourceDirs = [ "testing" ];
          mainPath = [ "trivial.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../contrib/digest-0.0.1.2; }