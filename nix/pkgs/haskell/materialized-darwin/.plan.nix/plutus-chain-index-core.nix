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
      specVersion = "2.2";
      identifier = { name = "plutus-chain-index-core"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "sjoerd.visscher@iohk.io";
      author = "Sjoerd Visscher";
      homepage = "https://github.com/iohk/plutus#readme";
      url = "";
      synopsis = "";
      description = "Please see the README on GitHub at <https://github.com/input-output-hk/plutus#readme>";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" "NOTICE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
          (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
          (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
          (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."beam-core" or (errorHandler.buildDepError "beam-core"))
          (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
          (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
          (hsPkgs."cardano-api" or (errorHandler.buildDepError "cardano-api"))
          (hsPkgs."cardano-ledger-byron" or (errorHandler.buildDepError "cardano-ledger-byron"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
          (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
          (hsPkgs."io-classes" or (errorHandler.buildDepError "io-classes"))
          (hsPkgs."iohk-monitoring" or (errorHandler.buildDepError "iohk-monitoring"))
          (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
          (hsPkgs."memory" or (errorHandler.buildDepError "memory"))
          (hsPkgs."nothunks" or (errorHandler.buildDepError "nothunks"))
          (hsPkgs."openapi3" or (errorHandler.buildDepError "openapi3"))
          (hsPkgs."ouroboros-network" or (errorHandler.buildDepError "ouroboros-network"))
          (hsPkgs."ouroboros-network-framework" or (errorHandler.buildDepError "ouroboros-network-framework"))
          (hsPkgs."ouroboros-consensus" or (errorHandler.buildDepError "ouroboros-consensus"))
          (hsPkgs."ouroboros-consensus-byron" or (errorHandler.buildDepError "ouroboros-consensus-byron"))
          (hsPkgs."ouroboros-consensus-cardano" or (errorHandler.buildDepError "ouroboros-consensus-cardano"))
          (hsPkgs."ouroboros-consensus-shelley" or (errorHandler.buildDepError "ouroboros-consensus-shelley"))
          (hsPkgs."prettyprinter" or (errorHandler.buildDepError "prettyprinter"))
          (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
          (hsPkgs."semigroups" or (errorHandler.buildDepError "semigroups"))
          (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."typed-protocols" or (errorHandler.buildDepError "typed-protocols"))
          (hsPkgs."unordered-containers" or (errorHandler.buildDepError "unordered-containers"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-openapi3" or (errorHandler.buildDepError "servant-openapi3"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."servant-swagger-ui" or (errorHandler.buildDepError "servant-swagger-ui"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."servant-client" or (errorHandler.buildDepError "servant-client"))
          (hsPkgs."servant-client-core" or (errorHandler.buildDepError "servant-client-core"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."text-class" or (errorHandler.buildDepError "text-class"))
          ];
        buildable = true;
        modules = [
          "Plutus/ChainIndex"
          "Plutus/ChainIndex/Api"
          "Plutus/ChainIndex/ChainIndexError"
          "Plutus/ChainIndex/ChainIndexLog"
          "Plutus/ChainIndex/Client"
          "Plutus/ChainIndex/DbSchema"
          "Plutus/ChainIndex/Effects"
          "Plutus/ChainIndex/Emulator"
          "Plutus/ChainIndex/Emulator/DiskState"
          "Plutus/ChainIndex/Emulator/Handlers"
          "Plutus/ChainIndex/Emulator/Server"
          "Plutus/ChainIndex/Handlers"
          "Plutus/ChainIndex/Server"
          "Plutus/ChainIndex/Tx"
          "Plutus/ChainIndex/TxIdState"
          "Plutus/ChainIndex/TxOutBalance"
          "Plutus/ChainIndex/TxUtxoBalance"
          "Plutus/ChainIndex/Types"
          "Plutus/ChainIndex/UtxoState"
          "Plutus/Monitoring/Util"
          "Cardano/Protocol/Socket/Type"
          "Cardano/Protocol/Socket/Client"
          "Plutus/ChainIndex/Compatibility"
          "Plutus/Contract/CardanoAPI"
          ];
        hsSourceDirs = [ "src" ];
        };
      tests = {
        "plutus-chain-index-test" = {
          depends = [
            (hsPkgs."plutus-ledger" or (errorHandler.buildDepError "plutus-ledger"))
            (hsPkgs."plutus-ledger-api" or (errorHandler.buildDepError "plutus-ledger-api"))
            (hsPkgs."plutus-tx" or (errorHandler.buildDepError "plutus-tx"))
            (hsPkgs."plutus-chain-index-core" or (errorHandler.buildDepError "plutus-chain-index-core"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."beam-migrate" or (errorHandler.buildDepError "beam-migrate"))
            (hsPkgs."beam-sqlite" or (errorHandler.buildDepError "beam-sqlite"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."contra-tracer" or (errorHandler.buildDepError "contra-tracer"))
            (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
            (hsPkgs."fingertree" or (errorHandler.buildDepError "fingertree"))
            (hsPkgs."freer-extras" or (errorHandler.buildDepError "freer-extras"))
            (hsPkgs."freer-simple" or (errorHandler.buildDepError "freer-simple"))
            (hsPkgs."hedgehog" or (errorHandler.buildDepError "hedgehog"))
            (hsPkgs."lens" or (errorHandler.buildDepError "lens"))
            (hsPkgs."resource-pool" or (errorHandler.buildDepError "resource-pool"))
            (hsPkgs."serialise" or (errorHandler.buildDepError "serialise"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hedgehog" or (errorHandler.buildDepError "tasty-hedgehog"))
            ];
          buildable = true;
          modules = [
            "Generators"
            "Plutus/ChainIndex/Emulator/DiskStateSpec"
            "Plutus/ChainIndex/Emulator/HandlersSpec"
            "Plutus/ChainIndex/HandlersSpec"
            "Util"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../plutus-chain-index-core; }