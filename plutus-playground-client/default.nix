{ purs-tidy, pkgs, lib, gitignore-nix, haskell, webCommon, buildPursPackage, buildNodeModules, filterNpm }:
let
  playground-exe = haskell.packages.plutus-playground-server.components.exes.plutus-playground-server;

  ghcWithPlutus = haskell.project.ghcWithPackages (ps: [ ps.plutus-core ps.plutus-tx ps.plutus-contract ps.plutus-ledger ps.playground-common ]);

  # generate-purescript: script to create purescript bridge code
  #
  # Note: We need to add ghc to the path because the purescript generator
  # actually invokes ghc to generate test data so we need ghc with the necessary deps
  generate-purescript = pkgs.writeShellScriptBin "plutus-playground-generate-purs" ''
    if [ ! -d plutus-playground-client ]; then 
      echo Please run plutus-playground-generate-purs from the root of the repository
      exit 1
    fi

    GHC_WITH_PKGS=${ghcWithPlutus}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    generatedDir=./plutus-playground-client/generated
    rm -rf $generatedDir

    echo Generating purescript files in $generatedDir
    ${playground-exe}/bin/plutus-playground-server psgenerator $generatedDir
    echo -e Done generating purescript files

    echo Formatting purescript files in $generatedDir
    ${purs-tidy}/bin/purs-tidy format-in-place $generatedDir
    echo Done formatting purescript files
  '';

  # start-backend: script to start the plutus-playground-server
  #
  # Note: We need to add ghc to the path because the server provides /runghc
  # which needs ghc and dependencies.
  start-backend = pkgs.writeShellScriptBin "plutus-playground-server" ''
    if [ ! -d plutus-playground-client ]; then 
      echo Please run plutus-playground-server from the root of the repository
      exit 1
    fi

    generatedDir=./plutus-playground-client/generated
    if [ ! -d $generatedDir ]; then 
      echo $generatedDir not found
      plutus-playground-generate-purs
    elif [ $# == 0 ]; then 
      dirAge=$(datediff now $(date -r $generatedDir +%F))
      echo Using Purescript files in $generatedDir which are $dirAge days old. 
      echo Run plutus-playground-generate-purs -g to regenerate
    elif [ "$1" == "-g" ]; then 
       plutus-playground-generate-purs
    fi 

    echo
    echo "plutus-playground-server: for development use only"
    GHC_WITH_PKGS=${ghcWithPlutus}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    export GITHUB_CALLBACK_PATH=https://localhost:8009/api/oauth/github/callback

    ${playground-exe}/bin/plutus-playground-server webserver "$@"
  '';

  cleanSrc = pkgs.lib.sources.sourceByRegex ./. [
    "e2e-tests.*"
    "static.*"
    "generated.*"
    "test.*"
    "src.*"
    "entry.js"
    "package-lock.json"
    "package.json"
    "spago-packages.nix"
    "spago.dhall"
    "webpack.config.js"
  ];

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
  };

  client = pkgs.lib.overrideDerivation
    (buildPursPackage {
      inherit pkgs nodeModules;
      src = cleanSrc;
      name = "plutus-playground-client";
      # ideally we would just use `npm run test` but
      # this executes `spago` which *always* attempts to download
      # remote files (which obviously fails in sandboxed builds)
      checkPhase = ''
        node -e 'require("./output/Test.Main").main()'
      '';
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    })
    (_: {
      WEB_COMMON_SRC = webCommon.cleanSrc;
    });
in
{
  inherit client generate-purescript start-backend;
  server = playground-exe;
}
