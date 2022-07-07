{ purs-tidy, pkgs, lib, gitignore-nix, haskell, webCommon, buildPursPackage, buildNodeModules, filterNpm }:
let
  playground-exe = haskell.packages.plutus-playground-server.components.exes.plutus-playground-server;

  ghcWithPlutus = haskell.project.ghcWithPackages (ps: [ ps.plutus-core ps.plutus-tx ps.plutus-contract ps.plutus-ledger ps.playground-common ]);

  # generate-purescript: script to create purescript bridge code
  #
  # Note: We need to add ghc to the path because the purescript generator
  # actually invokes ghc to generate test data so we need ghc with the necessary deps
  generate-purescript = pkgs.writeShellScript "plutus-playground-generate-purs" ''
    if [ "$#" -ne 1 ]; then
      echo usage: plutus-playground-generate-purs GENERATED_DIR
      exit 1
    fi

    generatedDir="$1"
    rm -rf $generatedDir

    GHC_WITH_PKGS=${ghcWithPlutus}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    echo Generating purescript files in $generatedDir
    ${playground-exe}/bin/plutus-playground-server psgenerator $generatedDir
    echo Done generating purescript files
    echo
    echo Formatting purescript files in $generatedDir
    ${purs-tidy}/bin/purs-tidy format-in-place $generatedDir
    echo Done formatting purescript files
  '';

  purescript-generated = pkgs.runCommand "plutus-playground-generate-purs" { } ''
    ${generate-purescript} $out
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

    if [ ! -d $generatedDir ] || [ "$1" == "-g" ]; then 
      ${generate-purescript} $generatedDir
    fi 

    dirAge=$(datediff now $(date -r $generatedDir +%F))
    echo
    echo "*** Using Purescript files in $generatedDir which are $dirAge days old."
    echo "*** To regenerate, run plutus-playground-server -g"
    echo
    echo
    echo plutus-playground-server: for development use only
    
    GHC_WITH_PKGS=${ghcWithPlutus}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    export GITHUB_CALLBACK_PATH=https://localhost:8009/api/oauth/github/callback

    test "$1" == "-g" && shift 1 # takes care of the -g flag
    ${playground-exe}/bin/plutus-playground-server webserver "$@"
  '';

  # Note that this ignores the generated folder too, but it's fine since it is 
  # added via extraSrcs 
  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
  };

  client = pkgs.lib.overrideDerivation
    (buildPursPackage {
      inherit pkgs nodeModules;
      src = cleanSrc;
      extraSrcs = {
        generated = purescript-generated;
      };
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
  inherit client start-backend;
  server = playground-exe;
}
