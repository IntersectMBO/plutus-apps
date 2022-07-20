{ purs-tidy, pkgs, lib, gitignore-nix, haskell, webCommon, buildPursPackage, buildNodeModules, filterNpm }:
let
  playground-exe = haskell.packages.plutus-playground-server.components.exes.plutus-playground-server;

  ghc-with-plutus = haskell.project.ghcWithPackages (ps: [ ps.plutus-core ps.plutus-tx ps.plutus-contract ps.plutus-ledger ps.playground-common ]);

  # We need these because we are not allowed to reference plutus-ledger inside 
  # the nix shell.   
  build-ghc-with-plutus = "$(nix-build --no-build-output -E '(import ./default.nix {}).plutus-apps.haskell.project.ghcWithPackages(ps: [ ps.plutus-core ps.plutus-tx ps.plutus-contract ps.plutus-ledger ps.playground-common ])')";

  build-playground-exe = "$(nix-build default.nix -A plutus-apps.haskell.packages.plutus-playground-server.components.exes.plutus-playground-server)";

  # generate-purescript: script to create purescript bridge code
  #
  # Note: We need to add ghc to the path because the purescript generator
  # actually invokes ghc to generate test data so we need ghc with the necessary deps
  generate-purescript = pkgs.writeShellApplication {
    name = "plutus-playground-generate-purs";
    runtimeInputs = [ pkgs.nix ];
    text = ''
      if [ "$#" -ne 1 ]; then
        echo usage: plutus-playground-generate-purs GENERATED_DIR
        exit 1
      fi

      generatedDir="$1"
      rm -rf "$generatedDir"

      GHC_WITH_PKGS="${build-ghc-with-plutus}"
      export PATH=$GHC_WITH_PKGS/bin:$PATH

      echo Generating purescript files in "$generatedDir"
      "${build-playground-exe}"/bin/plutus-playground-server psgenerator "$generatedDir"
      echo Done generating purescript files
      echo
      echo Formatting purescript files in "$generatedDir"
      ${purs-tidy}/bin/purs-tidy format-in-place "$generatedDir"
      echo Done formatting purescript files
    '';
  };

  purescript-generated = pkgs.stdenv.mkDerivation {
    name = "purescript-generated";
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out
      GHC_WITH_PKGS="${ghc-with-plutus}"
      export PATH=$GHC_WITH_PKGS/bin:$PATH
      "${playground-exe}"/bin/plutus-playground-server psgenerator "$out"
    '';
  };

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

    if [ ! -d "$generatedDir" ] || [ "$1" == "-g" ]; then 
      ${generate-purescript}/bin/plutus-playground-generate-purs "$generatedDir"
    fi 

    dirAge=$(datediff now "$(date -r "$generatedDir" +%F)")
    echo
    echo "*** Using Purescript files in $generatedDir which are $dirAge days old."
    echo "*** To regenerate, run plutus-playground-server -g"
    echo
    echo
    echo plutus-playground-server: for development use only
    
    GHC_WITH_PKGS="${build-ghc-with-plutus}"
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    export GITHUB_CALLBACK_PATH=https://localhost:8009/api/oauth/github/callback

    echo before "$1"
    test "$1" == "-g" && shift 1 # takes care of the -g flag
    echo after "$1"
    "${build-playground-exe}"/bin/plutus-playground-server webserver "$@"
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
  inherit client start-backend generate-purescript;
  server = playground-exe;
}
