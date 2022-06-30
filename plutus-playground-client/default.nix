{ purs-tidy, pkgs, lib, gitignore-nix, haskell, webCommon, buildPursPackage, buildNodeModules, filterNpm }:
let
  playground-exe = haskell.packages.plutus-playground-server.components.exes.plutus-playground-server;

  ghcWithPlutus = haskell.project.ghcWithPackages (ps: [ ps.plutus-core ps.plutus-tx ps.plutus-contract ps.plutus-ledger ps.playground-common ]);

  # Output containing the purescript bridge code
  # We need to add ghc with dependecies because `psgenerator` needs to invoke ghc to
  # create test data.
  generated-purescript =
    # For some reason on darwin GHC will complain bout missing otool, I really don't know why
    pkgs.runCommand "plutus-playground-purescript" { buildInputs = lib.optional pkgs.stdenv.isDarwin [ pkgs.darwin.cctools ]; } ''
      PATH=${ghcWithPlutus}/bin:$PATH
      mkdir $out
      ${playground-exe}/bin/plutus-playground-server psgenerator $out
      cp ${builtins.path { name = "tidyrc.json"; path = ../.tidyrc.json; } } $out/.tidyrc.json
      cp ${builtins.path { name = "tidyoperators"; path = ../.tidyoperators; } } $out/.tidyoperators
      cd $out
      ${purs-tidy}/bin/purs-tidy format-in-place $out
      rm $out/.tidyrc.json
      rm $out/.tidyoperators
    '';

  # generate-purescript: script to create purescript bridge code
  #
  # * Note-1: We need to add ghc to the path because the purescript generator
  # actually invokes ghc to generate test data so we need ghc with the necessary deps
  generate-purescript = pkgs.writeShellScriptBin "plutus-playground-generate-purs" ''
    GHC_WITH_PKGS=${ghcWithPlutus}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    rm -rf ./generated
    ${playground-exe}/bin/plutus-playground-server psgenerator generated
    cd ..
    echo Formatting files...
    ${purs-tidy}/bin/purs-tidy format-in-place ./plutus-playground-client/generated
    echo Done: formatted
  '';

  # start-backend: script to start the plutus-playground-server
  #
  # Note-1: We need to add ghc to the path because the server provides /runghc
  # which needs ghc and dependencies.
  start-backend = pkgs.writeShellScriptBin "plutus-playground-server" ''
    echo "plutus-playground-server: for development use only"
    GHC_WITH_PKGS=${ghcWithPlutus}
    export PATH=$GHC_WITH_PKGS/bin:$PATH

    export FRONTEND_URL=https://localhost:8009
    export WEBGHC_URL=http://localhost:8080
    export GITHUB_CALLBACK_PATH=https://localhost:8009/api/oauth/github/callback

    ${playground-exe}/bin/plutus-playground-server webserver "$@"
  '';

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
  inherit client generate-purescript generated-purescript start-backend;
  server = playground-exe;
}
