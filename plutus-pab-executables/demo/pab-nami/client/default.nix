{ purs-tidy, pkgs, gitignore-nix, haskell, webCommon, buildPursPackage, buildNodeModules, filterNpm }:
let
  pab-nami-demo-invoker = haskell.packages.plutus-pab-executables.components.exes.plutus-pab-nami-demo;
  pab-nami-demo-generator = haskell.packages.plutus-pab-executables.components.exes.plutus-pab-nami-demo-generator;

  pab-setup-invoker = haskell.packages.plutus-pab-executables.components.exes.plutus-pab-setup;

  build-pab-nami-demo-generator = "$(nix-build default.nix -A pab-nami-demo.pab-nami-demo-generator)";

  build-pab-nami-demo-invoker = "$(nix-build default.nix --no-build-output -A pab-nami-demo.pab-nami-demo-invoker)";

  generate-purescript = pkgs.writeShellApplication {
    name = "pab-nami-demo-generate-purs";
    runtimeInputs = [ pkgs.nix ];
    text = ''
      if [ "$#" -ne 1 ]; then
        echo usage: pab-nami-demo-generate-purs GENERATED_DIR
        exit 1
      fi

      generatedDir="$1"
      rm -rf "$generatedDir"

      echo Generating purescript files in "$generatedDir"
      "${build-pab-nami-demo-generator}"/bin/plutus-pab-nami-demo-generator --output-dir "$generatedDir"
      echo Done generating purescript files
      echo
      echo Formatting purescript files in "$generatedDir"
      ${purs-tidy}/bin/purs-tidy format-in-place "$generatedDir"
      echo Done formatting purescript files
    '';
  };

  purescript-generated = pkgs.stdenv.mkDerivation {
    name = "purescript-generated";
    buildInputs = [ pab-nami-demo-generator ];
    phases = [ "installPhase" ];
    installPhase = ''
      mkdir -p $out 
      plutus-pab-nami-demo-generator --output-dir "$out"
    '';
  };

  # purescript-generated = pkgs.runCommand "pab-nami-demo-generate-purs" { } ''
  #   "${pab-nami-demo-generator}"/bin/plutus-pab-nami-demo-generator --output-dir "$out"
  # '';

  start-backend = pkgs.writeShellScriptBin "pab-nami-demo-server" ''
    if [ ! -d plutus-pab-executables ]; then 
      echo Please run pab-nami-demo-server from the root of the repository
      exit 1
    fi

    generatedDir=./plutus-pab-executables/demo/pab-nami/client/generated

    if [ ! -d "$generatedDir" ] || [ "$1" == "-g" ]; then 
      ${generate-purescript}/bin/pab-nami-demo-generate-purs "$generatedDir"
    fi 

    dirAge=$(datediff now "$(date -r "$generatedDir" +%F)")
    echo
    echo "*** Using Purescript files in $generatedDir which are $dirAge days old."
    echo "*** To regenerate, run pab-nami-demo-server -g"
    echo
    echo
    echo pab-nami-demo-server: for development use only

    configFile=./plutus-pab-executables/demo/pab-nami/pab/plutus-pab.yaml

    "${build-pab-nami-demo-invoker}"/bin/plutus-pab-nami-demo --config "$configFile" migrate
    "${build-pab-nami-demo-invoker}"/bin/plutus-pab-nami-demo --config "$configFile" webserver
  '';

  # Note that this ignores the generated folder too, but it's fine since it is 
  # added via extraSrcs 
  cleanSrc = gitignore-nix.gitignoreSource ./.;

  nodeModules = buildNodeModules {
    projectDir = filterNpm cleanSrc;
    packageJson = ./package.json;
    packageLockJson = ./package-lock.json;
    githubSourceHashMap = { };
  };

  client = pkgs.lib.overrideDerivation
    (buildPursPackage {
      inherit pkgs nodeModules;
      src = cleanSrc;
      extraSrcs = {
        generated = purescript-generated;
      };
      checkPhase = ''
        node -e 'require("./output/Test.Main").main()'
        ls -la ${pab-nami-demo-generator}
      '';
      name = "pab-nami-demo";
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    })
    (_: {
      WEB_COMMON_SRC = webCommon.cleanSrc;
    });
in
{
  inherit client pab-nami-demo-invoker pab-setup-invoker start-backend pab-nami-demo-generator;
}
