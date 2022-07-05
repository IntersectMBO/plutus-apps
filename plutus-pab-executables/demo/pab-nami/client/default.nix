{ purs-tidy, pkgs, gitignore-nix, haskell, webCommon, buildPursPackage, buildNodeModules, filterNpm }:
let
  pab-nami-demo-invoker = haskell.packages.plutus-pab-executables.components.exes.plutus-pab-nami-demo;
  pab-nami-demo-generator = haskell.packages.plutus-pab-executables.components.exes.plutus-pab-nami-demo-generator;

  pab-setup-invoker = haskell.packages.plutus-pab-executables.components.exes.plutus-pab-setup;

  generate-purescript = pkgs.writeShellScriptBin "pab-nami-demo-generate-purs" ''
    if [ ! -d plutus-pab-executables ]; then 
      echo Please run pab-nami-demo-generate-purs from the root of the repository
      exit 1
    fi

    generatedDir=./plutus-pab-executables/demo/pab-nami/client/generated
    rm -rf $generatedDir

    echo Generating purescript files in $generatedDir
    ${pab-nami-demo-generator}/bin/plutus-pab-nami-demo-generator --output-dir $generatedDir
    echo -e Done generating purescript files

    echo Formatting purescript files in $generatedDir
    ${purs-tidy}/bin/purs-tidy format-in-place $generatedDir
    echo Done formatting purescript files
  '';

  start-backend = pkgs.writeShellScriptBin "pab-nami-demo-server" ''
    if [ ! -d plutus-pab-executables ]; then 
      echo Please run pab-nami-demo-server from the root of the repository
      exit 1
    fi

    generatedDir=./plutus-pab-executables/demo/pab-nami/client/generated
    if [ ! -d $generatedDir ]; then 
      echo $generatedDir not found
      pab-nami-demo-generate-purs
    elif [ $# == 0 ]; then 
      dirAge=$(datediff now $(date -r $generatedDir +%F))
      echo Using Purescript files in $generatedDir which are $dirAge days old. 
      echo Run pab-nami-demo-server -g to regenerate
    elif [ "$1" == "-g" ]; then 
      pab-nami-demo-generate-purs
    fi 

    echo
    echo "pab-nami-demo-server: for development use only"

    configFile=./plutus-pab-executables/demo/pab-nami/pab/plutus-pab.yaml

    ${pab-nami-demo-invoker}/bin/plutus-pab-nami-demo --config $configFile migrate
    ${pab-nami-demo-invoker}/bin/plutus-pab-nami-demo --config $configFile webserver
  '';

  cleanSrc = pkgs.lib.sources.sourceByRegex ./. [
    "static.*"
    "generated.*"
    "test.*"
    "src.*"
    "entry.js"
    "index.html"
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
    githubSourceHashMap = { };
  };

  client = pkgs.lib.overrideDerivation
    (buildPursPackage {
      inherit pkgs nodeModules;
      src = cleanSrc;
      checkPhase = ''
        node -e 'require("./output/Test.Main").main()'
      '';
      name = "pab-nami-demo";
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    })
    (_: {
      WEB_COMMON_SRC = webCommon.cleanSrc;
    });
in
{
  inherit client pab-nami-demo-invoker pab-nami-demo-generator pab-setup-invoker generate-purescript start-backend;
}
