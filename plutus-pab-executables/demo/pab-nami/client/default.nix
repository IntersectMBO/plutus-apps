{ pkgs, gitignore-nix, haskell, webCommon, buildPursPackage, buildNodeModules, filterNpm }:
let
  pab-nami-demo-invoker = haskell.packages.plutus-pab-executables.components.exes.plutus-pab-nami-demo;
  pab-nami-demo-generator = haskell.packages.plutus-pab-executables.components.exes.plutus-pab-nami-demo-generator;


  # TODO: Use the PS generator in the demo app
  generated-purescript = pkgs.runCommand "pab-nami-demo-purescript" { } ''
    mkdir $out
    ${pab-nami-demo-generator}/bin/plutus-pab-nami-demo-generator --output-dir $out
  '';

  generate-purescript = pkgs.writeShellScriptBin "pab-nami-demo-generate-purs" ''
    generatedDir=./generated
    rm -rf $generatedDir
    $(nix-build ../../../../default.nix -A pab-nami-demo.pab-nami-demo-generator)/bin/plutus-pab-nami-demo-generator --output-dir $generatedDir
  '';

  start-backend = pkgs.writeShellScriptBin "pab-nami-demo-server" ''
    echo "pab-nami-demo-server: for development use only"
    $(nix-build ../../../../default.nix --quiet --no-build-output -A pab-nami-demo.pab-nami-demo-invoker)/bin/plutus-pab-nami-demo --config ../pab/plutus-pab.yaml migrate
    $(nix-build ../../../../default.nix --quiet --no-build-output -A pab-nami-demo.pab-nami-demo-invoker)/bin/plutus-pab-nami-demo --config ../pab/plutus-pab.yaml webserver
  '';

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
      checkPhase = ''
        node -e 'require("./output/Test.Main").main()'
      '';
      name = "pab-nami-demo";
      spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    })
    (_: {
      WEB_COMMON_SRC = webCommon;
    });
in
{
  inherit client pab-nami-demo-invoker pab-nami-demo-generator generate-purescript generated-purescript start-backend;
}
