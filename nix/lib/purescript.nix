{ stdenv
, nodejs
, easyPS
}:

{ pkgs
  # path to project sources
, src
  # name of the project
, name
  # packages as generated by psc-pacakge2nix
, packages
  # spago packages as generated by spago2nix
, spagoPackages
  # a map of source directory name to contents that will be symlinked into the environment before building
, extraSrcs ? { }
  # node_modules to use
, nodeModules
  # control execution of unit tests
, checkPhase
}:
let
  addExtraSrc = k: v: "ln -sf ${v} ${k}";
  addExtraSrcs = builtins.concatStringsSep "\n" (builtins.attrValues (pkgs.lib.mapAttrs addExtraSrc extraSrcs));
  extraPSPaths = builtins.concatStringsSep " " (map (d: "'${d}/**/*.purs'") (builtins.attrNames extraSrcs));
  getGlob = pkg: ''".spago/${pkg.name}/${pkg.version}/src/**/*.purs"'';
  spagoSources = builtins.toString (builtins.map getGlob (builtins.attrValues spagoPackages.inputs));
in
stdenv.mkDerivation {
  inherit name src checkPhase;
  buildInputs = [
    nodejs
    nodeModules
    easyPS.purs
    easyPS.spago
    easyPS.psc-package
    easyPS.psa
    spagoPackages.installSpagoStyle
  ];
  buildPhase = ''
    export HOME=$NIX_BUILD_TOP
    shopt -s globstar
    cp -R ${nodeModules}/node_modules .
    chmod -R u+rw ./node_modules
    ${addExtraSrcs}

    install-spago-style
    echo building project...
    psa compile --strict --censor-lib --stash --is-lib=generated --is-lib=.spago ${spagoSources} ${extraPSPaths} "src/**/*.purs" "test/**/*.purs"
    echo done.
    npm run build:webpack:prod
  '';
  doCheck = true;
  installPhase = ''
    mv dist $out
  '';

  # The `nodeModules` we symlinked above contain `devDependencies` which
  # are only required at build-time and not at run-time. `npm prune --production`
  # removes all packages that belong to `devDependencies`.
  #
  # [Note 1]: that we have to run this during `postInstall` because 
  # `devDependencies` may be required during `checkPhase` which runs before.
  #
  # [Note 2]: *currently* the installPhase only uses `dist` but this may change
  # or may be overriden so we follow the best-practice of pruning dependencies here.
  postInstall = ''
    npm prune --production
  '';
}
