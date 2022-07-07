{ system ? builtins.currentSystem
, enableHaskellProfiling ? false
, sourcesOverride ? { }
, sources ? import ./nix/sources.nix { inherit system; } // sourcesOverride
, packages ? import ./. { inherit system enableHaskellProfiling sources sourcesOverride; }
}:
let
  inherit (packages) pkgs plutus-apps plutus-playground pab-nami-demo docs webCommon;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt glibcLocales;
  inherit (plutus-apps) haskell stylish-haskell sphinxcontrib-haddock sphinx-markdown-tables sphinxemoji nix-pre-commit-hooks cabal-fmt;

  # Feed cardano-wallet, cardano-cli & cardano-node to our shell.
  # This is stable as it doesn't mix dependencies with this code-base;
  # the fetched binaries are the "standard" builds that people test.
  # This should be fast as it mostly fetches Hydra caches without building much.
  cardano-wallet = (import sources.flake-compat {
    inherit pkgs;
    src = builtins.fetchTree
      {
        type = "github";
        owner = "input-output-hk";
        repo = "cardano-wallet";
        rev = "f6d4db733c4e47ee11683c343b440552f59beff7";
        narHash = "sha256-3oeHsrAhDSSKBSzpGIAqmOcFmBdAJ5FR02UXPLb/Yz0=";
      };
  }).defaultNix;
  cardano-node = import
    (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      # A standard release compatible with the cardano-wallet commit above is always preferred.
      rev = "2b1d18c6c7b7142d9eebfec34da48840ed4409b6";
      sha256 = "102pj525ysvj27h9nb8gidxm1cmwp8vpdyfnpwm1ywz3zkpk2mjp";
    })
    { };

  # For Sphinx, and ad-hoc usage
  sphinxTools = python3.withPackages (ps: [
    sphinxcontrib-haddock.sphinxcontrib-domaintools
    sphinx-markdown-tables
    sphinxemoji
    ps.sphinxcontrib_plantuml
    ps.sphinxcontrib-bibtex
    ps.sphinx-autobuild
    ps.sphinx
    ps.sphinx_rtd_theme
    ps.recommonmark
  ]);

  # Configure project pre-commit hooks
  pre-commit-check = nix-pre-commit-hooks.run {
    src = (lib.cleanSource ./.);
    tools = {
      stylish-haskell = stylish-haskell;
      nixpkgs-fmt = nixpkgs-fmt;
      shellcheck = pkgs.shellcheck;
      cabal-fmt = cabal-fmt;
    };
    hooks = {
      purs-tidy-hook = {
        enable = true;
        name = "purs-tidy";
        entry = "${plutus-apps.purs-tidy}/bin/purs-tidy format-in-place";
        files = "\\.purs$";
        language = "system";
      };
      stylish-haskell.enable = true;
      nixpkgs-fmt = {
        enable = true;
        # While nixpkgs-fmt does exclude patterns specified in `.ignore` this
        # does not appear to work inside the hook. For now we have to thus
        # maintain excludes here *and* in `./.ignore` and *keep them in sync*.
        excludes = [ ".*nix/pkgs/haskell/materialized.*/.*" ".*/spago-packages.nix$" ];
      };
      cabal-fmt.enable = true;
      shellcheck.enable = true;
      png-optimization = {
        enable = true;
        name = "png-optimization";
        description = "Ensure that PNG files are optimized";
        entry = "${pkgs.optipng}/bin/optipng";
        files = "\\.png$";
      };
    };
  };

  nixFlakesAlias = pkgs.runCommand "nix-flakes-alias" { } ''
    mkdir -p $out/bin
    ln -sv ${pkgs.nixFlakes}/bin/nix $out/bin/nix-flakes
  '';

  # build inputs from nixpkgs ( -> ./nix/default.nix )
  nixpkgsInputs = with pkgs; [
    awscli2
    bzip2
    cacert
    editorconfig-core-c
    ghcid
    jq
    nixFlakesAlias
    nixpkgs-fmt
    cabal-fmt
    nodejs
    plantuml
    # See https://github.com/cachix/pre-commit-hooks.nix/issues/148 for why we need this
    pre-commit
    shellcheck
    sqlite-interactive
    stack
    wget
    yq
    z3
    zlib
  ];

  # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with plutus-apps; [
    cabal-install
    cardano-node.cardano-cli
    cardano-node.cardano-node
    cardano-wallet.packages.${pkgs.system}.cardano-wallet
    cardano-repo-tool
    docs.build-and-serve-docs
    fixPngOptimization
    fix-purs-tidy
    fixCabalFmt
    fixStylishHaskell
    haskell-language-server
    haskell-language-server-wrapper
    hie-bios
    hlint
    pab-nami-demo.generate-purescript
    pab-nami-demo.start-backend
    plutus-playground.generate-purescript
    plutus-playground.start-backend
    psa
    purescript-language-server
    purs
    purs-tidy
    spago
    spago2nix
    stylish-haskell
    updateClientDeps
  ]);

in
haskell.project.shellFor {
  nativeBuildInputs = nixpkgsInputs ++ localInputs ++ [ sphinxTools ];
  # We don't currently use this, and it's a pain to materialize, and otherwise
  # costs a fair bit of eval time.
  withHoogle = false;

  shellHook = ''
    ${pre-commit-check.shellHook}
  ''
  # Work around https://github.com/NixOS/nix/issues/3345, which makes
  # tests etc. run single-threaded in a nix-shell.
  # Sets the affinity to cores 0-1000 for $$ (current PID in bash)
  # Only necessary for linux - darwin doesn't even expose thread
  # affinity APIs!
  + lib.optionalString stdenv.isLinux ''
    ${utillinux}/bin/taskset -pc 0-1000 $$
  ''
  + ''
    export WEB_COMMON_SRC=${webCommon.cleanSrc}
  '';

  # This is no longer set automatically as of more recent `haskell.nix` revisions,
  # but is useful for users with LANG settings.
  LOCALE_ARCHIVE = lib.optionalString
    (stdenv.hostPlatform.libc == "glibc")
    "${glibcLocales}/lib/locale/locale-archive";
}
