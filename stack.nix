{ nixpkgs ? import <nixpkgs> {}, ghc }:
    let
        buildStackProject = nixpkgs.haskell.lib.buildStackProject.override { inherit ghc;};
    in
        with nixpkgs;

        buildStackProject {
            name = builtins.trace ghc.name "servant-subscriber-env";
            # ncurses needed for intero
            buildInputs = [ gcc haskell.packages.lts-6_0.ghc-mod git ncurses zlib ];
            shellHook = "export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt";
        }
