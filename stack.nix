with (import <nixpkgs> {});
let
  haskellPackages = haskell.packages.lts-6_7;
  ghc = haskellPackages.ghc;
in
  haskell.lib.buildStackProject {
  name = "myEnv";
  #    buildInputs = [ gcc git zlib pkgconfig ghc glibcLocales ];
  buildInputs = [ zlib haskellPackages.ghc-mod postgresql ];
    ghc = ghc;
    shellHook = "export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt";
}
