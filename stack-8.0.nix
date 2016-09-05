with (import <nixpkgs> {});
let
  ghc = haskellPackages.ghc;
in
haskell.lib.buildStackProject {
  name = "myEnv";
#    buildInputs = [ gcc git zlib pkgconfig ghc glibcLocales ];
  buildInputs = [ zlib haskellPackages.ghc-mod ];
  ghc = ghc;
  shellHook = "export SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt";
}
