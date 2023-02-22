# TODO(std) DUP

{ inputs, cell }:

let
  haskell-language-server = cell.library.haskell-language-server-project.hsPkgs.haskell-language-server.components.exes.haskell-language-server; # editorconfig-checker-disable-line

  pkgs = cell.library.pkgs;
in
pkgs.stdenv.mkDerivation {
  name = "haskell-language-server-wrapper";
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    echo '#!${pkgs.bash}/bin/bash' > $out/bin/haskell-language-server-wrapper
    echo '${haskell-language-server}/bin/haskell-language-server "$@"' >> $out/bin/haskell-language-server-wrapper
    chmod u+x $out/bin/haskell-language-server-wrapper
  '';
}
