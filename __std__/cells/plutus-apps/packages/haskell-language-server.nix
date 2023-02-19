# TODO(std) DUP

{ inputs, cell }:

let
  exes = cell.library.haskell-language-server-project.hsPkgs.haskell-language-server.components.exes; # editorconfig-checker-disable-line
in
cell.library.pkgs.stdenv.mkDerivation {
  name = "haskell-language-server";
  phases = [ "installPhase" ];
  installPhase = ''
    mkdir -p $out/bin
    cp -r ${exes.haskell-language-server}/bin/* $out/bin
    cp -r ${exes.haskell-language-server-wrapper}/bin/* $out/bin
  '';
}
