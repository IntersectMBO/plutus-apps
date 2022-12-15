# TODO(std) DUP(almost, list of suffices in src differs)

{ inputs, cell }:

let
  inherit (cell.library) pkgs;
in

pkgs.stdenv.mkDerivation {
  name = "read-the-docs-site";

  src = pkgs.lib.sourceFilesBySuffices
    (inputs.self + /doc)
    [ ".py" ".rst" ".md" ".hs" ".png" ".svg" ".bib" ".csv" ".css" ".html" ];

  buildInputs = [
    cell.packages.sphinx-toolchain
    # We need this here in order to get the `plantuml` executable in PATH.
    # Unfortunately `python3.withPackages` (used by cell.packages.sphinx-toolchain above)
    # won't do it automatically.
    pkgs.python3Packages.sphinxcontrib_plantuml
  ];

  dontInstall = true;

  # TODO(std) fix once we have combined-plutus-apps-haddock
  # cp -aR ${cell.packages.combined-plutus-apps-haddock}/share/doc haddock
  buildPhase = ''
    mkdir haddock # FIXME (see above)
    # -n gives warnings on missing link targets, -W makes warnings into errors
    SPHINX_HADDOCK_DIR=haddock sphinx-build -n -W . $out
    cp -aR haddock $out
  '';
}
