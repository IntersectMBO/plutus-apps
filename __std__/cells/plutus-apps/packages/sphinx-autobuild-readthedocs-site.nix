# TODO(std) DUP(almost: root/doc -> root/doc/read-the-docs-site)

{ inputs, cell }:

cell.library.pkgs.writeShellApplication {
  name = "autobuild-docs";
  runtimeInputs = [
    cell.packages.repo-root
    cell.packages.sphinx-toolchain
  ];
  text = ''
    export SPHINX_HADDOCK_DIR="${cell.packages.combined-plutus-apps-haddock}/share/doc"
    root="$(repo-root)"
    sphinx-autobuild -j 4 -n "$root/doc" "$root/doc/_build"
  '';
}
