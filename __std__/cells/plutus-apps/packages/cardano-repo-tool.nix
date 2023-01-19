{ inputs, cell }:

let
  project = cell.library.pkgs.haskell-nix.cabalProject' {
    src = inputs.cardano-repo-tool.outPath;

    compiler-nix-name = cell.library.ghc-compiler-nix-name;

    index-state = cell.library.cabal-project-index-state;

    sha256map = {
      "https://github.com/input-output-hk/nix-archive"."7dcf21b2af54d0ab267f127b6bd8fa0b31cfa49d" = "0mhw896nfqbd2iwibzymydjlb3yivi9gm0v2g1nrjfdll4f7d8ly";
    };
  };
in
project.hsPkgs.cardano-repo-tool.components.exes.cardano-repo-tool
