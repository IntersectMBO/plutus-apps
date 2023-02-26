{ inputs, cell }@block:
{
  pkgs = import ./pkgs.nix block;

  combine-haddock = import ./combine-haddock.nix block;

  easy-ps = import ./easy-ps.nix block;

  plutus-apps-project = import ./plutus-apps-project.nix block;

  plutus-core-objects-inv = import ./plutus-core-objects-inv.nix block;

  make-plutus-apps-project = import ./make-plutus-apps-project.nix block;

  gitignore-source = import ./gitignore-source.nix block;

  haskell-language-server-project = import ./haskell-language-server-project.nix block;

  ghc-compiler-nix-name = import ./ghc-compiler-nix-name.nix block;

  cabal-project-index-state = import ./cabal-project-index-state.nix block;

  cardano-node = import ./cardano-node.nix block;

  bitte-packages = import ./bitte-packages.nix block;
}
