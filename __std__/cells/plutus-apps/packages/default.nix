{ inputs, cell }@block:
{
  combined-plutus-apps-haddock = import ./combined-plutus-apps-haddock.nix block;

  read-the-docs-site = import ./read-the-docs-site.nix block;

  serve-readthedocs-site = import ./serve-readthedocs-site.nix block;

  sphinx-markdown-tables = import ./sphinx-markdown-tables.nix block;

  sphinx-toolchain = import ./sphinx-toolchain.nix block;

  sphinxcontrib-bibtex = import ./sphinxcontrib-bibtex.nix block;

  sphinxemoji = import ./sphinxemoji.nix block;

  cabal-install = import ./cabal-install.nix block;

  check-the-flake = import ./check-the-flake.nix block;

  cabal-fmt = import ./cabal-fmt.nix block;

  fix-cabal-fmt = import ./fix-cabal-fmt.nix block;

  fix-png-optimization = import ./fix-png-optimization.nix block;

  fix-stylish-haskell = import ./fix-stylish-haskell.nix block;

  ghc = import ./ghc.nix block;

  haskell-language-server-wrapper = import ./haskell-language-server-wrapper.nix block;

  hie-bios = import ./hie-bios.nix block;

  hlint = import ./hlint.nix block;

  nixpkgs-fmt = import ./nixpkgs-fmt.nix block;

  pre-commit-check = import ./pre-commit-check.nix block;

  repo-root = import ./repo-root.nix block;

  stylish-haskell = import ./stylish-haskell.nix block;

  sphinx-autobuild-readthedocs-site = import ./sphinx-autobuild-readthedocs-site.nix block;

  sphinx-build-readthedocs-site = import ./sphinx-build-readthedocs-site.nix block;

  plutus-use-cases = import ./plutus-use-cases.nix block;

  plutus-uniswap = import ./plutus-uniswap.nix block;

  pab-cli = import ./pab-cli.nix block;

  plutus-chain-index = import ./plutus-chain-index.nix block;

  plutus-playground-client-entrypoint = import ./plutus-playground-client-entrypoint.nix block;

  create-script-context = import ./create-script-context.nix block;

  cardano-wallet = import ./cardano-wallet.nix block;

  scriv = import ./scriv.nix block;

  inherit (import ./sphinxcontrib-haddock.nix block)

    sphinxcontrib-domaintools

    sphinxcontrib-haddock

    sphobjinv;

}
