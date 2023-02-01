############################################################################
# Extra Haskell packages which we build with haskell.nix, but which aren't
# part of our project's package set themselves.
#
# These are for e.g. developer usage, or for running formatting tests.
############################################################################
{ stdenv
, lib
, haskell-nix
, sources
, index-state
, compiler-nix-name
, buildPackages
, writeShellScript
}:
let
  # TODO Remove this patch once the PR gets merged upstream.
  # See https://github.com/phadej/cabal-fmt/pull/45
  cabalFmtProject = haskell-nix.cabalProject' {
    src = buildPackages.fetchgit {
      url = "https://github.com/zeme-iohk/cabal-fmt.git";
      rev = "7b5c9b4fac55aad15a0b33bcd22b40a244bf30af";
      sha256 = "04w1dy83ml7wgm5ay1rd4kiwfmdd9sc2y8bp3l0ja7xwvh4fgkmr";
    };
    # Cabal is a boot library, so haskell.nix would normally use the one coming
    # from the compiler-nix-name (currently 3.2). However cabal-fmt depends on 
    # Cabal library version 3.6, hence we add this line.
    modules = [{ reinstallableLibGhc = true; }];
    inherit compiler-nix-name index-state;
  };
  cabalInstallProject = haskell-nix.hackage-project {
    name = "cabal-install";
    version = "3.6.2.0";
    inherit compiler-nix-name index-state;
  };
  cardanoRepoToolProject = haskell-nix.cabalProject' {
    src = sources.cardano-repo-tool;
    inherit compiler-nix-name index-state;
    sha256map = {
      "https://github.com/input-output-hk/nix-archive"."7dcf21b2af54d0ab267f127b6bd8fa0b31cfa49d" = "0mhw896nfqbd2iwibzymydjlb3yivi9gm0v2g1nrjfdll4f7d8ly";
    };
  };
  hlsProject = haskell-nix.cabalProject' {
    # See https://github.com/haskell/haskell-language-server/issues/411.
    # We want to use stylish-haskell, hlint, and implicit-hie as standalone tools
    # *and* through HLS. But we need to have consistent versions in both cases,
    # otherwise e.g. you could format the code in HLS and then have the CI
    # complain that it's wrong
    #
    # The solution we use here is to:
    # a) Where we care (mostly just formatters), constrain the versions of
    #    tools which HLS uses explicitly
    # b) Pull out the tools themselves from the HLS project so we can use
    #    them elsewhere


    src = sources.haskell-language-server;
    sha256map = {
      "https://github.com/pepeiborra/ekg-json"."7a0af7a8fd38045fd15fb13445bdcc7085325460" = "sha256-fVwKxGgM0S4Kv/4egVAAiAjV7QB5PBqMVMCfsv7otIQ="; # editorconfig-checker-disable-line
    };

    # inherit compiler-nix-name;

    modules = [{
      # See https://github.com/haskell/haskell-language-server/pull/1382#issuecomment-780472005
      packages.ghcide.flags.ghc-patched-unboxed-bytecode = true;
    }];
  };
in
{
  inherit (hlsProject.hsPkgs) haskell-language-server hie-bios implicit-hie stylish-haskell hlint;
  inherit (cabalInstallProject.hsPkgs) cabal-install;
  inherit (cardanoRepoToolProject.hsPkgs) cardano-repo-tool;
  inherit (cabalFmtProject.hsPkgs) cabal-fmt;
}
