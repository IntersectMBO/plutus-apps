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
    # We want to use stylish-haskell, hlint, and implicit-hie as standalone tools *and* through HLS. But we need to have consistent versions in both
    # cases, otherwise e.g. you could format the code in HLS and then have the CI complain that it's wrong.
    # The solution we use here is to:
    # a) Where we care (mostly just formatters), constrain the versions of tools which HLS uses explicitly
    # b) pull out the tools themselves from the HLS project so we can use them elsewhere
    cabalProjectLocal = ''
      constraints: stylish-haskell==0.13.0.0, hlint==3.2.7
      allow-newer: hls-stylish-haskell-plugin:stylish-haskell
    '';
    src = sources.haskell-language-server;
    inherit compiler-nix-name;
    sha256map = {
      "https://github.com/hsyl20/ghc-api-compat"."8fee87eac97a538dbe81ff1ab18cff10f2f9fa15" = "16bibb7f3s2sxdvdy2mq6w1nj1lc8zhms54lwmj17ijhvjys29vg";
      "https://github.com/haskell/lsp.git"."ef59c28b41ed4c5775f0ab0c1e985839359cec96" = "1whcgw4hhn2aplrpy9w8q6rafwy7znnp0rczgr6py15fqyw2fwb5";
    };
    modules = [{
      # Workaround for https://github.com/haskell/haskell-language-server/issues/1160
      packages.haskell-language-server.patches = lib.mkIf stdenv.isDarwin [ ../../patches/haskell-language-server-dynamic.patch ];
      # See https://github.com/haskell/haskell-language-server/pull/1382#issuecomment-780472005
      packages.ghcide.flags.ghc-patched-unboxed-bytecode = true;
    }];
  };
in
{
  inherit (hlsProject.hsPkgs) haskell-language-server hie-bios implicit-hie stylish-haskell hlint;
  inherit (cabalInstallProject.hsPkgs) cabal-install;
  inherit (cardanoRepoToolProject.hsPkgs) cardano-repo-tool;
}
