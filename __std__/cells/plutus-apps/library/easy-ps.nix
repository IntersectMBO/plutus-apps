{ inputs, cell }:

# easy-purescript-nix has some kind of wacky internal IFD
# usage that breaks the logic that makes source fetchers
# use native dependencies. This isn't easy to fix, since
# the only places that need to use native dependencies
# are deep inside, and we don't want to build the whole
# thing native. Fortunately, we only want to build the
# client on Linux, so that's okay. However, it does
# mean that e.g. we can't build the client dep updating
# script on Darwin.
cell.library.pkgs.callPackage inputs.easy-purescript-nix { }
