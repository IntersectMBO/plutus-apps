# TODO(std) DUP(except purs-tidy)

{ inputs, cell }:

let
  inherit (cell.library) pkgs;
in

inputs.pre-commit-hooks-nix.lib.run {

  src = pkgs.lib.cleanSource inputs.self;

  tools = {
    shellcheck = pkgs.shellcheck;
    stylish-haskell = cell.packages.stylish-haskell;
    nixpkgs-fmt = cell.packages.nixpkgs-fmt;
    cabal-fmt = cell.packages.cabal-fmt;
  };

  hooks = {
    stylish-haskell.enable = true;
    cabal-fmt.enable = true;
    shellcheck.enable = true;

    purs-tidy-hook = {
      enable = true;
      name = "purs-tidy";
      entry = "${cell.library.easy-ps.purs-tidy}/bin/purs-tidy format-in-place";
      files = "\\.purs$";
      language = "system";
    };

    editorconfig-checker = {
      enable = true;
      entry = "${pkgs.editorconfig-checker}/bin/editorconfig-checker";
    };

    nixpkgs-fmt = {
      enable = true;
      # While nixpkgs-fmt does exclude patterns specified in `.ignore` this
      # does not appear to work inside the hook. For now we have to thus
      # maintain excludes here *and* in `./.ignore` and *keep them in sync*.
      excludes =
        [
          ".*nix/pkgs/haskell/materialized.*/.*"
          ".*/spago-packages.nix$"
          ".*/packages.nix$"
        ];
    };

    png-optimization = {
      enable = true;
      name = "png-optimization";
      description = "Ensure that PNG files are optimized";
      entry = "${pkgs.optipng}/bin/optipng";
      files = "\\.png$";
    };
  };
}
