# TODO(std) DUP 

{ inputs, cell }:

cell.library.pkgs.callPackage inputs.sphinxcontrib-haddock {
  pythonPackages = cell.library.pkgs.python3Packages;
}
