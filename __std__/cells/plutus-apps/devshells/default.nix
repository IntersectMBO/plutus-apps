{ inputs, cell }@block: rec
{
  default = plutus-apps-shell;

  plutus-apps-shell = import ./plutus-apps-shell.nix block;
}
