{ inputs, cell }@block:
{
  hydraJobs = import ./hydraJobs.nix block;
}
