{ inputs, cell }@block:
{
  ciJobs = import ./ciJobs.nix block;
}
