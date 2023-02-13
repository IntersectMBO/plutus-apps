{ inputs, cell }@block:
{
  hydra-jobs = import ./hydra-jobs.nix block;
}
