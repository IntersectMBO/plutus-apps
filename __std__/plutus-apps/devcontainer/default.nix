{ inputs, cell }@block:
{
  devcontainer-docker-image = import ./devcontainer-docker-image.nix block;

  devcontainer-push-docker-image = import ./devcontainer-push-docker-image.nix block;
}
