# TODO(std) DUP

{ inputs, cell }:

cell.library.pkgs.writeShellApplication {

  name = "fix-purs-tidy";

  runtimeInputs = [
    cell.library.pkgs.fd
    cell.library.easy-ps.purs-tidy
  ];

  text = ''
    fd --extension purs --exec-batch purs-tidy format-in-place {}
  '';
}
