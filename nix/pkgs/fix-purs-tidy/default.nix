{ writeShellScriptBin, fd, purs-tidy }:

writeShellScriptBin "fix-purs-tidy" ''
  set -e
  echo formatting PureScript files...
  ${fd}/bin/fd \
    --extension purs \
    --exec-batch ${purs-tidy}/bin/purs-tidy format-in-place {}
  echo done.
''
