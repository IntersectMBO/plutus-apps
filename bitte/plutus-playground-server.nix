# Needed variables:
#  NOMAD_PORT_${variant}_playground_server
{ writeShellScriptBin, pkg, variant, symlinkJoin, lib, cacert, z3 }:
writeShellScriptBin "entrypoint" ''
  export PATH=${lib.makeBinPath [ pkg z3 ]}
  export SYSTEM_CERTIFICATE_PATH=${cacert}/etc/ssl/certs/ca-bundle.crt
  exec ${variant}-playground-server webserver -p $NOMAD_PORT_${variant}_playground_server
''
