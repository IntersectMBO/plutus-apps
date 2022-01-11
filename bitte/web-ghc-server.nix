# Needed variables:
#  NOMAD_PORT_web_ghc_server
#  NOMAD_IP_web_ghc_server
{ writeShellScriptBin, web-ghc-server, symlinkJoin }:
writeShellScriptBin "entrypoint" ''
  exec -a web-ghc-server ${web-ghc-server}/bin/web-ghc-server webserver --port "$NOMAD_PORT_web_ghc_server" --bind "$NOMAD_IP_web_ghc_server"
''
