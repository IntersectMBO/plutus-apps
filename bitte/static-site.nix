# Needed variables:
#  NOMAD_PORT_${port-name}
#  NOMAD_IP_${port-name}
{ writeShellScriptBin, symlinkJoin, lib, writeText, lighttpd }: { root, port-name }:

let
  config = writeText "lighttpd.conf" ''
    server.modules = ("mod_deflate")
    server.document-root = "${root}"
    server.port = env.NOMAD_PORT_${port-name}
    server.bind = env.NOMAD_IP_${port-name}
    index-file.names = ("index.html")
    mimetype.assign = (
      ".css" => "text/css",
      ".jpg" => "image/jpeg",
      ".jpeg" => "image/jpeg",
      ".html" => "text/html",
      ".js" => "text/javascript",
      ".svg" => "image/svg+xml",
    )
    deflate.cache-dir = "/tmp"
    deflate.mimetypes = ("text/plain", "text/html", "text/css")
    server.upload-dirs = ("/tmp")
  '';
in
writeShellScriptBin "entrypoint" ''
  exec -a lighttpd ${lighttpd}/bin/lighttpd -f ${config} -D
''
