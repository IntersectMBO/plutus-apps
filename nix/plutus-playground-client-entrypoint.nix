{ repoRoot, inputs, pkgs, lib, system }:

let
  docs = inputs.self.packages.read-the-docs-site;

  shiftedDocs = pkgs.linkFarm docs.name [{ name = "doc"; path = docs; }];

  root = pkgs.symlinkJoin {
    name = "plutus-playground-client-and-docs";
    paths = [ shiftedDocs ];
  };

  port-name = "plutus_playground_client";

  config = pkgs.writeText "lighttpd.conf" ''
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

  # Needed variables:
  #   NOMAD_PORT_${port-name}
  #   NOMAD_IP_${port-name}
  entrypoint = pkgs.writeShellScriptBin "entrypoint" ''
    exec -a lighttpd ${pkgs.lighttpd}/bin/lighttpd -f ${config} -D
  '';

in

entrypoint
