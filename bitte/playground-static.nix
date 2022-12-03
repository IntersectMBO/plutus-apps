{ staticSite, linkFarm, symlinkJoin, docs }:
let
  shiftedDocs = linkFarm docs.name [{ name = "doc"; path = docs; }];
in
{ variant }: staticSite {
  root = (symlinkJoin {
    name = "${variant}-playground-client-and-docs";
    paths = [ shiftedDocs ];
  });
  port-name = "${variant}_playground_client";
}
