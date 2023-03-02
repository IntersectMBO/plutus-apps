{ inputs, cell }:

# For some reason plutus-core isn't de-systemized 
let
  read-the-docs-site = inputs.plutus-core.packages."${cell.library.pkgs.system}".read-the-docs-site;
in
"${read-the-docs-site}/objects.inv"
