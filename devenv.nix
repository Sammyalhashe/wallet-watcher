{ pkgs, ... }:

{
  languages.ocaml.enable = true;

  packages = (with pkgs.ocamlPackages; [
    findlib
    lwt
    lwt_ppx
    cohttp-lwt-unix
    yojson
    notty-community
    nottui
    lwd
    ca-certs
    ocaml-lsp
  ]) ++ [
    pkgs.dune_3
    pkgs.ocamlformat
    pkgs.jq
  ];

  enterShell = ''
    echo "OCaml + Dune + Notty dev environment loaded!"
  '';
}
