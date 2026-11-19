{ lib, buildDunePackage, lwt, uri, cohttp, cohttp-lwt, cohttp-lwt-unix, yojson
, notty-community, lwt_ssl, ca-certs }:

buildDunePackage {
  pname = "wallet_watcher";
  version = "0.2.0";

  src = lib.cleanSourceWith {
    src = ./..;
    filter = path: _type:
      !(builtins.elem (baseNameOf path) [ "_build" ".devenv" ".direnv" ".jj" ".git" "result" ]);
  };

  # Propagated so consumers resolve the library's deps. conduit only offers
  # HTTPS when lwt_ssl is linked; ca-certs supplies the trust roots.
  propagatedBuildInputs = [
    lwt uri cohttp cohttp-lwt cohttp-lwt-unix yojson notty-community
    lwt_ssl ca-certs
  ];

  outputs = [ "out" ];

  doCheck = true;

  meta = {
    description = "Multi-chain crypto wallet balances and distribution";
    mainProgram = "wallet-watcher";
  };
}
