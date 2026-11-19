{ lib, buildDunePackage, lwt, uri, cohttp, cohttp-lwt, cohttp-lwt-unix, yojson
, notty-community, lwt_ssl, ca-certs }:

buildDunePackage {
  pname = "wallet_watcher";
  version = "0.2.0";

  src = lib.fileset.toSource {
    root = ./..;
    fileset = lib.fileset.unions [
      ../dune-project ../bin ../lib ../tui ../test ../config.json
    ];
  };

  # Propagated so consumers resolve the library's deps. conduit picks its SSL
  # backend when conduit itself is built; ca-certs supplies the trust roots.
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
