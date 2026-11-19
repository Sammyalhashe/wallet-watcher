{
  description = "wallet_watcher: multi-chain crypto wallet balances and distribution";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      overlay = final: prev: {
        ocamlPackages = prev.ocamlPackages.overrideScope (ofinal: _: {
          wallet_watcher = ofinal.callPackage ./nix/package.nix { };
        });
      };
    in
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "aarch64-linux"
      "aarch64-darwin"
      "x86_64-darwin"
    ]
      (system:
        let
          pkgs = import nixpkgs { inherit system; overlays = [ self.overlays.default ]; };
          app = pkgs.ocamlPackages.wallet_watcher;
        in
        {
          packages = {
            default = app;
            ocamlPackage = app;
          };
          apps.default = flake-utils.lib.mkApp { drv = app; };
        })
    // {
      overlays.default = overlay;
      homeManagerModules.default = import ./nix/hm-module.nix self;
    };
}
