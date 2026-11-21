self:
{ config, lib, pkgs, ... }:
let
  inherit (lib) mkOption mkEnableOption mkIf types;
  cfg = config.programs.wallet-watcher;

  networks = [ "bitcoin" "ethereum" "base" "avalanche" "solana" "sui" "zcash" "bittensor" ];

  walletType = types.submodule {
    options = lib.genAttrs networks (network: mkOption {
      type = types.nullOr types.str;
      default = null;
      description =
        if network == "bitcoin" then
          "Address on bitcoin, or an output descriptor scanned by your bitcoind (see bitcoind)."
        else
          "Address on ${network}.";
    });
  };

  # Files, not strings, so secrets never enter the world-readable store.
  secretFiles = lib.filterAttrs (_: file: file != null) {
    TAOSTATS_API_KEY = cfg.taostatsApiKeyFile;
    ZERION_API_KEY = cfg.zerionApiKeyFile;
    BITCOIND_RPC_PASSWORD = cfg.bitcoind.passwordFile;
  };

  literals = lib.filterAttrs (_: value: value != null) {
    BITCOIND_RPC_URL = cfg.bitcoind.url;
    BITCOIND_RPC_USER = cfg.bitcoind.user;
  };

  wrapperArgs =
    lib.mapAttrsToList (name: file:
      "--run ${lib.escapeShellArg "if [ -r ${lib.escapeShellArg file} ]; then export ${name}=\"$(cat ${lib.escapeShellArg file})\"; fi"}")
      secretFiles
    ++ lib.mapAttrsToList (name: value: "--set ${name} ${lib.escapeShellArg value}") literals;

  wrapped = pkgs.symlinkJoin {
    name = "wallet-watcher-with-env";
    paths = [ cfg.package ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/wallet-watcher ${lib.concatStringsSep " " wrapperArgs}
    '';
    inherit (cfg.package) meta;
  };

  renderedConfig = builtins.toJSON {
    wallets = lib.mapAttrsToList (label: addresses: {
      inherit label;
      addresses = lib.filterAttrs (_: address: address != null) addresses;
    }) cfg.wallets;
  };
in
{
  options.programs.wallet-watcher = {
    enable = mkEnableOption "wallet-watcher, a multi-chain wallet distribution TUI";

    package = mkOption {
      type = types.package;
      default = self.packages.${pkgs.stdenv.hostPlatform.system}.default;
      description = "The wallet-watcher package to install.";
    };

    wallets = mkOption {
      type = types.attrsOf walletType;
      default = { };
      example = lib.literalExpression ''
        { Ledger = { bitcoin = "bc1q..."; ethereum = "0x..."; base = "0x..."; }; }
      '';
      description = ''
        Wallets keyed by label, each with at most one address per network.
        Values may be sops placeholders when the result is rendered by a sops
        template (see configJson and configFile).
      '';
    };

    configJson = mkOption {
      type = types.str;
      readOnly = true;
      default = renderedConfig;
      description = "The config.json rendered from wallets, e.g. for a sops template's content.";
    };

    taostatsApiKeyFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "/run/secrets/taostats_api_key";
      description = ''
        File holding a Taostats API key, read at launch to enable Bittensor (TAO)
        balances. Without it, TAO rows show as unavailable.
      '';
    };

    zerionApiKeyFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "/run/secrets/zerion_api_key";
      description = ''
        File holding a Zerion API key, read at launch. With it, ethereum, base
        and avalanche entries show ERC-20 tokens and DeFi positions; without
        it, only native balances.
      '';
    };

    bitcoind = {
      url = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "http://127.0.0.1:8332";
        description = "RPC URL of the bitcoind that scans bitcoin descriptors.";
      };
      user = mkOption {
        type = types.nullOr types.str;
        default = null;
        description = "RPC user; it needs scantxoutset in its whitelist.";
      };
      passwordFile = mkOption {
        type = types.nullOr types.str;
        default = null;
        example = "/run/secrets/bitcoind_rpc_password";
        description = "File holding the RPC password, read at launch.";
      };
    };

    configFile = mkOption {
      type = types.nullOr types.str;
      default = null;
      description = ''
        Path of an already-rendered config.json, such as a sops template path.
        When set it is linked into place instead of writing configJson to the store,
        which keeps real addresses out of the world-readable Nix store.
      '';
    };
  };

  config = mkIf cfg.enable {
    home.packages = [
      (if wrapperArgs == [ ] then cfg.package else wrapped)
    ];

    xdg.configFile."wallet-watcher/config.json" =
      if cfg.configFile != null then
        { source = config.lib.file.mkOutOfStoreSymlink cfg.configFile; }
      else
        mkIf (cfg.wallets != { }) { text = cfg.configJson; };
  };
}
