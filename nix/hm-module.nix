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
      description = "Address on ${network}.";
    });
  };

  # A file, not a string, so the key never enters the world-readable store.
  withTaostatsKey = keyFile: pkgs.symlinkJoin {
    name = "wallet-watcher-with-taostats-key";
    paths = [ cfg.package ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/wallet-watcher \
        --run ${lib.escapeShellArg "if [ -r ${keyFile} ]; then export TAOSTATS_API_KEY=\"$(cat ${keyFile})\"; fi"}
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
      (if cfg.taostatsApiKeyFile != null then withTaostatsKey cfg.taostatsApiKeyFile else cfg.package)
    ];

    xdg.configFile."wallet-watcher/config.json" =
      if cfg.configFile != null then
        { source = config.lib.file.mkOutOfStoreSymlink cfg.configFile; }
      else
        mkIf (cfg.wallets != { }) { text = cfg.configJson; };
  };
}
