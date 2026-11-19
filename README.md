# wallet-watcher

A terminal dashboard for a multi-chain crypto portfolio. It reads wallets from
a JSON config, fetches every balance and the USD prices concurrently at
launch, and shows the result as a per-position table and as braille pie charts
you can drill into by coin or by wallet. Read-only: it needs addresses, never
keys.

```text
 NET WORTH  $7,295,353.72 (partial)
 ----------------------------------------------------------------------------------------------------
 NET WORTH > by coin

    ⣠⣴⣶⡇⡏⢸⣿⣶⣦⣄       # BTC   88.85%    100.918125  $6,482,127.59
  ⣠⣾⣌⢻⣿⣿ ⢸⣿⣿⣿⣿⣷⣄     = SOL    8.54%   4210.500000    $623,280.32
 ⢰⣿⣿⣿⣷⣝⢿⡄⢸⣿⣿⣿⣿⣿⣿⡆    + ETH    2.43%     57.450000    $177,071.82
 ⣿⣿⣿⣿⣿⣿⣦⡁⢸⣿⣿⣿⣿⣿⣿⣿    * AVAX   0.18%    410.000000     $12,874.00
 ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿    o SUI        -  15000.000000              -
 ⠸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠇
  ⠙⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋
    ⠙⠻⠿⣿⣿⣿⣿⠿⠟⠋
 ----------------------------------------------------------------------------------------------------
 base Hot Wallet: rpc timed out
 j/k move   Enter drill   Tab coin/wallet   c table   q quit
```

The sample comes from fixture data (`test/render_preview.ml`), not live balances.

Contents: [Networks](#supported-networks) | [Configuration](#configuration) |
[Keys](#keys-and-views) | [Nix](#nix) | [Code map](#code-map) |
[Develop](#develop-and-test)

## Supported networks

| Config key  | Assets shown      | Data source                                   | Notes                                                       |
| ----------- | ----------------- | --------------------------------------------- | ----------------------------------------------------------- |
| `bitcoin`   | BTC               | mempool.space address API                     | Confirmed balance only.                                     |
| `ethereum`  | ETH               | publicnode JSON-RPC (`eth_getBalance`)        |                                                             |
| `base`      | ETH               | publicnode JSON-RPC (`eth_getBalance`)        | Counts as the same asset as Ethereum ETH.                   |
| `avalanche` | AVAX, ETH         | publicnode C-Chain JSON-RPC                   | ETH is bridged WETH.e. If the WETH.e lookup fails, AVAX is still shown and the ETH leg is dropped without a footer error. |
| `solana`    | SOL               | `api.mainnet-beta.solana.com` (`getBalance`)  | Native SOL only.                                            |
| `sui`       | SUI               | `fullnode.mainnet.sui.io` (`suix_getAllBalances`) | Native SUI only; other coin types are ignored.          |
| `zcash`     | ZEC               | Blockchair                                    | Transparent addresses only. Addresses starting `zs`, `zc` or `u1` are refused. |
| `bittensor` | TAO               | Taostats API                                  | Needs `TAOSTATS_API_KEY`; without it the row shows as failed, not zero. |

USD prices come from CoinGecko. An asset CoinGecko does not return is listed
with an unknown price: it takes no pie area and its share is `-`, never 0%.

Monero is not supported: reading a Monero balance requires a view key.

ETH on `ethereum`, `base` and `avalanche` is one asset. It merges in the
by-coin pie and in the ETH drill-down.

Balances are fetched once at launch. There is no refresh key; restart to
refetch. A failing endpoint degrades only its own wallet and network, and the
reason is listed in the footer.

## Configuration

`config.json` holds a list of wallets. Each wallet is a unique `label` plus at
most one address per network. At least one address is required.

```json
{
  "wallets": [
    {
      "label": "Cold Storage",
      "addresses": {
        "bitcoin": "bc1qgdjqv0av3q56jvd82tkdjpy7gdp9ut8tlqmgrpmv24sq90ecnvqqjwvw97",
        "ethereum": "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045",
        "base": "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045",
        "avalanche": "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
      }
    },
    {
      "label": "Hot Wallet",
      "addresses": {
        "solana": "9WzDXwBbmkg8ZTbNMqUxvQRAyrZzDsGYdLVL9zYtAWWM",
        "zcash": "t1XPq7rYVWBiyHdHRg1PJZrcCRrixXiLQrd"
      }
    }
  ]
}
```

Network keys are the values in the first column of the table above
(case-insensitive). An unknown network, a missing `label`, an empty
`addresses` object or a duplicate label is a startup error. The repository's
`config.json` is a fuller example.

### Lookup order

The first match wins:

1. The first command-line argument: `wallet-watcher path/to/config.json`.
2. `./config.json`, if it exists.
3. `$XDG_CONFIG_HOME/wallet-watcher/config.json`, when `XDG_CONFIG_HOME` is set
   to an absolute path.
4. `~/.config/wallet-watcher/config.json`.

### Environment

| Variable           | Effect                                                   |
| ------------------ | -------------------------------------------------------- |
| `TAOSTATS_API_KEY` | Enables Bittensor balances. Sent as the `authorization` header. |

```sh
export TAOSTATS_API_KEY=...
```

## Keys and views

The dashboard opens on the table. Press `c` for the pie view.

| Key                       | Action                                                          |
| ------------------------- | --------------------------------------------------------------- |
| `c`                       | Toggle table and pie view.                                      |
| `Tab`                     | At a pie root, switch between by coin and by wallet.            |
| `j` / `k`, `Down` / `Up`  | Move the selection in the legend.                               |
| `Enter` / `l`             | Drill in. A coin shows its split by wallet; a wallet shows its mix of coins. |
| `Backspace` / `h` / `Esc` | Go up one level (no-op at a root). The parent keeps the selection. |
| `q` / `Ctrl-C`            | Quit.                                                           |

Views:

- **Table**: every position with amount, USD value and share of the asset's
  units, plus a subtotal row per asset.
- **Pie, by coin**: share of net worth per asset. Enter on a coin shows that
  coin's units split across wallets.
- **Pie, by wallet**: share of net worth per wallet. Enter on a wallet shows
  its mix of coins.

Beyond eight slices the tail folds into an `Other (n)` row, which cannot be
drilled into.

The banner reads `NET WORTH $...`. It appends `(partial)` when a fetch failed
or a held asset has no price, so the total is a lower bound. It reads
`n/a (prices unavailable)` when no position could be priced.

## Nix

### Run without installing

```sh
nix run github:Sammyalhashe/wallet-watcher -- config.json
```

The flake outputs are `packages.default` (also `packages.ocamlPackage`),
`apps.default`, `overlays.default` and `homeManagerModules.default`.

### home-manager module

Add the flake as an input, import the module, and enable it. The module
installs the package and writes `~/.config/wallet-watcher/config.json`.

| Option                         | Type                            | Meaning                                                                 |
| ------------------------------ | ------------------------------- | ----------------------------------------------------------------------- |
| `programs.wallet-watcher.enable` | bool                          | Install wallet-watcher.                                                 |
| `package`                      | package                         | Defaults to the flake's `packages.default`.                             |
| `wallets`                      | attrs of label to per-network address | Each network option is a nullable string. Unknown network names fail evaluation, so typos are caught. |
| `configJson`                   | string, read-only               | The `config.json` rendered from `wallets`.                              |
| `configFile`                   | null or path string             | An already-rendered config, linked into place instead of writing `configJson` to the store. |
| `taostatsApiKeyFile`           | null or path string             | File holding the Taostats key. The binary is wrapped to read it at launch. |

With `wallets = { }` and no `configFile`, no config file is written.

#### Plain wallets

```nix
{ inputs, ... }:
{
  imports = [ inputs.wallet-watcher.homeManagerModules.default ];

  programs.wallet-watcher = {
    enable = true;
    wallets = {
      "Cold Storage" = {
        bitcoin = "bc1qgdjqv0av3q56jvd82tkdjpy7gdp9ut8tlqmgrpmv24sq90ecnvqqjwvw97";
        ethereum = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045";
        base = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045";
      };
    };
  };
}
```

> **Warning:** This writes your addresses into the world-readable Nix store,
> and into your repository if the configuration is public. Use the sops-nix
> pattern below to keep them out.

#### Secrets with sops-nix

Build the wallets from sops placeholders, render them with a sops template,
and point `configFile` at the rendered file. Addresses stay out of the store.
The template belongs to the system (NixOS or nix-darwin) configuration and
reads the home-manager user's `configJson`:

```nix
{ config, inputs, ... }:
{
  sops.secrets.cold_storage_btc = { };
  sops.secrets.cold_storage_eth = { };
  sops.secrets.taostats_api_key = { owner = "alice"; };

  sops.templates."wallet-watcher.json" = {
    owner = "alice";
    content = config.home-manager.users.alice.programs.wallet-watcher.configJson;
  };

  home-manager.users.alice = {
    imports = [ inputs.wallet-watcher.homeManagerModules.default ];

    programs.wallet-watcher = {
      enable = true;
      wallets."Cold Storage" = {
        bitcoin = config.sops.placeholder.cold_storage_btc;
        ethereum = config.sops.placeholder.cold_storage_eth;
      };
      configFile = config.sops.templates."wallet-watcher.json".path;
      taostatsApiKeyFile = config.sops.secrets.taostats_api_key.path;
    };
  };
}
```

Replace `alice` with your user. The secret names are examples.

### Use the library from another OCaml flake

Apply `overlays.default`; it adds `ocamlPackages.wallet_watcher`.

```nix
pkgs = import nixpkgs {
  inherit system;
  overlays = [ wallet-watcher.overlays.default ];
};
# then, in your package:
buildInputs = [ pkgs.ocamlPackages.wallet_watcher ];
```

In `dune`:

```lisp
(libraries wallet_watcher)       ; config, fetching, analytics: no terminal dependency
(libraries wallet_watcher.tui)   ; the Notty frontend
```

`wallet_watcher` (`lib/`) does not depend on Notty, so it can back another
front end. `wallet_watcher.tui` is the Notty frontend:
`Wallet_watcher_tui.Tui.run : Portfolio.t -> unit Lwt.t`. A minimal consumer
mirrors `bin/main.ml`: `Config.load`, then `Chain_api.fetch_all`, then
`Portfolio.of_snapshot`.

## Code map

```text
bin/
  main.ml            Entry point: resolve config path, fetch, launch the TUI.
lib/                 Library wallet_watcher (no Notty dependency).
  config.ml          Config types (network, asset), JSON parsing, config path lookup,
                     network keys, CoinGecko ids.
  chain_api.ml       Dispatch network -> fetcher, assets_of_network, fetch_all (everything
                     concurrent, one snapshot).
  snapshot.ml        Result of one fetch round: per-address balances, prices, price error.
  portfolio.ml       Analytics: positions, failures, total_usd (option) and
                     total_is_partial; by_asset, by_wallet, asset_by_wallet,
                     wallet_by_asset; fold (Other (n)); table.
  pie.ml, pie.mli    Braille pie rasteriser, independent of any terminal library.
  chains/
    http.ml          HTTP and JSON-RPC transport, 20 s timeout, errors as results.
    json.ml          Total JSON and hex helpers; malformed input is Error, never raises.
    prices.ml        CoinGecko spot prices.
    bitcoin.ml       BTC via mempool.space.
    evm.ml           ETH, AVAX and WETH.e on Ethereum, Base and Avalanche.
    solana.ml        SOL via JSON-RPC.
    sui.ml           SUI via JSON-RPC.
    zcash.ml         ZEC via Blockchair; refuses shielded addresses.
    bittensor.ml     TAO via Taostats; needs TAOSTATS_API_KEY.
tui/                 Library wallet_watcher.tui (Notty frontend).
  tui.ml             Lwt event loop and terminal setup.
  nav.ml             Pure navigation state, key bindings and transitions.
  screen.ml          Composes title, net-worth banner, body and footer.
  table_view.ml      Position table with per-asset subtotals.
  pie_view.ml        Pie, legend and drill-down breadcrumb.
  palette.ml         Colours and per-slice glyphs.
  format.ml          Number, address and text formatting.
test/
  test_chain_api.ml  Chain response parsing against recorded payloads.
  test_config.ml     Config parsing, path lookup, sample config.json.
  test_pie.ml        Pie geometry.
  test_portfolio.ml  Analytics, navigation and rendering.
  test_util.ml       Shared check helpers.
  render_preview.ml  Prints table and pie screens from fixture data.
nix/
  package.nix        buildDunePackage derivation.
  hm-module.nix      home-manager module.
flake.nix            Packages, app, overlay, home-manager module.
config.json          Sample config.
```

### Add a network

1. Create `lib/chains/<network>.ml` with `fetch : string -> (asset * float) list result Lwt.t`
   (see `solana.ml` for the smallest example).
2. Add a variant to `Config.network`, and to `all_networks` and
   `string_of_network`. Add a `Config.asset` (with `string_of_asset` and
   `coingecko_id`) if it holds a new coin.
3. Dispatch it in `Chain_api.fetch_network`, and list its assets in
   `Chain_api.assets_of_network`.
4. Add the key to `networks` in `nix/hm-module.nix`.

## Develop and test

Requires [Nix](https://nixos.org) with [devenv](https://devenv.sh).

```sh
devenv shell
dune build
dune exec bin/main.exe                  # config lookup as above
dune exec bin/main.exe -- other.json
dune test                               # offline
dune exec test/render_preview.exe       # table, pie and a drilled pie from fixture data
```

`dune test` runs four suites: `test_chain_api`, `test_config`, `test_pie` and
`test_portfolio`. They run offline against recorded payloads. `render_preview`
shows layout changes without a terminal, a network or real balances.

To install the binary without Nix:

```sh
dune build @install
dune install --prefix ~/.local     # installs `wallet-watcher` to ~/.local/bin
```

### Known limitation

The HTTP client (cohttp) does not use `HTTPS_PROXY` or `https_proxy`. Behind a
proxy that blocks direct connections, every live fetch fails and the footer
lists the errors. The offline tests are unaffected.
