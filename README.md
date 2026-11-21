# wallet-watcher

A terminal dashboard for a multi-chain crypto portfolio. It reads wallets from
a JSON config, fetches every balance and the USD prices concurrently at
launch, and shows the result as a per-position table and as braille pie charts
you can drill into by coin or by wallet. Read-only: it needs addresses, never
keys.

```text
 NET WORTH  $7,312,353.72 (partial)
 ----------------------------------------------------------------------------------------------------
 NET WORTH > by coin

    ⣠⣴⣶⡇⡟⢸⣿⣶⣦⣄       # BTC   88.55%    100.918125  $6,482,127.59
  ⣠⣶⣜⢿⣿⣷⠁⢸⣿⣿⣿⣿⣷⣄     = SOL    8.51%   4210.500000    $623,280.32
 ⢰⣿⣿⣿⣷⡝⢿⡀⢸⣿⣿⣿⣿⣿⣿⡆    + ETH    2.42%     57.450000    $177,071.82
 ⣿⣿⣿⣿⣿⣿⣦⡃⢸⣿⣿⣿⣿⣿⣿⣿    * USDC   0.34%  25000.000000     $25,000.00
 ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿    o AVAX   0.18%    410.000000     $12,874.00
 ⠸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠇    x SUI        -  15000.000000              -
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
| `bitcoin`   | BTC               | An address: mempool.space address API. A descriptor: your bitcoind (`scantxoutset`) | Confirmed balance only. A value containing `(` is an output descriptor, which counts every address of an HD or multisig wallet; it needs the `BITCOIND_RPC_*` variables. |
| `ethereum`  | ETH, or every token and DeFi position | publicnode JSON-RPC (`eth_getBalance`), or Zerion when `ZERION_API_KEY` is set | See [EVM with Zerion](#evm-with-zerion).            |
| `base`      | ETH, or every token and DeFi position | publicnode JSON-RPC (`eth_getBalance`), or Zerion | Counts as the same asset as Ethereum ETH.         |
| `avalanche` | AVAX, ETH, or every token and DeFi position | publicnode C-Chain JSON-RPC, or Zerion  | Without Zerion, ETH is bridged WETH.e. If the WETH.e lookup fails, AVAX is still shown and the ETH leg is dropped without a footer error. |
| `solana`    | SOL               | `api.mainnet-beta.solana.com` (`getBalance`)  | Native SOL only.                                            |
| `sui`       | SUI               | `fullnode.mainnet.sui.io` (`suix_getAllBalances`) | Native SUI only; other coin types are ignored.          |
| `zcash`     | ZEC               | Blockchair                                    | Transparent addresses only. Addresses starting `zs`, `zc` or `u1` are refused. |
| `bittensor` | TAO               | Taostats API                                  | Needs `TAOSTATS_API_KEY`; without it the row shows as failed, not zero. |

USD prices for the native coins come from CoinGecko. An asset CoinGecko does
not return is listed with an unknown price: it takes no pie area and its share
is `-`, never 0%. Tokens from Zerion carry their own USD value.

Monero is not supported: reading a Monero balance requires a view key.

An asset is identified by its upper-case symbol, so ETH on `ethereum`, `base`
and `avalanche` is one asset, as is USDC on every chain. It merges in the
by-coin pie and in the drill-down.

### Bitcoin descriptors

A `bitcoin` value such as `wsh(sortedmulti(2,[fp/48h/0h/0h/2h]xpub.../<0;1>/*,...))`
is balanced by your own node with `scantxoutset`, so no address leaves your
network. The checksum is optional. `<0;1>` is expanded into the receive and
change descriptors, so Bitcoin Core older than 29 works too; wildcard
descriptors are scanned to index 1000. Scans run one at a time and may take
minutes each, with a 10-minute timeout. The RPC user needs only
`scantxoutset` in its whitelist.

### EVM with Zerion

With `ZERION_API_KEY` set, each `ethereum`, `base` and `avalanche` entry makes
one Zerion positions request for that chain, and shows ERC-20 tokens and DeFi
positions (deposits, loans, staking, LP and vault shares) with Zerion's USD
values. The table's NETWORK column adds the protocol and position type, e.g.
`base - Aave V3 deposit`.

- A loan is shown with a negative amount and value. The net worth subtracts
  it; pie slices show only what is held.
- Positions Zerion marks as not displayable, and positions worth under $1,
  are dropped.
- Every remaining position is counted as Zerion reports it. If a protocol's
  receipt token also shows up as a wallet balance, it is counted twice.

Without the key, these networks fall back to native balances as above.

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
    },
    {
      "label": "Multisig",
      "addresses": {
        "bitcoin": "wsh(sortedmulti(2,[aaaaaaaa/48h/0h/0h/2h]xpub6E.../<0;1>/*,[bbbbbbbb/48h/0h/0h/2h]xpub6F.../<0;1>/*))"
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

| Variable                | Effect                                                   |
| ----------------------- | -------------------------------------------------------- |
| `TAOSTATS_API_KEY`      | Enables Bittensor balances. Sent as the `authorization` header. |
| `ZERION_API_KEY`        | Reads EVM tokens and DeFi positions from Zerion. Sent as the Basic auth user. |
| `BITCOIND_RPC_URL`      | bitcoind RPC endpoint for bitcoin descriptors, e.g. `http://127.0.0.1:8332`. |
| `BITCOIND_RPC_USER`     | bitcoind RPC user.                                       |
| `BITCOIND_RPC_PASSWORD` | bitcoind RPC password.                                   |

A descriptor without all three `BITCOIND_RPC_*` variables shows as failed, not
zero.

```sh
export TAOSTATS_API_KEY=...
export ZERION_API_KEY=...
export BITCOIND_RPC_URL=http://127.0.0.1:8332 BITCOIND_RPC_USER=walletwatcher BITCOIND_RPC_PASSWORD=...
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
  held units, plus a subtotal row per asset. Loans have no share.
- **Pie, by coin**: share of net worth per asset. Enter on a coin shows that
  coin's units split across wallets.
- **Pie, by wallet**: share of net worth per wallet. Enter on a wallet shows
  its mix of coins.

Beyond eight slices the tail folds into an `Other (n)` row, which cannot be
drilled into.

The banner reads `NET WORTH $...`, net of loans. It appends `(partial)` when a
fetch failed or a held asset has no price, so the total is a lower bound. It
reads `n/a (prices unavailable)` when no position could be priced.

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
| `zerionApiKeyFile`             | null or path string             | File holding the Zerion key, exported as `ZERION_API_KEY` at launch. |
| `bitcoind.url`                 | null or string                  | Exported as `BITCOIND_RPC_URL`.                                         |
| `bitcoind.user`                | null or string                  | Exported as `BITCOIND_RPC_USER`.                                        |
| `bitcoind.passwordFile`        | null or path string             | File holding the RPC password, exported as `BITCOIND_RPC_PASSWORD` at launch. |

Key files are read only if readable when the program starts, so they never
enter the Nix store. Without any of these options the package is installed
unwrapped.

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
  sops.secrets.zerion_api_key = { owner = "alice"; };
  sops.secrets.bitcoind_rpc_password = { owner = "alice"; };

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
      zerionApiKeyFile = config.sops.secrets.zerion_api_key.path;
      bitcoind = {
        url = "http://127.0.0.1:8332";
        user = "walletwatcher";
        passwordFile = config.sops.secrets.bitcoind_rpc_password.path;
      };
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
  config.ml          Config types (network; asset = symbol), JSON parsing, config path
                     lookup, network keys, native-coin CoinGecko ids.
  chain_api.ml       Dispatch network -> fetcher (Zerion for EVM when keyed),
                     assets_of_network, fetch_all (everything concurrent, one snapshot).
  snapshot.ml        Holdings and the result of one fetch round: per-address
                     balances, prices, price error.
  portfolio.ml       Analytics: positions, failures, total_usd (option) and
                     total_is_partial; by_asset, by_wallet, asset_by_wallet,
                     wallet_by_asset; fold (Other (n)); table.
  pie.ml, pie.mli    Braille pie rasteriser, independent of any terminal library.
  chains/
    http.ml          HTTP and JSON-RPC transport, per-call headers and timeout
                     (20 s default), Basic auth, errors as results.
    json.ml          Total JSON and hex helpers; malformed input is Error, never raises.
    prices.ml        CoinGecko spot prices.
    bitcoin.ml       BTC via mempool.space, or a descriptor via bitcoind scantxoutset.
    evm.ml           ETH, AVAX and WETH.e on Ethereum, Base and Avalanche.
    zerion.ml        EVM tokens and DeFi positions via Zerion; needs ZERION_API_KEY.
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

1. Create `lib/chains/<network>.ml` with `fetch : string -> Snapshot.fetch_result Lwt.t`,
   returning `Snapshot.holding` values (see `solana.ml` for the smallest
   example). An asset is just its upper-case symbol; set `usd` on a holding
   when the source values it.
2. Add a variant to `Config.network`, and to `all_networks` and
   `string_of_network`. For a new native coin, add a constant next to
   `Config.btc` and its entry in `Config.coingecko_ids`, so it gets a price.
3. Dispatch it in `Chain_api.fetch_network`, and list its priced assets in
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
