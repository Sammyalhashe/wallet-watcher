# wallet-watcher

An async terminal dashboard for a multi-chain crypto portfolio. Reads a list of
wallets from `config.json`, fetches balances for ETH, SOL, BTC, TAO and SUI
concurrently over JSON-RPC/REST, prices them against CoinGecko, and renders the
result with Notty.

```
  WALLET WATCHER
 NET WORTH  $10,228,905.78
 -------------------------------------------------------------------------------
 ASSET  WALLET           ADDRESS                   AMOUNT      USD VALUE  SHARE %
 BTC    Cold Storage     bc1qgdjqv0..jwvw97      1.245000     $79,968.28    1.23%
 BTC    Satoshi Genesis  1A1zP1eP5Q..DivfNa     99.673125  $6,402,159.31   98.77%
 BTC    TOTAL            @ $64,231.55          100.918125  $6,482,127.59  100.00%
```

Press `c` for the chart view (net-worth allocation, plus how each coin is split
across its wallets), and `q` or `Ctrl-C` to quit.

## Develop

Requires [Nix](https://nixos.org) with [devenv](https://devenv.sh). The shell
pins OCaml 5.5, dune 3, lwt, cohttp, yojson and notty.

```sh
devenv shell          # or `direnv allow` once, via the bundled .envrc
dune build
dune exec bin/main.exe             # uses ./config.json
dune exec bin/main.exe -- other.json
```

`config.json` maps a chain to its wallets:

```json
{
  "ETH": [{ "label": "Vitalik", "address": "0xd8dA6BF2...96045" }],
  "BTC": [{ "label": "Cold Storage", "address": "bc1qgdjqv0..." }]
}
```

Bittensor balances need an API key; without it the TAO rows render as
unavailable rather than as zero:

```sh
export TAOSTATS_API_KEY=...
```

## Test

```sh
dune test                          # 91 assertions, no network required
dune exec test/render_preview.exe  # print both views from fixture data
```

The decoders, share math, bar apportionment and formatting are all tested
against recorded payloads, so the suite runs offline. `render_preview` is the
way to eyeball layout changes without a terminal, a network or real balances.

## Install

```sh
dune build @install
dune install --prefix ~/.local     # installs the `portfolio-tui` binary
```

`portfolio-tui` reads `config.json` from the working directory unless a path is
given as its first argument.

## Notes

- The nixpkgs package is `ocamlPackages.notty-community`; plain `notty` was
  removed upstream. In dune the libraries are `notty-community` and
  `notty-community.lwt`, though the modules are still `Notty` / `Notty_lwt`.
- Balances are parsed as `float`, not `int`: 1 ETH is 10^18 wei and OCaml's
  `max_int` is ~4.6x10^18, so an int would overflow past ~4.6 ETH.
- A failed endpoint degrades one row (`n/a`, with the reason in the footer)
  rather than aborting the refresh.
