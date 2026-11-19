# wallet-watcher

An async terminal dashboard for a multi-chain crypto portfolio. Reads a list of
wallets from a JSON config, fetches balances on each wallet's networks
concurrently, prices them against CoinGecko, and renders the result with Notty:
a per-position table, and braille pie charts you can drill into.

## Keys

| Key                     | Action                                                  |
| ----------------------- | ------------------------------------------------------- |
| `c`                     | toggle table / pie view                                 |
| `Tab`                   | at the pie root, switch between by coin and by wallet   |
| `j` `k` / `Down` `Up`   | move the selection in the legend                        |
| `Enter` / `l`           | drill in: a coin shows its split by wallet, a wallet shows its mix of coins |
| `Backspace` / `h` / `Esc` | go up one level (no-op at the root)                   |
| `q` / `Ctrl-C`          | quit                                                    |

Beyond eight slices the tail folds into an `Other (n)` row, which cannot be
drilled into. A coin with no price is listed but takes no area: its share is
unknown, never 0%.

## Config

```json
{
  "wallets": [
    {
      "label": "Cold Storage",
      "addresses": {
        "bitcoin": "bc1qgdjqv0...",
        "ethereum": "0xd8dA6BF2...",
        "base": "0xd8dA6BF2...",
        "avalanche": "0xd8dA6BF2..."
      }
    }
  ]
}
```

A wallet is a label plus one address per network. Labels must be unique. See
`config.json` in this repo for a full example.

The config is looked up in this order:

1. the first command-line argument
2. `./config.json`
3. `$XDG_CONFIG_HOME/wallet-watcher/config.json` (`~/.config` if unset)

## Networks

| Network     | Assets reported                                  |
| ----------- | ------------------------------------------------ |
| `bitcoin`   | BTC                                              |
| `ethereum`  | ETH                                              |
| `base`      | ETH                                              |
| `avalanche` | AVAX, and ETH held as WETH.e                     |
| `solana`    | SOL                                              |
| `sui`       | SUI                                              |
| `zcash`     | ZEC, transparent (`t1...`) addresses only        |
| `bittensor` | TAO                                              |

ETH on ethereum, base and avalanche is one asset: it merges in the by-coin pie
and in the ETH drill-down. Bittensor balances need an API key; without it the
TAO rows show as failed rather than as zero:

```sh
export TAOSTATS_API_KEY=...
```

A failing endpoint degrades only its own wallet and network; the reason is
listed in the footer.

## Develop

Requires [Nix](https://nixos.org) with [devenv](https://devenv.sh).

```sh
devenv shell
dune build
dune exec bin/main.exe             # config lookup as above
dune exec bin/main.exe -- other.json
dune test                          # offline, no network required
dune exec test/render_preview.exe  # table, pie and a drilled pie from fixture data
```

`render_preview` is the way to eyeball layout changes without a terminal, a
network or real balances.

## Install

```sh
dune build @install
dune install --prefix ~/.local     # installs the `wallet-watcher` binary
```

## Library

The analytics live in `wallet_watcher`, which has no terminal dependency, so it
can back another front end:

- `Wallet_watcher.Config`, `Chain_api`, `Snapshot` load the config and fetch.
- `Wallet_watcher.Portfolio.of_snapshot` flattens a snapshot into positions;
  `by_asset`, `by_wallet`, `asset_by_wallet` and `wallet_by_asset` return
  ranked slices (`name`, `amount`, `usd`, `pct`), and `table` returns the
  per-asset rows with subtotals.

The Notty UI is the separate `wallet_watcher.tui` library
(`Wallet_watcher_tui.Tui.run : Portfolio.t -> unit Lwt.t`).

## Notes

- The nixpkgs package is `ocamlPackages.notty-community`; the modules are still
  `Notty` / `Notty_lwt`.
- Balances are `float`, not `int`: 1 ETH is 10^18 wei and OCaml's `max_int` is
  ~4.6x10^18.
