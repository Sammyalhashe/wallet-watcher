(* Prints the dashboard from a fixture snapshot, so layout can be checked
   without a terminal, a network or real balances. *)

open Wallet_watcher
module Nav = Wallet_watcher_tui.Nav
module Screen = Wallet_watcher_tui.Screen

let balance wallet network address result : Snapshot.balance =
  { wallet; network; address; result }

let snapshot : Snapshot.t =
  { balances =
      [ balance "Cold Storage" Config.Bitcoin
          "bc1qgdjqv0av3q56jvd82tkdjpy7gdp9ut8tlqmgrpmv24sq90ecnvqqjwvw97"
          (Ok [ (Config.BTC, 1.245) ]);
        balance "Cold Storage" Config.Ethereum
          "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
          (Ok [ (Config.ETH, 12.5) ]);
        balance "Cold Storage" Config.Base
          "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
          (Ok [ (Config.ETH, 3.25) ]);
        balance "Cold Storage" Config.Avalanche
          "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
          (Ok [ (Config.AVAX, 410.); (Config.ETH, 1.5) ]);
        balance "Satoshi Genesis" Config.Bitcoin "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
          (Ok [ (Config.BTC, 99.673125) ]);
        balance "Hot Wallet" Config.Ethereum
          "0xBE0eB53F46cd790Cd13851d5EFf43D12404d33E8"
          (Ok [ (Config.ETH, 40.2) ]);
        balance "Hot Wallet" Config.Solana "9WzDXwBbmkg8ZTbNMqUxvQRAyrZzDsGYdLVL9zYtAWWM"
          (Ok [ (Config.SOL, 4210.5) ]);
        balance "Hot Wallet" Config.Sui
          "0x02a212de6a9dfa3a69e22387acfbafbb1a9e591bd9d636e7895dcfc8de05f331"
          (Ok [ (Config.SUI, 15000.) ]);
        balance "Hot Wallet" Config.Zcash "t1XPq7rYVWBiyHdHRg1PJZrcCRrixXiLQrd"
          (Ok [ (Config.ZEC, 0.) ]);
        balance "Hot Wallet" Config.Base "0xBE0eB53F46cd790Cd13851d5EFf43D12404d33E8"
          (Error "rpc timed out") ];
    (* SUI is left unpriced on purpose. *)
    prices =
      [ (Config.BTC, 64231.55); (Config.ETH, 3082.19); (Config.AVAX, 31.4);
        (Config.SOL, 148.03); (Config.ZEC, 52.1) ];
    price_error = None }

let width = 100

let () =
  let portfolio = Portfolio.of_snapshot snapshot in
  let show state =
    Notty_unix.output_image ~cap:Notty.Cap.dumb
      (Screen.render state portfolio (width, 40));
    print_newline ()
  in
  List.iter show
    [ { Nav.initial with view = Nav.Table };
      { Nav.initial with view = Nav.Pie };
      { Nav.view = Nav.Pie; focus = Nav.Coin Config.BTC; selected = 0 } ]
