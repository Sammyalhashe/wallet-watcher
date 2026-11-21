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
          (Ok [ (Snapshot.holding Config.btc 1.245) ]);
        balance "Cold Storage" Config.Ethereum
          "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
          (Ok [ (Snapshot.holding Config.eth 12.5) ]);
        (* Shaped like a Zerion result: self-valued, with a deposit and a loan. *)
        balance "Cold Storage" Config.Base
          "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
          (Ok [ { asset = Config.eth; amount = 3.25; usd = Some 10017.12; detail = None };
                { asset = "USDC"; amount = 25000.; usd = Some 25000.;
                  detail = Some "Aave V3 deposit" };
                { asset = "USDC"; amount = -8000.; usd = Some (-8000.);
                  detail = Some "Aave V3 loan" } ]);
        balance "Cold Storage" Config.Avalanche
          "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"
          (Ok [ (Snapshot.holding Config.avax 410.); (Snapshot.holding Config.eth 1.5) ]);
        balance "Satoshi Genesis" Config.Bitcoin "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
          (Ok [ (Snapshot.holding Config.btc 99.673125) ]);
        balance "Hot Wallet" Config.Ethereum
          "0xBE0eB53F46cd790Cd13851d5EFf43D12404d33E8"
          (Ok [ (Snapshot.holding Config.eth 40.2) ]);
        balance "Hot Wallet" Config.Solana "9WzDXwBbmkg8ZTbNMqUxvQRAyrZzDsGYdLVL9zYtAWWM"
          (Ok [ (Snapshot.holding Config.sol 4210.5) ]);
        balance "Hot Wallet" Config.Sui
          "0x02a212de6a9dfa3a69e22387acfbafbb1a9e591bd9d636e7895dcfc8de05f331"
          (Ok [ (Snapshot.holding Config.sui 15000.) ]);
        balance "Hot Wallet" Config.Zcash "t1XPq7rYVWBiyHdHRg1PJZrcCRrixXiLQrd"
          (Ok [ (Snapshot.holding Config.zec 0.) ]);
        balance "Hot Wallet" Config.Base "0xBE0eB53F46cd790Cd13851d5EFf43D12404d33E8"
          (Error "rpc timed out") ];
    (* SUI is left unpriced on purpose. *)
    prices =
      [ (Config.btc, 64231.55); (Config.eth, 3082.19); (Config.avax, 31.4);
        (Config.sol, 148.03); (Config.zec, 52.1) ];
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
      { Nav.view = Nav.Pie; focus = Nav.Coin Config.btc; selected = 0 } ]
