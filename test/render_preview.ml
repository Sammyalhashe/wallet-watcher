(* Renders the dashboard from a fixture snapshot and writes it to stdout.

   Covers the cases that are awkward to reach against live endpoints: a failed
   balance, an asset with no price, and a chain gated behind a missing API key. *)

open Portfolio_lib

let wallet chain label address = { Config.chain; label; address }

let snapshot =
  { Chain_api.balances =
      [ (wallet Config.BTC "Cold Storage" "bc1qgdjqv0av3q56jvd82tkdjpy7gdp9ut8tlqmgrpmv24sq90ecnvqqjwvw97", Ok 1.24500000);
        (wallet Config.BTC "Satoshi Genesis" "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa", Ok 99.67312500);
        (wallet Config.ETH "Vitalik" "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045", Ok 1013.402117);
        (wallet Config.ETH "Hot Wallet" "0xBE0eB53F46cd790Cd13851d5EFf43D12404d33E8", Error "timed out");
        (wallet Config.SOL "Staking" "9WzDXwBbmkg8ZTbNMqUxvQRAyrZzDsGYdLVL9zYtAWWM", Ok 4210.5);
        (wallet Config.SUI "Main" "0x02a212de6a9dfa3a69e22387acfbafbb1a9e591bd9d636e7895dcfc8de05f331", Ok 15000.);
        (wallet Config.TAO "Subnet Miner" "5F4tQyWrhfGVcNhoqeiNsR6KjD4wMZ2kfhLj4oHYuyHbZAc3",
         Error "TAO: set TAOSTATS_API_KEY to enable Bittensor balances") ];
    (* SUI deliberately omitted, to show an unpriced asset. *)
    prices =
      [ (Config.BTC, 64231.55); (Config.ETH, 3082.19); (Config.SOL, 148.03) ];
    price_error = None }

let () =
  let portfolio = Portfolio.of_snapshot snapshot in
  let width =
    match Notty_unix.winsize Unix.stdout with Some (w, _) -> w | None -> 100
  in
  List.iter
    (fun view ->
       Notty_unix.output_image ~cap:Notty.Cap.ansi
         (Tui.render view portfolio (width, 40));
       print_newline ())
    [ Tui.Table; Tui.Charts ]
