open Wallet_watcher
open Test_util

let check_ok_string name ~expect = function
  | Ok v -> check_string name ~expect v
  | Error e -> report name false ("unexpected Error: " ^ e)

let raises f =
  match f () with
  | _ -> false
  | exception Config.Parse_error _ -> true

let () =
  print_endline "config";
  let cfg =
    Config.of_json
      (json
         {|{"wallets":[
             {"label":"Ledger","addresses":{"bitcoin":"bc1","Base":"0xa","avalanche":"0xa"}},
             {"label":"Phantom","addresses":{"solana":"so1"}}]}|})
  in
  check "two wallets" (List.length cfg = 2);
  let ledger = List.hd cfg in
  check_string "label" ~expect:"Ledger" ledger.label;
  check "networks parsed case-insensitively"
    (List.map fst ledger.addresses = [ Config.Bitcoin; Config.Base; Config.Avalanche ]);
  check_string "coingecko AVAX" ~expect:"avalanche-2" (Config.coingecko_id Config.AVAX);
  check_string "coingecko ZEC" ~expect:"zcash" (Config.coingecko_id Config.ZEC);
  check "network round-trips"
    (List.for_all
       (fun n -> Config.network_of_string (Config.string_of_network n) = n)
       Config.all_networks);
  check "rejects unknown network"
    (raises (fun () ->
       Config.of_json (json {|{"wallets":[{"label":"x","addresses":{"monero":"4a"}}]}|})));
  check "rejects missing label"
    (raises (fun () -> Config.of_json (json {|{"wallets":[{"addresses":{"bitcoin":"b"}}]}|})));
  check "rejects empty addresses"
    (raises (fun () -> Config.of_json (json {|{"wallets":[{"label":"x","addresses":{}}]}|})));
  check "rejects duplicate labels"
    (raises (fun () ->
       Config.of_json
         (json
            {|{"wallets":[{"label":"x","addresses":{"bitcoin":"a"}},
                          {"label":"x","addresses":{"bitcoin":"b"}}]}|})));
  check "rejects old chain-keyed format"
    (raises (fun () -> Config.of_json (json {|{"BTC":[{"label":"a","address":"b"}]}|})));
  let path = Config.config_path_of in
  check_ok_string "local file wins"
    ~expect:"config.json" (path ~home:(Some "/h") ~xdg:(Some "/x") ~local_exists:true);
  check_ok_string "absolute XDG used" ~expect:"/x/wallet-watcher/config.json"
    (path ~home:(Some "/h") ~xdg:(Some "/x") ~local_exists:false);
  check_ok_string "relative XDG ignored" ~expect:"/h/.config/wallet-watcher/config.json"
    (path ~home:(Some "/h") ~xdg:(Some "rel/x") ~local_exists:false);
  check_ok_string "empty XDG ignored" ~expect:"/h/.config/wallet-watcher/config.json"
    (path ~home:(Some "/h") ~xdg:(Some "") ~local_exists:false);
  check_error "no HOME and no XDG is an error"
    (path ~home:None ~xdg:None ~local_exists:false);
  check_ok_string "XDG alone suffices" ~expect:"/x/wallet-watcher/config.json"
    (path ~home:None ~xdg:(Some "/x") ~local_exists:false);
  check_ok_string "local file needs no env" ~expect:"config.json"
    (path ~home:None ~xdg:None ~local_exists:true);
  check "sample config.json loads" (List.length (Config.load "../config.json") = 3);
  summary ()
