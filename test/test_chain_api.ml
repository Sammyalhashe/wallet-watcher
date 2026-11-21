open Wallet_watcher
open Test_util

let test_hex () =
  print_endline "hex -> float";
  check_ok_float "zero" ~expect:0. (Json.float_of_hex "0x0");
  check_ok_float "one wei" ~expect:1. (Json.float_of_hex "0x1");
  check_ok_float "0xff" ~expect:255. (Json.float_of_hex "0xFF");
  check_ok_float "no 0x prefix" ~expect:255. (Json.float_of_hex "ff");
  check_ok_float "1e18 wei" ~expect:1e18 (Json.float_of_hex "0xde0b6b3a7640000");
  (* 10^21 wei exceeds max_int and would wrap if parsed as int. *)
  check_ok_float "1e21 wei exceeds max_int" ~expect:1e21
    (Json.float_of_hex "0x3635c9adc5dea00000");
  check_error "rejects garbage" (Json.float_of_hex "0xzz");
  check_error "rejects empty" (Json.float_of_hex "0x")

let test_eth () =
  print_endline "ETH decoder";
  check_ok_float "1 ETH" ~expect:1.
    (Evm.parse_native_balance
       (json {|{"jsonrpc":"2.0","id":1,"result":"0xde0b6b3a7640000"}|}));
  check_ok_float "zero balance" ~expect:0.
    (Evm.parse_native_balance (json {|{"jsonrpc":"2.0","id":1,"result":"0x0"}|}));
  check_ok_float "1000 ETH" ~expect:1000.
    (Evm.parse_native_balance
       (json {|{"jsonrpc":"2.0","id":1,"result":"0x3635c9adc5dea00000"}|}));
  check_error "surfaces rpc error"
    (Evm.parse_native_balance
       (json {|{"jsonrpc":"2.0","id":1,"error":{"code":-32602,"message":"bad address"}}|}));
  check_error "missing result" (Evm.parse_native_balance (json {|{"jsonrpc":"2.0"}|}))

let test_rpc_result () =
  print_endline "rpc_result";
  check "bitcoind's null error is success"
    (Http.rpc_result (json {|{"result":{"height":1},"error":null,"id":1}|})
     = Ok (json {|{"height":1}|}));
  check_error "non-null error fails"
    (Http.rpc_result (json {|{"result":null,"error":{"code":-8,"message":"Scan already in progress"},"id":1}|}));
  check_error "null error without result fails" (Http.rpc_result (json {|{"error":null,"id":1}|}))

let test_sol () =
  print_endline "SOL decoder";
  check_ok_float "lamports -> SOL" ~expect:1.23456789
    (Solana.parse_balance
       (json
          {|{"jsonrpc":"2.0","result":{"context":{"slot":1},"value":1234567890},"id":1}|}));
  check_ok_float "zero" ~expect:0.
    (Solana.parse_balance
       (json {|{"jsonrpc":"2.0","result":{"context":{"slot":1},"value":0},"id":1}|}));
  check_error "surfaces rpc error"
    (Solana.parse_balance
       (json {|{"jsonrpc":"2.0","error":{"message":"Invalid param"},"id":1}|}))

let test_btc () =
  print_endline "BTC decoder";
  (* funded 1.0 BTC, spent 0.4 BTC -> 0.6 BTC *)
  check_ok_float "funded - spent" ~expect:0.6
    (Bitcoin.parse_balance
       (json
          {|{"address":"bc1q","chain_stats":{"funded_txo_sum":100000000,
             "spent_txo_sum":40000000,"tx_count":3},
             "mempool_stats":{"funded_txo_sum":0,"spent_txo_sum":0}}|}));
  check_ok_float "fully spent" ~expect:0.
    (Bitcoin.parse_balance
       (json
          {|{"chain_stats":{"funded_txo_sum":50000,"spent_txo_sum":50000}}|}));
  check_error "missing chain_stats"
    (Bitcoin.parse_balance (json {|{"address":"bc1q"}|}))

let test_btc_descriptor () =
  print_endline "BTC descriptors";
  let multisig = "wsh(sortedmulti(2,[aa/48h/0h/0h/2h]xpubA/<0;1>/*,[bb/48h/0h/0h/2h]xpubB/<0;1>/*))" in
  check "parenthesised value is a descriptor" (Bitcoin.is_descriptor multisig);
  check "address is not a descriptor"
    (not (Bitcoin.is_descriptor "bc1qgdjqv0av3q56jvd82tkdjpy7gdp9ut8tlqmgrpmv24sq90ecnvqqjwvw97"));
  check "multipath expands into receive and change, every key"
    (Bitcoin.expand_multipath (multisig ^ "#abcd1234")
     = [ "wsh(sortedmulti(2,[aa/48h/0h/0h/2h]xpubA/0/*,[bb/48h/0h/0h/2h]xpubB/0/*))";
         "wsh(sortedmulti(2,[aa/48h/0h/0h/2h]xpubA/1/*,[bb/48h/0h/0h/2h]xpubB/1/*))" ]);
  check "single-path descriptor keeps its checksum"
    (Bitcoin.expand_multipath "wpkh(xpubA/0/*)#abcd1234" = [ "wpkh(xpubA/0/*)#abcd1234" ]);
  check "range only for wildcard descriptors"
    (Bitcoin.scan_params [ "wpkh(xpubA/0/*)"; "addr(bc1q)" ]
     = [ `String "start";
         `List [ `Assoc [ ("desc", `String "wpkh(xpubA/0/*)"); ("range", `Int 1000) ];
                 `Assoc [ ("desc", `String "addr(bc1q)") ] ] ]);
  check_ok_float "scan total_amount" ~expect:0.5123
    (Bitcoin.parse_scan
       (json
          {|{"result":{"success":true,"txouts":9000000,"height":870000,"bestblock":"00ab",
             "unspents":[{"txid":"aa","vout":0,"amount":0.5,"height":1},
                         {"txid":"bb","vout":1,"amount":0.0123,"height":2}],
             "total_amount":0.51230000},"error":null,"id":1}|}));
  check_error "scan in progress is an error"
    (Bitcoin.parse_scan
       (json
          {|{"result":null,"error":{"code":-8,"message":"Scan already in progress, use action \"abort\" or \"status\""},"id":1}|}));
  check_error "aborted scan is an error"
    (Bitcoin.parse_scan (json {|{"result":{"success":false},"error":null,"id":1}|}));
  Unix.putenv "BITCOIND_RPC_URL" "";
  check "missing credentials fail without a request"
    (match Lwt_main.run (Bitcoin.fetch_descriptor multisig) with
     | Error e -> String.starts_with ~prefix:"BTC: set BITCOIND_RPC_URL" e
     | Ok _ -> false)

let zerion_item ~id ~kind ?protocol ?(displayable = true) ~symbol ~amount ~value () =
  Printf.sprintf
    {|{"type":"positions","id":"%s-%s","attributes":{"parent":null,"protocol":%s,
       "name":"%s","position_type":"%s",
       "quantity":{"int":"0","decimals":18,"float":%s,"numeric":"%s"},
       "value":%s,"price":1.0,"changes":null,
       "fungible_info":{"name":"%s","symbol":"%s","icon":null,"flags":{"verified":true}},
       "flags":{"displayable":%b,"is_trash":false}},
       "relationships":{"chain":{"data":{"type":"chains","id":"base"}},
                        "fungible":{"data":{"type":"fungibles","id":"%s"}}}}|}
    id kind
    (match protocol with Some p -> Printf.sprintf "%S" p | None -> "null")
    symbol kind amount amount value symbol symbol displayable id

let test_zerion () =
  print_endline "Zerion positions";
  let payload =
    json
      ({|{"links":{"self":"x"},"data":[|}
       ^ String.concat ","
           [ zerion_item ~id:"eth" ~kind:"wallet" ~symbol:"ETH" ~amount:"1.5" ~value:"4500.0" ();
             zerion_item ~id:"usdc" ~kind:"wallet" ~symbol:"USDC" ~amount:"1000" ~value:"1000" ();
             zerion_item ~id:"usdc" ~kind:"deposit" ~protocol:"Aave V3" ~symbol:"USDC"
               ~amount:"1000" ~value:"1000" ();
             zerion_item ~id:"usdt" ~kind:"loan" ~protocol:"Aave V3" ~symbol:"USDT"
               ~amount:"200" ~value:"200.5" ();
             zerion_item ~id:"spam" ~kind:"wallet" ~displayable:false ~symbol:"SCAM"
               ~amount:"1" ~value:"99999" ();
             zerion_item ~id:"shib" ~kind:"wallet" ~symbol:"shib" ~amount:"1000" ~value:"0.3" ();
             zerion_item ~id:"steth" ~kind:"staked" ~symbol:"stETH" ~amount:"2" ~value:"null" () ]
       ^ "]}")
  in
  match Zerion.parse_positions payload with
  | Error e -> report "parse_positions" false e
  | Ok holdings ->
    let find_detail asset detail =
      List.find_opt
        (fun (h : Snapshot.holding) -> h.asset = asset && h.detail = detail)
        holdings
    in
    check "dust and spam are gone" (List.length holdings = 5);
    (match find_detail "ETH" None with
     | Some h -> check "wallet ETH is self-valued" (h.amount = 1.5 && h.usd = Some 4500.)
     | None -> report "wallet ETH kept" false "missing");
    check "wallet USDC kept beside an Aave USDC deposit" (find_detail "USDC" None <> None);
    check "deposit keeps protocol detail" (find_detail "USDC" (Some "Aave V3 deposit") <> None);
    (match find_detail "USDT" (Some "Aave V3 loan") with
     | Some h -> check "loan negated" (h.amount = -200. && h.usd = Some (-200.5))
     | None -> report "loan kept" false "missing");
    check "non-displayable skipped"
      (not (List.exists (fun (h : Snapshot.holding) -> h.asset = "SCAM") holdings));
    check "dust dropped"
      (not (List.exists (fun (h : Snapshot.holding) -> h.asset = "SHIB") holdings));
    (match find_detail "STETH" (Some "staked") with
     | Some h -> check "unvalued position kept unpriced, symbol upper-cased" (h.usd = None)
     | None -> report "unvalued position kept" false "missing");
    check_error "missing data is an error" (Zerion.parse_positions (json {|{"errors":[]}|}));
    check_error "malformed position is an error"
      (Zerion.parse_positions (json {|{"data":[{"attributes":{"position_type":"wallet"}}]}|}));
    check_string "positions url"
      ~expect:"https://api.zerion.io/v1/wallets/0xabc/positions/?filter%5Bpositions%5D=no_filter&filter%5Bchain_ids%5D=base&filter%5Btrash%5D=only_non_trash&currency=usd"
      (Zerion.positions_url ~chain:"base" "0xabc")

let test_sui () =
  print_endline "SUI decoder";
  (* Only the native coin type counts; the USDC row must be ignored. *)
  check_ok_float "native SUI only" ~expect:2.5
    (Sui.parse_balance
       (json
          {|{"jsonrpc":"2.0","result":[
             {"coinType":"0x2::sui::SUI","coinObjectCount":2,"totalBalance":"2500000000"},
             {"coinType":"0xabc::usdc::USDC","coinObjectCount":1,"totalBalance":"9999000000"}
            ],"id":1}|}));
  check_ok_float "no SUI holdings" ~expect:0.
    (Sui.parse_balance (json {|{"jsonrpc":"2.0","result":[],"id":1}|}));
  check_error "surfaces rpc error"
    (Sui.parse_balance
       (json {|{"jsonrpc":"2.0","error":{"message":"bad address"},"id":1}|}))

let test_tao () =
  print_endline "TAO decoder";
  check_ok_float "data[0].balance_total" ~expect:12.5
    (Bittensor.parse_balance
       (json {|{"data":[{"balance_total":"12500000000","balance_free":"1"}]}|}));
  check_ok_float "flat balance_free" ~expect:2.
    (Bittensor.parse_balance (json {|{"balance_free":2000000000}|}));
  check_error "unrecognised shape"
    (Bittensor.parse_balance (json {|{"unexpected":true}|}))

let test_prices () =
  print_endline "price decoder";
  let payload =
    json
      {|{"ethereum":{"usd":3000.5},"bitcoin":{"usd":65000},"solana":{"usd":150.25},
         "avalanche-2":{"usd":30.5},"zcash":{"usd":45}}|}
  in
  match
    Prices.parse Config.[ eth; btc; sui; avax; zec ] payload
  with
  | Error e -> report "parse_prices" false e
  | Ok prices ->
    check "prices: SUI absent, four found" (List.length prices = 4);
    check_float "ETH price" ~expect:3000.5 (List.assoc Config.eth prices);
    check_float "BTC price" ~expect:65000. (List.assoc Config.btc prices);
    check_float "AVAX price" ~expect:30.5 (List.assoc Config.avax prices);
    check_float "ZEC price" ~expect:45. (List.assoc Config.zec prices);
    check "SUI omitted" (List.assoc_opt Config.sui prices = None)

let vitalik = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"

let test_calldata () =
  print_endline "balanceOf calldata";
  (match Evm.balance_of_calldata vitalik with
   | Ok data ->
     check_string "known vector"
       ~expect:"0x70a08231000000000000000000000000d8da6bf26964af9d7eed9e03e53415d37aa96045"
       data
   | Error e -> report "known vector" false e);
  check_error "rejects short address" (Evm.balance_of_calldata "0xabc");
  check_error "rejects missing 0x"
    (Evm.balance_of_calldata "d8dA6BF26964aF9D7eEd9e03E53415D37aA96045ab");
  check_error "rejects non-hex"
    (Evm.balance_of_calldata "0xZZdA6BF26964aF9D7eEd9e03E53415D37aA96045")

let test_erc20 () =
  print_endline "ERC-20 decoder";
  check_ok_float "1.5 tokens at 18 decimals" ~expect:1.5
    (Evm.parse_erc20_balance ~decimals:18
       (json
          {|{"jsonrpc":"2.0","id":1,"result":"0x00000000000000000000000000000000000000000000000014d1120d7b160000"}|}));
  check_ok_float "6 decimals" ~expect:2.
    (Evm.parse_erc20_balance ~decimals:6
       (json {|{"jsonrpc":"2.0","id":1,"result":"0x1e8480"}|}));
  check_error "surfaces rpc error"
    (Evm.parse_erc20_balance ~decimals:18
       (json {|{"jsonrpc":"2.0","id":1,"error":{"code":-32000,"message":"execution reverted"}}|}))

let test_zec () =
  print_endline "ZEC decoder";
  let address = "t1abc" in
  let payload =
    json
      {|{"data":{"t1abc":{"address":{"type":"pubkeyhash","balance":123456789,
         "received":0,"spent":0},"transactions":[]}},"context":{"code":200}}|}
  in
  check_ok_float "zatoshi -> ZEC" ~expect:1.23456789
    (Zcash.parse_balance address payload);
  check_error "missing address key" (Zcash.parse_balance "t1other" payload);
  check "shielded address rejected without a request"
    (match Lwt_main.run (Zcash.fetch "zs1xyz") with
     | Error "ZEC: shielded addresses are not supported" -> true
     | _ -> false)

let test_avalanche_combine () =
  let both = Evm.combine_avalanche (Ok 2.) (Ok 3.) in
  check "both legs give AVAX and ETH"
    (both = Ok [ (Snapshot.holding Config.avax 2.); (Snapshot.holding Config.eth 3.) ]);
  check "failed WETH leg keeps AVAX"
    (Evm.combine_avalanche (Ok 2.) (Error "boom") = Ok [ (Snapshot.holding Config.avax 2.) ]);
  check_error "failed AVAX leg fails the result"
    (Evm.combine_avalanche (Error "boom") (Ok 3.));
  check_error "both legs failing is an error"
    (Evm.combine_avalanche (Error "a") (Error "b"))

let test_single_line_errors () =
  let single_line s = not (String.exists (fun c -> Char.code c < 0x20) s) in
  (match Json.parse_json "{\n  \"a\": ,\n}" with
   | Error e -> check "parse_json error is one line" (single_line e)
   | Ok _ -> check "parse_json rejects bad input" false);
  check "http error message is one line"
    (single_line (Http.error_message "GET" 502 "<html>\r\n<body>\tBad\n"));
  check_string "http error message keeps text" ~expect:"GET: HTTP 502 <html>  <body> Bad "
    (Http.error_message "GET" 502 "<html>\r\n<body>\tBad\n");
  let hit = ref false in
  let result =
    Lwt_main.run
      (Http.guard "x" (fun () -> hit := true; failwith "sync"))
  in
  check "guard catches synchronous raise" (!hit && Result.is_error result)

let () =
  test_avalanche_combine ();
  test_single_line_errors ();
  test_hex ();
  test_eth ();
  test_rpc_result ();
  test_sol ();
  test_btc ();
  test_btc_descriptor ();
  test_sui ();
  test_tao ();
  test_calldata ();
  test_erc20 ();
  test_zerion ();
  test_zec ();
  test_prices ();
  summary ()
