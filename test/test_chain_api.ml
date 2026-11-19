open Wallet_watcher
open Test_util

let test_hex () =
  print_endline "hex -> float";
  check_ok_float "zero" ~expect:0. (Chain_api.float_of_hex "0x0");
  check_ok_float "one wei" ~expect:1. (Chain_api.float_of_hex "0x1");
  check_ok_float "0xff" ~expect:255. (Chain_api.float_of_hex "0xFF");
  check_ok_float "no 0x prefix" ~expect:255. (Chain_api.float_of_hex "ff");
  check_ok_float "1e18 wei" ~expect:1e18 (Chain_api.float_of_hex "0xde0b6b3a7640000");
  (* 10^21 wei exceeds max_int and would wrap if parsed as int. *)
  check_ok_float "1e21 wei exceeds max_int" ~expect:1e21
    (Chain_api.float_of_hex "0x3635c9adc5dea00000");
  check_error "rejects garbage" (Chain_api.float_of_hex "0xzz");
  check_error "rejects empty" (Chain_api.float_of_hex "0x")

let test_eth () =
  print_endline "ETH decoder";
  check_ok_float "1 ETH" ~expect:1.
    (Chain_api.parse_eth_balance
       (json {|{"jsonrpc":"2.0","id":1,"result":"0xde0b6b3a7640000"}|}));
  check_ok_float "zero balance" ~expect:0.
    (Chain_api.parse_eth_balance (json {|{"jsonrpc":"2.0","id":1,"result":"0x0"}|}));
  check_ok_float "1000 ETH" ~expect:1000.
    (Chain_api.parse_eth_balance
       (json {|{"jsonrpc":"2.0","id":1,"result":"0x3635c9adc5dea00000"}|}));
  check_error "surfaces rpc error"
    (Chain_api.parse_eth_balance
       (json {|{"jsonrpc":"2.0","id":1,"error":{"code":-32602,"message":"bad address"}}|}));
  check_error "missing result" (Chain_api.parse_eth_balance (json {|{"jsonrpc":"2.0"}|}))

let test_sol () =
  print_endline "SOL decoder";
  check_ok_float "lamports -> SOL" ~expect:1.23456789
    (Chain_api.parse_sol_balance
       (json
          {|{"jsonrpc":"2.0","result":{"context":{"slot":1},"value":1234567890},"id":1}|}));
  check_ok_float "zero" ~expect:0.
    (Chain_api.parse_sol_balance
       (json {|{"jsonrpc":"2.0","result":{"context":{"slot":1},"value":0},"id":1}|}));
  check_error "surfaces rpc error"
    (Chain_api.parse_sol_balance
       (json {|{"jsonrpc":"2.0","error":{"message":"Invalid param"},"id":1}|}))

let test_btc () =
  print_endline "BTC decoder";
  (* funded 1.0 BTC, spent 0.4 BTC -> 0.6 BTC *)
  check_ok_float "funded - spent" ~expect:0.6
    (Chain_api.parse_btc_balance
       (json
          {|{"address":"bc1q","chain_stats":{"funded_txo_sum":100000000,
             "spent_txo_sum":40000000,"tx_count":3},
             "mempool_stats":{"funded_txo_sum":0,"spent_txo_sum":0}}|}));
  check_ok_float "fully spent" ~expect:0.
    (Chain_api.parse_btc_balance
       (json
          {|{"chain_stats":{"funded_txo_sum":50000,"spent_txo_sum":50000}}|}));
  check_error "missing chain_stats"
    (Chain_api.parse_btc_balance (json {|{"address":"bc1q"}|}))

let test_sui () =
  print_endline "SUI decoder";
  (* Only the native coin type counts; the USDC row must be ignored. *)
  check_ok_float "native SUI only" ~expect:2.5
    (Chain_api.parse_sui_balance
       (json
          {|{"jsonrpc":"2.0","result":[
             {"coinType":"0x2::sui::SUI","coinObjectCount":2,"totalBalance":"2500000000"},
             {"coinType":"0xabc::usdc::USDC","coinObjectCount":1,"totalBalance":"9999000000"}
            ],"id":1}|}));
  check_ok_float "no SUI holdings" ~expect:0.
    (Chain_api.parse_sui_balance (json {|{"jsonrpc":"2.0","result":[],"id":1}|}));
  check_error "surfaces rpc error"
    (Chain_api.parse_sui_balance
       (json {|{"jsonrpc":"2.0","error":{"message":"bad address"},"id":1}|}))

let test_tao () =
  print_endline "TAO decoder";
  check_ok_float "data[0].balance_total" ~expect:12.5
    (Chain_api.parse_tao_balance
       (json {|{"data":[{"balance_total":"12500000000","balance_free":"1"}]}|}));
  check_ok_float "flat balance_free" ~expect:2.
    (Chain_api.parse_tao_balance (json {|{"balance_free":2000000000}|}));
  check_error "unrecognised shape"
    (Chain_api.parse_tao_balance (json {|{"unexpected":true}|}))

let test_prices () =
  print_endline "price decoder";
  let payload =
    json
      {|{"ethereum":{"usd":3000.5},"bitcoin":{"usd":65000},"solana":{"usd":150.25},
         "avalanche-2":{"usd":30.5},"zcash":{"usd":45}}|}
  in
  match
    Chain_api.parse_prices Config.[ ETH; BTC; SUI; AVAX; ZEC ] payload
  with
  | Error e -> report "parse_prices" false e
  | Ok prices ->
    check "prices: SUI absent, four found" (List.length prices = 4);
    check_float "ETH price" ~expect:3000.5 (List.assoc Config.ETH prices);
    check_float "BTC price" ~expect:65000. (List.assoc Config.BTC prices);
    check_float "AVAX price" ~expect:30.5 (List.assoc Config.AVAX prices);
    check_float "ZEC price" ~expect:45. (List.assoc Config.ZEC prices);
    check "SUI omitted" (List.assoc_opt Config.SUI prices = None)

let vitalik = "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045"

let test_calldata () =
  print_endline "balanceOf calldata";
  (match Chain_api.balance_of_calldata vitalik with
   | Ok data ->
     check_string "known vector"
       ~expect:"0x70a08231000000000000000000000000d8da6bf26964af9d7eed9e03e53415d37aa96045"
       data
   | Error e -> report "known vector" false e);
  check_error "rejects short address" (Chain_api.balance_of_calldata "0xabc");
  check_error "rejects missing 0x"
    (Chain_api.balance_of_calldata "d8dA6BF26964aF9D7eEd9e03E53415D37aA96045ab");
  check_error "rejects non-hex"
    (Chain_api.balance_of_calldata "0xZZdA6BF26964aF9D7eEd9e03E53415D37aA96045")

let test_erc20 () =
  print_endline "ERC-20 decoder";
  check_ok_float "1.5 tokens at 18 decimals" ~expect:1.5
    (Chain_api.parse_erc20_balance ~decimals:18
       (json
          {|{"jsonrpc":"2.0","id":1,"result":"0x00000000000000000000000000000000000000000000000014d1120d7b160000"}|}));
  check_ok_float "6 decimals" ~expect:2.
    (Chain_api.parse_erc20_balance ~decimals:6
       (json {|{"jsonrpc":"2.0","id":1,"result":"0x1e8480"}|}));
  check_error "surfaces rpc error"
    (Chain_api.parse_erc20_balance ~decimals:18
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
    (Chain_api.parse_zec_balance address payload);
  check_error "missing address key" (Chain_api.parse_zec_balance "t1other" payload);
  check "shielded address rejected without a request"
    (match Lwt_main.run (Chain_api.fetch_zec "zs1xyz") with
     | Error "ZEC: shielded addresses are not supported" -> true
     | _ -> false)

let () =
  test_hex ();
  test_eth ();
  test_sol ();
  test_btc ();
  test_sui ();
  test_tao ();
  test_calldata ();
  test_erc20 ();
  test_zec ();
  test_prices ();
  summary ()
