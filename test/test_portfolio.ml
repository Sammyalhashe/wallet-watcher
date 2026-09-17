(* Tests for the pure layers: response decoding, share math, formatting.
   No network: every decoder is fed a recorded payload. *)

open Portfolio_lib

let failures = ref 0
let passes = ref 0

let report name ok detail =
  if ok then (incr passes; Printf.printf "  ok   %s\n" name)
  else begin
    incr failures;
    Printf.printf "  FAIL %s: %s\n" name detail
  end

let check name ok = report name ok "condition was false"

(* Balances are floats, so compare with a relative tolerance. *)
let close ?(eps = 1e-9) a b =
  let scale = Float.max 1.0 (Float.max (Float.abs a) (Float.abs b)) in
  Float.abs (a -. b) <= eps *. scale

let check_float name ~expect actual =
  report name (close expect actual)
    (Printf.sprintf "expected %.10g, got %.10g" expect actual)

let check_ok_float name ~expect = function
  | Ok v -> check_float name ~expect v
  | Error e -> report name false ("unexpected Error: " ^ e)

let check_error name = function
  | Error _ -> report name true ""
  | Ok _ -> report name false "expected Error, got Ok"

let check_string name ~expect actual =
  report name (String.equal expect actual)
    (Printf.sprintf "expected %S, got %S" expect actual)

let json = Yojson.Basic.from_string

(* {1 Hex / wei arithmetic} *)

let test_hex () =
  print_endline "hex -> float";
  check_ok_float "zero" ~expect:0. (Chain_api.float_of_hex "0x0");
  check_ok_float "one wei" ~expect:1. (Chain_api.float_of_hex "0x1");
  check_ok_float "0xff" ~expect:255. (Chain_api.float_of_hex "0xFF");
  check_ok_float "no 0x prefix" ~expect:255. (Chain_api.float_of_hex "ff");
  (* 1 ETH = 10^18 wei. *)
  check_ok_float "1e18 wei" ~expect:1e18 (Chain_api.float_of_hex "0xde0b6b3a7640000");
  (* 1000 ETH = 10^21 wei, far beyond OCaml's max_int (~4.6e18): this is the
     case that would silently wrap if balances were parsed as int. *)
  check_ok_float "1e21 wei exceeds max_int" ~expect:1e21
    (Chain_api.float_of_hex "0x3635c9adc5dea00000");
  check_error "rejects garbage" (Chain_api.float_of_hex "0xzz");
  check_error "rejects empty" (Chain_api.float_of_hex "0x")

(* {1 Per-chain decoders} *)

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
      {|{"ethereum":{"usd":3000.5},"bitcoin":{"usd":65000},"solana":{"usd":150.25}}|}
  in
  match Chain_api.parse_prices [ Config.ETH; Config.BTC; Config.SUI ] payload with
  | Error e -> report "parse_prices" false e
  | Ok prices ->
    check "prices: ETH and BTC found, SUI absent" (List.length prices = 2);
    check_float "ETH price" ~expect:3000.5 (List.assoc Config.ETH prices);
    check_float "BTC price" ~expect:65000. (List.assoc Config.BTC prices);
    check "SUI omitted" (List.assoc_opt Config.SUI prices = None)

(* {1 Config parsing} *)

let test_config () =
  print_endline "config parsing";
  let cfg =
    Config.of_json
      (json
         {|{"ETH":[{"label":"Main","address":"0xaaa"},{"label":"Cold","address":"0xbbb"}],
            "BTC":[{"label":"Vault","address":"bc1q"}]}|})
  in
  check "three wallets" (List.length cfg = 3);
  let eth = List.filter (fun (w : Config.wallet) -> w.chain = Config.ETH) cfg in
  check "two ETH wallets" (List.length eth = 2);
  check_string "first label" ~expect:"Main" (List.hd eth).label;
  check_string "chain name" ~expect:"BTC" (Config.string_of_chain Config.BTC);
  check_string "coingecko id" ~expect:"bittensor" (Config.coingecko_id Config.TAO);
  let raises f = try f () |> ignore; false with Config.Parse_error _ -> true in
  check "rejects unknown chain"
    (raises (fun () -> Config.of_json (json {|{"DOGE":[]}|})));
  check "rejects missing address"
    (raises (fun () -> Config.of_json (json {|{"ETH":[{"label":"x"}]}|})));
  check "rejects non-object root"
    (raises (fun () -> Config.of_json (json {|[1,2,3]|})))

(* {1 Portfolio analytics} *)

let wallet chain label address = { Config.chain; label; address }

let test_shares () =
  print_endline "share and valuation math";
  let snapshot =
    { Chain_api.balances =
        [ (wallet Config.ETH "A" "0xa", Ok 3.0);
          (wallet Config.ETH "B" "0xb", Ok 1.0);
          (wallet Config.BTC "C" "bc1", Ok 0.5) ];
      prices = [ (Config.ETH, 2000.); (Config.BTC, 60000.) ];
      price_error = None }
  in
  let p = Portfolio.of_snapshot snapshot in
  check "two assets" (List.length p.assets = 2);
  (* Config order is preserved: ETH appeared first. *)
  let eth = List.nth p.assets 0 and btc = List.nth p.assets 1 in
  check "ETH first" (eth.chain = Config.ETH);
  check "BTC second" (btc.chain = Config.BTC);
  check_float "ETH total amount" ~expect:4.0 eth.total_amount;
  check_float "ETH total usd" ~expect:8000. (Option.get eth.total_usd);
  check_float "BTC total usd" ~expect:30000. (Option.get btc.total_usd);
  (* 3/4 and 1/4 of the ETH position. *)
  let a = List.nth eth.holdings 0 and b = List.nth eth.holdings 1 in
  check_float "wallet A share" ~expect:75. (Option.get a.share);
  check_float "wallet B share" ~expect:25. (Option.get b.share);
  check_float "wallet A usd" ~expect:6000. (Option.get a.usd);
  let sum =
    List.fold_left (fun acc (h : Portfolio.holding) -> acc +. Option.get h.share) 0.
      eth.holdings
  in
  check_float "shares sum to 100" ~expect:100. sum;
  check_float "portfolio net worth" ~expect:38000. p.total_usd;
  check "no errors" (Portfolio.errors p = [])

let test_partial_failure () =
  print_endline "degradation on partial failure";
  let snapshot =
    { Chain_api.balances =
        [ (wallet Config.ETH "Good" "0xa", Ok 2.0);
          (wallet Config.ETH "Dead" "0xb", Error "timed out");
          (* priced asset missing from the price table *)
          (wallet Config.SUI "S" "0x1", Ok 10.0) ];
      prices = [ (Config.ETH, 1000.) ];
      price_error = None }
  in
  let p = Portfolio.of_snapshot snapshot in
  let eth = List.nth p.assets 0 and sui = List.nth p.assets 1 in
  check_float "failed wallet excluded from total" ~expect:2.0 eth.total_amount;
  let good = List.nth eth.holdings 0 and dead = List.nth eth.holdings 1 in
  check_float "surviving wallet holds 100%" ~expect:100. (Option.get good.share);
  check "failed wallet has no share" (dead.share = None);
  check "failed wallet has no usd" (dead.usd = None);
  check "unpriced asset has no usd total" (sui.total_usd = None);
  check "unpriced holding has no usd" ((List.hd sui.holdings).usd = None);
  check_float "net worth counts only priced assets" ~expect:2000. p.total_usd;
  check "one error reported" (List.length (Portfolio.errors p) = 1)

let test_zero_balances () =
  print_endline "zero-balance asset";
  let snapshot =
    { Chain_api.balances = [ (wallet Config.SOL "Empty" "so1", Ok 0.0) ];
      prices = [ (Config.SOL, 150.) ];
      price_error = None }
  in
  let p = Portfolio.of_snapshot snapshot in
  let sol = List.hd p.assets in
  (* No division by zero: share is undefined rather than NaN. *)
  check "share undefined when total is zero" ((List.hd sol.holdings).share = None);
  check_float "zero net worth" ~expect:0. p.total_usd

(* {1 Formatting} *)

let test_format () =
  print_endline "formatting";
  check_string "cents" ~expect:"$0.50" (Tui.fmt_usd 0.5);
  check_string "no grouping under 1000" ~expect:"$999.00" (Tui.fmt_usd 999.);
  check_string "groups thousands" ~expect:"$1,000.00" (Tui.fmt_usd 1000.);
  check_string "groups millions" ~expect:"$1,234,567.89" (Tui.fmt_usd 1234567.891);
  check_string "groups billions" ~expect:"$1,000,000,000.00" (Tui.fmt_usd 1e9);
  check_string "share" ~expect:"75.00%" (Tui.fmt_share 75.);
  check_string "amount" ~expect:"1.500000" (Tui.fmt_amount 1.5);
  check_string "short address" ~expect:"0xd8dA6BF2..A96045"
    (Tui.short_address "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045");
  check_string "short address leaves small ones alone" ~expect:"0xabc"
    (Tui.short_address "0xabc");
  (* Padding must be exact or the table columns drift. *)
  check_string "pad left" ~expect:"ab   " (Tui.pad `Left 5 "ab");
  check_string "pad right" ~expect:"   ab" (Tui.pad `Right 5 "ab");
  check_string "pad leaves oversized alone" ~expect:"abcdef" (Tui.pad `Left 3 "abcdef");
  check_string "truncate" ~expect:"abc.." (Tui.truncate 5 "abcdefgh")

(* {1 Allocation and bar apportionment} *)

let test_allocation () =
  print_endline "allocation across assets";
  let snapshot =
    { Chain_api.balances =
        [ (wallet Config.BTC "V" "bc1", Ok 1.0);
          (wallet Config.ETH "E" "0xa", Ok 10.0);
          (wallet Config.SUI "S" "0x1", Ok 5.0) ];
      (* SUI unpriced on purpose. *)
      prices = [ (Config.BTC, 60000.); (Config.ETH, 3000.) ];
      price_error = None }
  in
  let p = Portfolio.of_snapshot snapshot in
  let alloc = Portfolio.allocation p in
  check "one entry per asset" (List.length alloc = 3);
  let pct_of chain =
    List.find_map
      (fun ((a : Portfolio.asset), pct) -> if a.chain = chain then Some pct else None)
      alloc
    |> Option.get
  in
  (* 60k BTC + 30k ETH = 90k net worth. *)
  check_float "BTC is 2/3 of net worth" ~expect:66.6666666667 (Option.get (pct_of Config.BTC));
  check_float "ETH is 1/3 of net worth" ~expect:33.3333333333 (Option.get (pct_of Config.ETH));
  check "unpriced asset has no allocation" (pct_of Config.SUI = None);
  let sum =
    List.filter_map (fun (_, pct) -> pct) alloc |> List.fold_left ( +. ) 0.
  in
  check_float "priced allocations sum to 100" ~expect:100. sum

let slice name pct = { Tui.s_name = name; s_pct = Some pct; s_value = "" }
let sum_cells c = Array.fold_left ( + ) 0 c

let test_bar_cells () =
  print_endline "stacked bar apportionment";
  let cells = Tui.allocate_cells 40 [ slice "a" 50.; slice "b" 50. ] in
  check "even split fills the bar" (sum_cells cells = 40);
  check "even split is even" (cells.(0) = 20 && cells.(1) = 20);
  let cells = Tui.allocate_cells 40 [ slice "a" 33.33; slice "b" 33.33; slice "c" 33.34 ] in
  check "thirds still fill the bar exactly" (sum_cells cells = 40);
  (* A 1.23% slice of a 42-wide bar rounds below one cell; it must still show. *)
  let cells = Tui.allocate_cells 42 [ slice "big" 98.77; slice "small" 1.23 ] in
  check "tiny slice survives rounding" (cells.(1) >= 1);
  check "tiny slice does not overflow the bar" (sum_cells cells = 42);
  (* The one-cell floor must claw back rather than overrun. *)
  let many = slice "big" 91. :: List.init 9 (fun i -> slice (string_of_int i) 1.) in
  let cells = Tui.allocate_cells 10 many in
  check "never exceeds the bar width" (sum_cells cells <= 10);
  (* More slices than cells: floor is dropped rather than looping forever. *)
  let cells = Tui.allocate_cells 3 (List.init 12 (fun i -> slice (string_of_int i) 8.)) in
  check "degenerate width stays within bounds" (sum_cells cells <= 3);
  let cells = Tui.allocate_cells 20 [ slice "a" 0.; slice "b" 0. ] in
  check "all-zero yields an empty bar" (sum_cells cells = 0);
  (* Unknown percentages take no width but keep their legend slot. *)
  let cells =
    Tui.allocate_cells 20
      [ slice "priced" 100.; { Tui.s_name = "unpriced"; s_pct = None; s_value = "" } ]
  in
  check "unpriced slice takes no cells" (cells.(1) = 0);
  check "priced slice takes the whole bar" (cells.(0) = 20)

let test_fold_extra () =
  print_endline "slice folding";
  let nine = List.init 9 (fun i -> slice (Printf.sprintf "w%d" i) 10.) in
  let folded = Tui.fold_extra nine in
  check "folds down to the palette size" (List.length folded = 8);
  let last = List.nth folded 7 in
  check "tail is bucketed as Other" (String.length last.Tui.s_name >= 5);
  check_float "Other carries the combined share" ~expect:20.
    (Option.get last.Tui.s_pct);
  let eight = List.init 8 (fun i -> slice (Printf.sprintf "w%d" i) 12.5) in
  check "exactly eight is left alone" (List.length (Tui.fold_extra eight) = 8)

(* {1 Runner} *)

let () =
  test_hex ();
  test_eth ();
  test_sol ();
  test_btc ();
  test_sui ();
  test_tao ();
  test_prices ();
  test_config ();
  test_shares ();
  test_partial_failure ();
  test_zero_balances ();
  test_allocation ();
  test_bar_cells ();
  test_fold_extra ();
  test_format ();
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1
