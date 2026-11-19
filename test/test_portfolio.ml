(* Pure layers: snapshot flattening, the four aggregations, navigation, formatting. *)

open Wallet_watcher
module Tui = Wallet_watcher_tui.Tui

let failures = ref 0
let passes = ref 0

let report name ok detail =
  if ok then (incr passes; Printf.printf "  ok   %s\n" name)
  else begin
    incr failures;
    Printf.printf "  FAIL %s: %s\n" name detail
  end

let check name ok = report name ok "condition was false"

let close ?(eps = 1e-9) a b =
  let scale = Float.max 1.0 (Float.max (Float.abs a) (Float.abs b)) in
  Float.abs (a -. b) <= eps *. scale

let check_float name ~expect actual =
  report name (close expect actual)
    (Printf.sprintf "expected %.10g, got %.10g" expect actual)

let check_string name ~expect actual =
  report name (String.equal expect actual)
    (Printf.sprintf "expected %S, got %S" expect actual)

let summary () =
  Printf.printf "\n%d passed, %d failed\n" !passes !failures;
  if !failures > 0 then exit 1

(* {1 Fixtures} *)

let balance wallet network result : Snapshot.balance =
  { wallet; network; address = "addr-" ^ wallet; result }

let snapshot_of ?(prices = []) balances : Snapshot.t =
  { balances; prices; price_error = None }

let names slices = List.map (fun (s : Portfolio.slice) -> s.name) slices

let pct_sum slices =
  List.fold_left
    (fun acc (s : Portfolio.slice) -> acc +. Option.value s.pct ~default:0.)
    0. slices

let find name slices =
  List.find (fun (s : Portfolio.slice) -> s.name = name) slices

let two_wallets =
  snapshot_of
    ~prices:[ (Config.BTC, 60000.); (Config.ETH, 2000.); (Config.AVAX, 10.) ]
    [ balance "A" Config.Bitcoin (Ok [ (Config.BTC, 1.) ]);
      balance "A" Config.Ethereum (Ok [ (Config.ETH, 1.) ]);
      balance "A" Config.Base (Ok [ (Config.ETH, 2.) ]);
      balance "A" Config.Avalanche (Ok [ (Config.AVAX, 100.); (Config.ETH, 1.) ]);
      balance "B" Config.Ethereum (Ok [ (Config.ETH, 4.) ]);
      balance "B" Config.Solana (Error "timed out") ]

(* {1 Tests} *)

let test_of_snapshot () =
  print_endline "of_snapshot flattening";
  let p = Portfolio.of_snapshot two_wallets in
  check "one position per (network, asset)" (List.length p.positions = 6);
  check "failure is kept apart" (List.length p.failures = 1);
  check_string "failure carries error" ~expect:"timed out" (List.hd p.failures).error;
  check "failure carries network" ((List.hd p.failures).network = Config.Solana);
  check "avalanche yields two positions"
    (List.length
       (List.filter (fun (x : Portfolio.position) -> x.network = Config.Avalanche) p.positions)
     = 2);
  check_float "net worth sums every priced position" ~expect:77000. p.total_usd

let test_by_asset () =
  print_endline "by_asset";
  let slices = Portfolio.by_asset (Portfolio.of_snapshot two_wallets) in
  check "sorted by usd desc" (names slices = [ "BTC"; "ETH"; "AVAX" ]);
  check_float "pcts sum to 100" ~expect:100. (pct_sum slices);
  check_float "BTC share of net worth" ~expect:(60000. /. 77000. *. 100.)
    (Option.get (find "BTC" slices).pct);
  check_float "ETH units merge across networks" ~expect:8.
    (Option.get (find "ETH" slices).amount)

let test_by_wallet () =
  print_endline "by_wallet";
  let slices = Portfolio.by_wallet (Portfolio.of_snapshot two_wallets) in
  check "failed-only networks do not add a wallet" (names slices = [ "A"; "B" ]);
  check_float "pcts sum to 100" ~expect:100. (pct_sum slices);
  check_float "A usd" ~expect:69000. (Option.get (find "A" slices).usd);
  check "amount is not meaningful" ((find "A" slices).amount = None)

let test_asset_by_wallet () =
  print_endline "asset_by_wallet";
  let p = Portfolio.of_snapshot two_wallets in
  let slices = Portfolio.asset_by_wallet p Config.ETH in
  check "wallets in a tie-free order" (names slices = [ "B"; "A" ] || names slices = [ "A"; "B" ]);
  check_float "ETH on ethereum+base+avalanche merges for A" ~expect:4.
    (Option.get (find "A" slices).amount);
  check_float "A holds half of all ETH" ~expect:50. (Option.get (find "A" slices).pct);
  check_float "pcts sum to 100" ~expect:100. (pct_sum slices)

let test_wallet_by_asset () =
  print_endline "wallet_by_asset";
  let slices = Portfolio.wallet_by_asset (Portfolio.of_snapshot two_wallets) "A" in
  check "usd order" (names slices = [ "BTC"; "ETH"; "AVAX" ]);
  check_float "pcts sum to 100" ~expect:100. (pct_sum slices);
  check_float "ETH units inside A" ~expect:4. (Option.get (find "ETH" slices).amount);
  check_float "ETH share of A" ~expect:(8000. /. 69000. *. 100.)
    (Option.get (find "ETH" slices).pct);
  check "unknown wallet is empty"
    (Portfolio.wallet_by_asset (Portfolio.of_snapshot two_wallets) "Z" = [])

let test_unpriced () =
  print_endline "unpriced assets";
  let p =
    Portfolio.of_snapshot
      (snapshot_of ~prices:[ (Config.BTC, 100.) ]
         [ balance "A" Config.Bitcoin (Ok [ (Config.BTC, 2.) ]);
           balance "A" Config.Sui (Ok [ (Config.SUI, 500.) ]);
           balance "B" Config.Sui (Ok [ (Config.SUI, 100.) ]) ])
  in
  let slices = Portfolio.by_asset p in
  check "unpriced sorts last" (names slices = [ "BTC"; "SUI" ]);
  check "unpriced has no usd" ((find "SUI" slices).usd = None);
  check "unpriced pct is unknown, not 0" ((find "SUI" slices).pct = None);
  check_float "priced coin takes the whole pie" ~expect:100.
    (Option.get (find "BTC" slices).pct);
  check_float "net worth ignores unpriced" ~expect:200. p.total_usd;
  let by_wallet = Portfolio.by_wallet p in
  check "wallet holding only unpriced coins has no pct"
    ((find "B" by_wallet).pct = None && (find "B" by_wallet).usd = None);
  check "unpriced wallet sorts last" (names by_wallet = [ "A"; "B" ]);
  let sui_split = Portfolio.asset_by_wallet p Config.SUI in
  check "units still split an unpriced coin" (names sui_split = [ "A"; "B" ]);
  check_float "A holds 5/6 of SUI" ~expect:(500. /. 600. *. 100.)
    (Option.get (find "A" sui_split).pct);
  check "no usd for unpriced split" ((find "A" sui_split).usd = None)

let test_zero_balances () =
  print_endline "zero balances";
  let p =
    Portfolio.of_snapshot
      (snapshot_of ~prices:[ (Config.SOL, 100.); (Config.BTC, 100.) ]
         [ balance "A" Config.Solana (Ok [ (Config.SOL, 0.) ]);
           balance "A" Config.Bitcoin (Ok [ (Config.BTC, 1.) ]);
           balance "Empty" Config.Solana (Ok [ (Config.SOL, 0.) ]) ])
  in
  check "positions keep the zero entry" (List.length p.positions = 3);
  check "by_asset omits zero" (names (Portfolio.by_asset p) = [ "BTC" ]);
  check "by_wallet omits zero-only wallets" (names (Portfolio.by_wallet p) = [ "A" ]);
  check "asset_by_wallet omits zero" (Portfolio.asset_by_wallet p Config.SOL = []);
  check "wallet_by_asset omits zero"
    (names (Portfolio.wallet_by_asset p "A") = [ "BTC" ]);
  let sol = List.find (fun (b : Portfolio.asset_block) -> b.coin = Config.SOL) (Portfolio.table p) in
  check "table share undefined at zero total"
    (List.for_all (fun (r : Portfolio.row) -> r.share = None) sol.rows)

let test_table () =
  print_endline "table blocks";
  let blocks = Portfolio.table (Portfolio.of_snapshot two_wallets) in
  check "blocks follow first appearance" (List.map (fun (b : Portfolio.asset_block) -> b.coin) blocks
                                          = [ Config.BTC; Config.ETH; Config.AVAX ]);
  let eth = List.nth blocks 1 in
  check "one row per position" (List.length eth.rows = 4);
  check_float "subtotal amount" ~expect:8. eth.subtotal_amount;
  check_float "subtotal usd" ~expect:16000. (Option.get eth.subtotal_usd);
  check_float "row shares sum to 100" ~expect:100.
    (List.fold_left (fun acc (r : Portfolio.row) -> acc +. Option.get r.share) 0. eth.rows)

(* {1 Navigation} *)

let nav_portfolio = Portfolio.of_snapshot two_wallets
let pie_at focus selected = { Tui.view = Tui.Pie; focus; selected }
let step state event = Option.get (Tui.step nav_portfolio state event)

let test_navigation () =
  print_endline "navigation";
  let root = pie_at Tui.Root_by_coin 0 in
  check "initial view is the table" (Tui.initial.view = Tui.Table);
  check "c toggles view" ((step Tui.initial Tui.Toggle_view).view = Tui.Pie);
  check "quit ends the loop" (Tui.step nav_portfolio root Tui.Quit = None);
  check "move down" ((step root (Tui.Move 1)).selected = 1);
  check "move clamps at top" ((step root (Tui.Move (-1))).selected = 0);
  check "move clamps at bottom" ((step root (Tui.Move 10)).selected = 2);
  check "moves ignored in table"
    ((step { root with view = Tui.Table } (Tui.Move 1)).selected = 0);
  check "drill coin row" ((step root Tui.Drill).focus = Tui.Coin Config.BTC);
  check "drill resets selection" ((step (pie_at Tui.Root_by_coin 1) Tui.Drill).selected = 0);
  check "drill second coin"
    ((step (pie_at Tui.Root_by_coin 1) Tui.Drill).focus = Tui.Coin Config.ETH);
  check "tab at root switches" ((step root Tui.Switch_root).focus = Tui.Root_by_wallet);
  check "tab twice returns"
    ((step (step root Tui.Switch_root) Tui.Switch_root).focus = Tui.Root_by_coin);
  check "drill wallet row"
    ((step (pie_at Tui.Root_by_wallet 0) Tui.Drill).focus = Tui.Wallet "A");
  check "tab is a no-op when drilled"
    ((step (pie_at (Tui.Coin Config.BTC) 0) Tui.Switch_root).focus = Tui.Coin Config.BTC);
  check "drill is a no-op on a leaf"
    ((step (pie_at (Tui.Coin Config.ETH) 0) Tui.Drill).focus = Tui.Coin Config.ETH);
  let back = step (pie_at (Tui.Coin Config.ETH) 0) Tui.Up in
  check "up from coin returns to coin root" (back.focus = Tui.Root_by_coin);
  check "up restores the drilled row" (back.selected = 1);
  check "up from wallet returns to wallet root"
    ((step (pie_at (Tui.Wallet "B") 0) Tui.Up).focus = Tui.Root_by_wallet);
  check "up at root is a no-op" (step root Tui.Up = root);
  check "toggle keeps focus"
    ((step (pie_at (Tui.Coin Config.BTC) 1) Tui.Toggle_view).focus = Tui.Coin Config.BTC)

let test_other_not_drillable () =
  print_endline "folded Other row";
  let many =
    List.init 10 (fun i ->
        balance (Printf.sprintf "W%d" i) Config.Bitcoin (Ok [ (Config.BTC, float_of_int (10 - i)) ]))
  in
  let p = Portfolio.of_snapshot (snapshot_of ~prices:[ (Config.BTC, 1.) ] many) in
  let rows = Tui.rows p Tui.Root_by_wallet in
  check "folds to palette size" (List.length rows = 8);
  check_string "last row is Other" ~expect:"Other (3)" (List.nth rows 7).name;
  let state = pie_at Tui.Root_by_wallet 7 in
  check "Other does not drill" (Tui.step p state Tui.Drill = Some state);
  check_float "folded pcts still sum to 100" ~expect:100. (pct_sum rows)

let test_key_mapping () =
  print_endline "key mapping";
  let key k = `Key (k, []) in
  check "q quits" (Tui.event_of_input (key (`ASCII 'q')) = Some Tui.Quit);
  check "ctrl-c quits" (Tui.event_of_input (`Key (`ASCII 'C', [ `Ctrl ])) = Some Tui.Quit);
  check "c toggles" (Tui.event_of_input (key (`ASCII 'c')) = Some Tui.Toggle_view);
  check "j moves down" (Tui.event_of_input (key (`ASCII 'j')) = Some (Tui.Move 1));
  check "up arrow moves up" (Tui.event_of_input (key (`Arrow `Up)) = Some (Tui.Move (-1)));
  check "enter drills" (Tui.event_of_input (key `Enter) = Some Tui.Drill);
  check "l drills" (Tui.event_of_input (key (`ASCII 'l')) = Some Tui.Drill);
  check "backspace goes up" (Tui.event_of_input (key `Backspace) = Some Tui.Up);
  check "escape goes up" (Tui.event_of_input (key `Escape) = Some Tui.Up);
  check "tab switches" (Tui.event_of_input (key `Tab) = Some Tui.Switch_root);
  check "other keys ignored" (Tui.event_of_input (key (`ASCII 'z')) = None)

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
  check_string "pad left" ~expect:"ab   " (Tui.pad `Left 5 "ab");
  check_string "pad right" ~expect:"   ab" (Tui.pad `Right 5 "ab");
  check_string "pad leaves oversized alone" ~expect:"abcdef" (Tui.pad `Left 3 "abcdef");
  check_string "truncate" ~expect:"abc.." (Tui.truncate 5 "abcdefgh");
  check_string "breadcrumb root" ~expect:"NET WORTH > by coin" (Tui.breadcrumb Tui.Root_by_coin);
  check_string "breadcrumb coin" ~expect:"NET WORTH > BTC by wallet"
    (Tui.breadcrumb (Tui.Coin Config.BTC));
  check_string "breadcrumb wallet" ~expect:"NET WORTH > Ledger by coin"
    (Tui.breadcrumb (Tui.Wallet "Ledger"))

let () =
  test_of_snapshot ();
  test_by_asset ();
  test_by_wallet ();
  test_asset_by_wallet ();
  test_wallet_by_asset ();
  test_unpriced ();
  test_zero_balances ();
  test_table ();
  test_navigation ();
  test_other_not_drillable ();
  test_key_mapping ();
  test_format ();
  summary ()
