(* Pure layers: snapshot flattening, the four aggregations, navigation, formatting. *)

open Wallet_watcher
module Format = Wallet_watcher_tui.Format
module Nav = Wallet_watcher_tui.Nav
module Pie_view = Wallet_watcher_tui.Pie_view
module Screen = Wallet_watcher_tui.Screen
module Palette = Wallet_watcher_tui.Palette
module Table_view = Wallet_watcher_tui.Table_view

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
    ~prices:[ (Config.btc, 60000.); (Config.eth, 2000.); (Config.avax, 10.) ]
    [ balance "A" Config.Bitcoin (Ok [ (Snapshot.holding Config.btc 1.) ]);
      balance "A" Config.Ethereum (Ok [ (Snapshot.holding Config.eth 1.) ]);
      balance "A" Config.Base (Ok [ (Snapshot.holding Config.eth 2.) ]);
      balance "A" Config.Avalanche (Ok [ (Snapshot.holding Config.avax 100.); (Snapshot.holding Config.eth 1.) ]);
      balance "B" Config.Ethereum (Ok [ (Snapshot.holding Config.eth 4.) ]);
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
  check "a failed fetch makes the total partial" p.total_is_partial;
  check_float "net worth sums every priced position" ~expect:77000. (Option.get p.total_usd)

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
  let slices = Portfolio.asset_by_wallet p Config.eth in
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
      (snapshot_of ~prices:[ (Config.btc, 100.) ]
         [ balance "A" Config.Bitcoin (Ok [ (Snapshot.holding Config.btc 2.) ]);
           balance "A" Config.Sui (Ok [ (Snapshot.holding Config.sui 500.) ]);
           balance "B" Config.Sui (Ok [ (Snapshot.holding Config.sui 100.) ]) ])
  in
  let slices = Portfolio.by_asset p in
  check "unpriced sorts last" (names slices = [ "BTC"; "SUI" ]);
  check "unpriced has no usd" ((find "SUI" slices).usd = None);
  check "unpriced pct is unknown, not 0" ((find "SUI" slices).pct = None);
  check_float "priced coin takes the whole pie" ~expect:100.
    (Option.get (find "BTC" slices).pct);
  check_float "net worth ignores unpriced" ~expect:200. (Option.get p.total_usd);
  check "unpriced holding marks total partial" p.total_is_partial;
  let by_wallet = Portfolio.by_wallet p in
  check "wallet holding only unpriced coins has no pct"
    ((find "B" by_wallet).pct = None && (find "B" by_wallet).usd = None);
  check "unpriced wallet sorts last" (names by_wallet = [ "A"; "B" ]);
  let sui_split = Portfolio.asset_by_wallet p Config.sui in
  check "units still split an unpriced coin" (names sui_split = [ "A"; "B" ]);
  check_float "A holds 5/6 of SUI" ~expect:(500. /. 600. *. 100.)
    (Option.get (find "A" sui_split).pct);
  check "no usd for unpriced split" ((find "A" sui_split).usd = None)

let test_zero_balances () =
  print_endline "zero balances";
  let p =
    Portfolio.of_snapshot
      (snapshot_of ~prices:[ (Config.sol, 100.); (Config.btc, 100.) ]
         [ balance "A" Config.Solana (Ok [ (Snapshot.holding Config.sol 0.) ]);
           balance "A" Config.Bitcoin (Ok [ (Snapshot.holding Config.btc 1.) ]);
           balance "Empty" Config.Solana (Ok [ (Snapshot.holding Config.sol 0.) ]) ])
  in
  check "positions keep the zero entry" (List.length p.positions = 3);
  check "by_asset omits zero" (names (Portfolio.by_asset p) = [ "BTC" ]);
  check "by_wallet omits zero-only wallets" (names (Portfolio.by_wallet p) = [ "A" ]);
  check "asset_by_wallet omits zero" (Portfolio.asset_by_wallet p Config.sol = []);
  check "wallet_by_asset omits zero"
    (names (Portfolio.wallet_by_asset p "A") = [ "BTC" ]);
  let sol = List.find (fun (b : Portfolio.asset_block) -> b.coin = Config.sol) (Portfolio.table p) in
  check "table share undefined at zero total"
    (List.for_all (fun (r : Portfolio.row) -> r.share = None) sol.rows)

let test_table () =
  print_endline "table blocks";
  let blocks = Portfolio.table (Portfolio.of_snapshot two_wallets) in
  check "blocks follow first appearance" (List.map (fun (b : Portfolio.asset_block) -> b.coin) blocks
                                          = [ Config.btc; Config.eth; Config.avax ]);
  let eth = List.nth blocks 1 in
  check "one row per position" (List.length eth.rows = 4);
  check_float "subtotal amount" ~expect:8. eth.subtotal_amount;
  check_float "subtotal usd" ~expect:16000. (Option.get eth.subtotal_usd);
  check_float "row shares sum to 100" ~expect:100.
    (List.fold_left (fun acc (r : Portfolio.row) -> acc +. Option.get r.share) 0. eth.rows)

let self_valued ?detail asset amount usd : Snapshot.holding =
  { asset; amount; usd = Some usd; detail }

let with_defi =
  snapshot_of ~prices:[ (Config.btc, 100.) ]
    [ balance "A" Config.Bitcoin (Ok [ Snapshot.holding Config.btc 1. ]);
      balance "A" Config.Base
        (Ok [ self_valued ~detail:"Aave V3 deposit" "USDC" 500. 500.;
              self_valued ~detail:"Aave V3 loan" "USDC" (-200.) (-200.);
              self_valued Config.eth 0.1 300. ]) ]

let test_self_valued_and_loans () =
  print_endline "self-valued holdings and loans";
  let p = Portfolio.of_snapshot with_defi in
  check_float "net total subtracts the loan" ~expect:700. (Option.get p.total_usd);
  check "self-valued holdings count as priced" (not p.total_is_partial);
  let slices = Portfolio.by_asset p in
  check "slices by gross value" (names slices = [ "USDC"; "ETH"; "BTC" ]);
  check_float "slice excludes the loan" ~expect:500. (Option.get (find "USDC" slices).usd);
  check_float "pcts sum to 100" ~expect:100. (pct_sum slices);
  check_float "wallet slice is gross" ~expect:900.
    (Option.get (find "A" (Portfolio.by_wallet p)).usd);
  let usdc =
    List.find (fun (b : Portfolio.asset_block) -> b.coin = "USDC") (Portfolio.table p)
  in
  check "loan row stays in the table" (List.length usdc.rows = 2);
  check_float "subtotal is net" ~expect:300. (Option.get usdc.subtotal_usd);
  let loan = List.nth usdc.rows 1 in
  check_float "loan row is negative" ~expect:(-200.) (Option.get loan.usd_value);
  check "loan has no share" (loan.share = None);
  check_float "deposit holds all units" ~expect:100. (Option.get (List.hd usdc.rows).share);
  check_string "network cell shows detail" ~expect:"base - Aave V3 loan"
    (fst (Table_view.position_row loan).(1));
  check_string "self-valued subtotal has no price" ~expect:""
    (fst (Table_view.subtotal_row usdc).(3));
  check "drill into a token"
    (Nav.drill_target p { Nav.view = Nav.Pie; focus = Nav.Root_by_coin; selected = 0 }
     = Some (Nav.Coin "USDC"))

(* {1 Navigation} *)

let nav_portfolio = Portfolio.of_snapshot two_wallets
let pie_at focus selected = { Nav.view = Nav.Pie; focus; selected }
let step state event = Option.get (Nav.step nav_portfolio state event)

let test_navigation () =
  print_endline "navigation";
  let root = pie_at Nav.Root_by_coin 0 in
  check "initial view is the table" (Nav.initial.view = Nav.Table);
  check "c toggles view" ((step Nav.initial Nav.Toggle_view).view = Nav.Pie);
  check "quit ends the loop" (Nav.step nav_portfolio root Nav.Quit = None);
  check "move down" ((step root (Nav.Move 1)).selected = 1);
  check "move clamps at top" ((step root (Nav.Move (-1))).selected = 0);
  check "move clamps at bottom" ((step root (Nav.Move 10)).selected = 2);
  check "moves ignored in table"
    ((step { root with view = Nav.Table } (Nav.Move 1)).selected = 0);
  check "drill coin row" ((step root Nav.Drill).focus = Nav.Coin Config.btc);
  check "drill resets selection" ((step (pie_at Nav.Root_by_coin 1) Nav.Drill).selected = 0);
  check "drill second coin"
    ((step (pie_at Nav.Root_by_coin 1) Nav.Drill).focus = Nav.Coin Config.eth);
  check "tab at root switches" ((step root Nav.Switch_root).focus = Nav.Root_by_wallet);
  check "tab twice returns"
    ((step (step root Nav.Switch_root) Nav.Switch_root).focus = Nav.Root_by_coin);
  check "drill wallet row"
    ((step (pie_at Nav.Root_by_wallet 0) Nav.Drill).focus = Nav.Wallet "A");
  check "tab is a no-op when drilled"
    ((step (pie_at (Nav.Coin Config.btc) 0) Nav.Switch_root).focus = Nav.Coin Config.btc);
  check "drill is a no-op on a leaf"
    ((step (pie_at (Nav.Coin Config.eth) 0) Nav.Drill).focus = Nav.Coin Config.eth);
  let back = step (pie_at (Nav.Coin Config.eth) 0) Nav.Up in
  check "up from coin returns to coin root" (back.focus = Nav.Root_by_coin);
  check "up restores the drilled row" (back.selected = 1);
  check "up from wallet returns to wallet root"
    ((step (pie_at (Nav.Wallet "B") 0) Nav.Up).focus = Nav.Root_by_wallet);
  check "up at root is a no-op" (step root Nav.Up = root);
  check "toggle keeps focus"
    ((step (pie_at (Nav.Coin Config.btc) 1) Nav.Toggle_view).focus = Nav.Coin Config.btc)

let test_other_not_drillable () =
  print_endline "folded Other row";
  let many =
    List.init 10 (fun i ->
        balance (Printf.sprintf "W%d" i) Config.Bitcoin (Ok [ (Snapshot.holding Config.btc (float_of_int (10 - i))) ]))
  in
  let p = Portfolio.of_snapshot (snapshot_of ~prices:[ (Config.btc, 1.) ] many) in
  let rows = Nav.rows p Nav.Root_by_wallet in
  check "nav fold size matches palette" (Nav.max_slices = Palette.max_slices);
  check "folds to palette size" (List.length rows = 8);
  check "only the bucket is marked folded"
    (List.for_all (fun (s : Portfolio.slice) -> s.folded = (s.name = "Other (3)")) rows);
  check_string "last row is Other" ~expect:"Other (3)" (List.nth rows 7).name;
  let state = pie_at Nav.Root_by_wallet 7 in
  check "Other does not drill" (Nav.step p state Nav.Drill = Some state);
  check_float "folded pcts still sum to 100" ~expect:100. (pct_sum rows)

let test_key_mapping () =
  print_endline "key mapping";
  let key k = `Key (k, []) in
  check "q quits" (Nav.event_of_input (key (`ASCII 'q')) = Some Nav.Quit);
  check "ctrl-c quits" (Nav.event_of_input (`Key (`ASCII 'C', [ `Ctrl ])) = Some Nav.Quit);
  check "c toggles" (Nav.event_of_input (key (`ASCII 'c')) = Some Nav.Toggle_view);
  check "j moves down" (Nav.event_of_input (key (`ASCII 'j')) = Some (Nav.Move 1));
  check "up arrow moves up" (Nav.event_of_input (key (`Arrow `Up)) = Some (Nav.Move (-1)));
  check "enter drills" (Nav.event_of_input (key `Enter) = Some Nav.Drill);
  check "l drills" (Nav.event_of_input (key (`ASCII 'l')) = Some Nav.Drill);
  check "backspace goes up" (Nav.event_of_input (key `Backspace) = Some Nav.Up);
  check "escape goes up" (Nav.event_of_input (key `Escape) = Some Nav.Up);
  check "tab switches" (Nav.event_of_input (key `Tab) = Some Nav.Switch_root);
  check "other keys ignored" (Nav.event_of_input (key (`ASCII 'z')) = None)

(* {1 Formatting} *)

let test_format () =
  print_endline "formatting";
  check_string "cents" ~expect:"$0.50" (Format.fmt_usd 0.5);
  check_string "no grouping under 1000" ~expect:"$999.00" (Format.fmt_usd 999.);
  check_string "groups thousands" ~expect:"$1,000.00" (Format.fmt_usd 1000.);
  check_string "groups millions" ~expect:"$1,234,567.89" (Format.fmt_usd 1234567.891);
  check_string "groups billions" ~expect:"$1,000,000,000.00" (Format.fmt_usd 1e9);
  check_string "share" ~expect:"75.00%" (Format.fmt_share 75.);
  check_string "amount" ~expect:"1.500000" (Format.fmt_amount 1.5);
  check_string "short address" ~expect:"0xd8dA6BF2..A96045"
    (Format.short_address "0xd8dA6BF26964aF9D7eEd9e03E53415D37aA96045");
  check_string "short address leaves small ones alone" ~expect:"0xabc"
    (Format.short_address "0xabc");
  check_string "pad left" ~expect:"ab   " (Format.pad `Left 5 "ab");
  check_string "pad right" ~expect:"   ab" (Format.pad `Right 5 "ab");
  check_string "pad leaves oversized alone" ~expect:"abcdef" (Format.pad `Left 3 "abcdef");
  check_string "truncate" ~expect:"abc.." (Format.truncate 5 "abcdefgh");
  check_string "negative usd" ~expect:"-$123.00" (Format.fmt_usd (-123.));
  check_string "negative usd groups" ~expect:"-$1,234.50" (Format.fmt_usd (-1234.5));
  check_string "sanitize controls" ~expect:"a b  c d" (Format.sanitize "a\nb\r c\td");
  check_string "sanitize non-ascii" ~expect:"Caf?? x" (Format.sanitize "Caf\xc3\xa9 x");
  check_string "sanitize keeps ascii" ~expect:"Hello, w0rld!" (Format.sanitize "Hello, w0rld!");
  check_string "truncate sanitizes first" ~expect:"a b.."
    (Format.truncate 5 "a\nb cdefgh");
  check "truncate never splits into invalid bytes"
    (String.for_all (fun c -> Char.code c < 0x80) (Format.truncate 5 "\xc3\xa9\xc3\xa9\xc3\xa9\xc3\xa9"));
  check_string "breadcrumb root" ~expect:"NET WORTH > by coin" (Pie_view.breadcrumb Nav.Root_by_coin);
  check_string "breadcrumb coin" ~expect:"NET WORTH > BTC by wallet"
    (Pie_view.breadcrumb (Nav.Coin Config.btc));
  check_string "breadcrumb wallet" ~expect:"NET WORTH > Ledger by coin"
    (Pie_view.breadcrumb (Nav.Wallet "Ledger"))

let nasty_snapshot =
  snapshot_of ~prices:[ (Config.btc, 100.) ]
    [ balance "Caf\xc3\xa9 wallet with a long name" Config.Bitcoin (Ok [ (Snapshot.holding Config.btc 1.) ]);
      balance "Caf\xc3\xa9 wallet with a long name" Config.Base
        (Error "Line 1:\nInvalid token\r\n\xc3");
      balance "B" Config.Bitcoin (Ok [ (Snapshot.holding Config.btc 1.) ]) ]

let renders_without_raising state =
  match Screen.render state (Portfolio.of_snapshot nasty_snapshot) (80, 40) with
  | _ -> true
  | exception _ -> false

let test_render_is_safe () =
  print_endline "render with hostile text";
  List.iter
    (fun (name, state) -> check name (renders_without_raising state))
    [ ("table", Nav.initial);
      ("pie by coin", pie_at Nav.Root_by_coin 0);
      ("pie by wallet", pie_at Nav.Root_by_wallet 0);
      ("pie drilled into wallet",
       pie_at (Nav.Wallet "Caf\xc3\xa9 wallet with a long name") 0) ];
  let p = Portfolio.of_snapshot nasty_snapshot in
  check "price_error is sanitised too"
    (match
       Screen.render Nav.initial { p with price_error = Some "bad\n\xff" } (80, 40)
     with
     | _ -> true
     | exception _ -> false);
  check "breadcrumb is sanitised"
    (String.for_all (fun c -> Char.code c >= 0x20 && Char.code c < 0x7f)
       (Pie_view.breadcrumb (Nav.Wallet "a\nb\xc3\xa9")))

let test_total_options () =
  print_endline "total completeness";
  let none = Portfolio.of_snapshot (snapshot_of [ balance "A" Config.Bitcoin (Ok [ (Snapshot.holding Config.btc 1.) ]) ]) in
  check "nothing priced gives no total" (none.total_usd = None);
  check "nothing priced is partial" none.total_is_partial;
  let full =
    Portfolio.of_snapshot
      (snapshot_of ~prices:[ (Config.btc, 5.) ] [ balance "A" Config.Bitcoin (Ok [ (Snapshot.holding Config.btc 1.) ]) ])
  in
  check "fully priced total" (full.total_usd = Some 5. && not full.total_is_partial);
  let pie_state = pie_at Nav.Root_by_coin 0 in
  check "unpriced pie still renders" (renders_without_raising pie_state)

let test_other_name_collision () =
  print_endline "Other name collision";
  let many =
    List.init 9 (fun i ->
        balance (if i = 8 then "Other (2)" else Printf.sprintf "W%d" i) Config.Bitcoin
          (Ok [ (Snapshot.holding Config.btc (float_of_int (20 - i))) ]))
  in
  let p = Portfolio.of_snapshot (snapshot_of ~prices:[ (Config.btc, 1.) ] many) in
  let rows = Nav.rows p Nav.Root_by_wallet in
  check_string "bucket named like a wallet" ~expect:"Other (2)" (List.nth rows 7).name;
  check "bucket does not drill"
    (Nav.drill_target p (pie_at Nav.Root_by_wallet 7) = None);
  check "real wallet with the same name drills when it is a slice"
    (let few = Portfolio.of_snapshot
         (snapshot_of ~prices:[ (Config.btc, 1.) ]
            [ balance "Other (2)" Config.Bitcoin (Ok [ (Snapshot.holding Config.btc 1.) ]) ]) in
     Nav.drill_target few (pie_at Nav.Root_by_wallet 0) = Some (Nav.Wallet "Other (2)"))

let () =
  test_render_is_safe ();
  test_total_options ();
  test_other_name_collision ();
  test_of_snapshot ();
  test_by_asset ();
  test_by_wallet ();
  test_asset_by_wallet ();
  test_wallet_by_asset ();
  test_unpriced ();
  test_zero_balances ();
  test_table ();
  test_self_valued_and_loans ();
  test_navigation ();
  test_other_not_drillable ();
  test_key_mapping ();
  test_format ();
  summary ()
