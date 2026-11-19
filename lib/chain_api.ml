(* Concurrent balance and price fetching. Every entry point returns a
   [result] rather than raising, so one dead endpoint degrades one row. *)

open Lwt.Infix

let timeout_s = 20.0
let user_agent = "portfolio-tui/0.1"

(* Float, not int: 1 ETH is 10^18 wei and native int overflows near 4.6e18. *)

let float_of_hex s =
  let s =
    if String.length s >= 2 && s.[0] = '0' && (s.[1] = 'x' || s.[1] = 'X') then
      String.sub s 2 (String.length s - 2)
    else s
  in
  if s = "" then Error "empty hex quantity"
  else begin
    let acc = ref 0. and bad = ref None in
    String.iter
      (fun c ->
         let d =
           match c with
           | '0' .. '9' -> Some (Char.code c - Char.code '0')
           | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
           | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
           | _ -> None
         in
         match d with
         | Some d -> acc := (!acc *. 16.) +. float_of_int d
         | None -> if !bad = None then bad := Some c)
      s;
    match !bad with
    | Some c -> Error (Printf.sprintf "invalid hex digit %C" c)
    | None -> Ok !acc
  end

(* JSON helpers are total, so malformed responses surface as [Error]. *)

let member key = function
  | `Assoc fields -> List.assoc_opt key fields
  | _ -> None

let rec path keys json =
  match keys with
  | [] -> Some json
  | k :: rest -> ( match member k json with Some v -> path rest v | None -> None)

let as_number = function
  | `Int i -> Some (float_of_int i)
  | `Float f -> Some f
  | `String s -> float_of_string_opt s
  | _ -> None

let missing keys = Error (Printf.sprintf "missing field %s" (String.concat "." keys))

let number_at keys json =
  match path keys json with
  | None -> missing keys
  | Some v -> (
    match as_number v with
    | Some f -> Ok f
    | None -> Error (Printf.sprintf "field %s is not a number" (String.concat "." keys)))

let parse_json body =
  match Yojson.Basic.from_string body with
  | json -> Ok json
  | exception Yojson.Json_error msg -> Error ("invalid JSON: " ^ msg)

let headers =
  Cohttp.Header.of_list
    [ ("content-type", "application/json"); ("user-agent", user_agent) ]

let with_timeout label thread =
  Lwt.pick
    [ thread; (Lwt_unix.sleep timeout_s >|= fun () -> Error (label ^ ": timed out")) ]

let read_response label (resp, body) =
  Cohttp_lwt.Body.to_string body >|= fun body ->
  let code = Cohttp.Code.code_of_status (Cohttp.Response.status resp) in
  if code >= 200 && code < 300 then Ok body
  else
    (* Some gateways answer errors with a full HTML page. *)
    let snippet = String.sub body 0 (min 120 (String.length body)) in
    Error (Printf.sprintf "%s: HTTP %d %s" label code snippet)

let guard label thread =
  Lwt.catch
    (fun () -> thread)
    (fun exn -> Lwt.return (Error (Printf.sprintf "%s: %s" label (Printexc.to_string exn))))

let http_get ?(label = "GET") url =
  with_timeout label
  @@ guard label
       (Cohttp_lwt_unix.Client.get ~headers (Uri.of_string url)
       >>= read_response label)

let http_post_json ?(label = "POST") url payload =
  let body = Cohttp_lwt.Body.of_string (Yojson.Basic.to_string payload) in
  with_timeout label
  @@ guard label
       (Cohttp_lwt_unix.Client.post ~headers ~body (Uri.of_string url)
       >>= read_response label)

let ( let* ) = Result.bind

let fetch_json ~label thread extract =
  thread >|= fun response ->
  let* body = response in
  let* json = parse_json body in
  extract json |> Result.map_error (fun e -> Printf.sprintf "%s: %s" label e)

let rpc_result json =
  match member "error" json with
  | Some err ->
    let msg =
      match path [ "error"; "message" ] json with
      | Some (`String m) -> m
      | _ -> Yojson.Basic.to_string err
    in
    Error ("rpc error: " ^ msg)
  | None -> ( match member "result" json with
    | Some r -> Ok r
    | None -> missing [ "result" ])

let rpc_call ~label ~url ~meth ~params =
  let payload =
    `Assoc
      [ ("jsonrpc", `String "2.0"); ("id", `Int 1); ("method", `String meth);
        ("params", `List params) ]
  in
  http_post_json ~label url payload

let eth_rpc_url = "https://ethereum-rpc.publicnode.com"
let base_rpc_url = "https://base-rpc.publicnode.com"
let avalanche_rpc_url = "https://avalanche-c-chain-rpc.publicnode.com"
let sol_rpc_url = "https://api.mainnet-beta.solana.com"
let sui_rpc_url = "https://fullnode.mainnet.sui.io:443"
let btc_api_url = "https://mempool.space/api/address/"
let taostats_url = "https://api.taostats.io/api/v3/account/"
let blockchair_zcash_url = "https://api.blockchair.com/zcash/dashboards/address/"
let bridged_weth_address = "0x49D5c2BdFfac6CE2BFdB6640F4F80f226bc10bAB"
let balance_of_selector = "0x70a08231"

(* Decoders are pure and separate from requests so tests can feed them fixtures. *)

let parse_eth_balance json =
  let* result = rpc_result json in
  match result with
  | `String hex ->
    let* wei = float_of_hex hex in
    Ok (wei /. 1e18)
  | _ -> Error "result is not a hex string"

let parse_erc20_balance ~decimals json =
  let* result = rpc_result json in
  match result with
  | `String hex ->
    let* raw = float_of_hex hex in
    Ok (raw /. (10. ** float_of_int decimals))
  | _ -> Error "result is not a hex string"

let is_hex_digit = function '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true | _ -> false

let balance_of_calldata address =
  let well_formed =
    String.length address = 42
    && address.[0] = '0'
    && address.[1] = 'x'
    && String.for_all is_hex_digit (String.sub address 2 40)
  in
  if not well_formed then Error (Printf.sprintf "invalid EVM address %S" address)
  else Ok (balance_of_selector ^ String.make 24 '0'
           ^ String.lowercase_ascii (String.sub address 2 40))

let parse_sol_balance json =
  let* result = rpc_result json in
  let* lamports = number_at [ "value" ] result in
  Ok (lamports /. 1e9)

let parse_btc_balance json =
  let* funded = number_at [ "chain_stats"; "funded_txo_sum" ] json in
  let* spent = number_at [ "chain_stats"; "spent_txo_sum" ] json in
  Ok ((funded -. spent) /. 1e8)

let parse_sui_balance json =
  let* result = rpc_result json in
  match result with
  | `List balances ->
    (* suix_getAllBalances lists every coin type; keep native SUI. *)
    let is_sui entry =
      match member "coinType" entry with
      | Some (`String t) -> t = "0x2::sui::SUI"
      | _ -> false
    in
    let total =
      List.filter is_sui balances
      |> List.filter_map (fun e ->
           match member "totalBalance" e with Some v -> as_number v | None -> None)
      |> List.fold_left ( +. ) 0.
    in
    Ok (total /. 1e9)
  | _ -> Error "result is not a list of balances"

let parse_tao_balance json =
  (* The v3 payload shape has varied between releases; [rao] is TAO * 10^9. *)
  let candidates =
    [ [ "data"; "0"; "balance_total" ]; [ "data"; "0"; "balance_free" ];
      [ "balance_total" ]; [ "balance_free" ]; [ "balance" ] ]
  in
  let lookup keys =
    let step acc k =
      match acc with
      | None -> None
      | Some (`List items) -> (
        match int_of_string_opt k with
        | Some i -> List.nth_opt items i
        | None -> None)
      | Some json -> member k json
    in
    List.fold_left step (Some json) keys
  in
  match
    List.find_map
      (fun keys -> match lookup keys with Some v -> as_number v | None -> None)
      candidates
  with
  | Some rao -> Ok (rao /. 1e9)
  | None -> Error "could not locate a balance field in the response"

let parse_zec_balance address json =
  let* zatoshi = number_at [ "data"; address; "address"; "balance" ] json in
  Ok (zatoshi /. 1e8)

let evm_get_balance ~label ~url address =
  fetch_json ~label
    (rpc_call ~label ~url ~meth:"eth_getBalance"
       ~params:[ `String address; `String "latest" ])
    parse_eth_balance

let fetch_evm_native ~asset ~url address =
  evm_get_balance ~label:(Config.string_of_asset asset ^ " " ^ address) ~url address
  >|= Result.map (fun amount -> [ (asset, amount) ])

let fetch_bridged_weth address =
  let label = "ETH (WETH.e) " ^ address in
  match balance_of_calldata address with
  | Error e -> Lwt.return (Error (label ^ ": " ^ e))
  | Ok data ->
    let call = `Assoc [ ("to", `String bridged_weth_address); ("data", `String data) ] in
    fetch_json ~label
      (rpc_call ~label ~url:avalanche_rpc_url ~meth:"eth_call"
         ~params:[ call; `String "latest" ])
      (parse_erc20_balance ~decimals:18)

let fetch_avalanche address =
  Lwt.both
    (evm_get_balance ~label:("AVAX " ^ address) ~url:avalanche_rpc_url address)
    (fetch_bridged_weth address)
  >|= fun (native, weth) ->
  let* avax = native in
  let* eth = weth in
  Ok [ (Config.AVAX, avax); (Config.ETH, eth) ]

let fetch_single ~asset ~label fetch parse =
  fetch_json ~label fetch parse
  >|= Result.map (fun amount -> [ (asset, amount) ])

let fetch_sol address =
  let label = "SOL " ^ address in
  fetch_single ~asset:Config.SOL ~label
    (rpc_call ~label ~url:sol_rpc_url ~meth:"getBalance" ~params:[ `String address ])
    parse_sol_balance

let fetch_btc address =
  let label = "BTC " ^ address in
  fetch_single ~asset:Config.BTC ~label
    (http_get ~label (btc_api_url ^ Uri.pct_encode address))
    parse_btc_balance

let fetch_sui address =
  let label = "SUI " ^ address in
  fetch_single ~asset:Config.SUI ~label
    (rpc_call ~label ~url:sui_rpc_url ~meth:"suix_getAllBalances"
       ~params:[ `String address ])
    parse_sui_balance

let has_shielded_prefix address =
  List.exists (fun prefix -> String.starts_with ~prefix address) [ "zs"; "zc"; "u1" ]

let fetch_zec address =
  if has_shielded_prefix address then
    Lwt.return (Error "ZEC: shielded addresses are not supported")
  else
    let label = "ZEC " ^ address in
    fetch_single ~asset:Config.ZEC ~label
      (http_get ~label (blockchair_zcash_url ^ Uri.pct_encode address))
      (parse_zec_balance address)

(* Without an API key the row is reported unavailable, not guessed. *)
let fetch_tao address =
  let label = "TAO " ^ address in
  match Sys.getenv_opt "TAOSTATS_API_KEY" with
  | None | Some "" ->
    Lwt.return (Error "TAO: set TAOSTATS_API_KEY to enable Bittensor balances")
  | Some key ->
    let headers =
      Cohttp.Header.of_list
        [ ("accept", "application/json"); ("authorization", key);
          ("user-agent", user_agent) ]
    in
    let request =
      guard label
        (Cohttp_lwt_unix.Client.get ~headers
           (Uri.of_string (taostats_url ^ Uri.pct_encode address))
        >>= read_response label)
      |> with_timeout label
    in
    fetch_single ~asset:Config.TAO ~label request parse_tao_balance

let fetch_network (network : Config.network) address =
  match network with
  | Config.Ethereum -> fetch_evm_native ~asset:Config.ETH ~url:eth_rpc_url address
  | Config.Base -> fetch_evm_native ~asset:Config.ETH ~url:base_rpc_url address
  | Config.Avalanche -> fetch_avalanche address
  | Config.Bitcoin -> fetch_btc address
  | Config.Solana -> fetch_sol address
  | Config.Sui -> fetch_sui address
  | Config.Zcash -> fetch_zec address
  | Config.Bittensor -> fetch_tao address

let assets_of_network : Config.network -> Config.asset list = function
  | Config.Ethereum | Config.Base -> [ Config.ETH ]
  | Config.Avalanche -> [ Config.AVAX; Config.ETH ]
  | Config.Bitcoin -> [ Config.BTC ]
  | Config.Solana -> [ Config.SOL ]
  | Config.Sui -> [ Config.SUI ]
  | Config.Zcash -> [ Config.ZEC ]
  | Config.Bittensor -> [ Config.TAO ]

let coingecko_url ids =
  "https://api.coingecko.com/api/v3/simple/price?ids=" ^ String.concat "," ids
  ^ "&vs_currencies=usd"

(* Assets CoinGecko omits are absent, and render as an unknown price. *)
let parse_prices assets json =
  Ok
    (List.filter_map
       (fun asset ->
          match path [ Config.coingecko_id asset; "usd" ] json with
          | Some v -> Option.map (fun p -> (asset, p)) (as_number v)
          | None -> None)
       assets)

let fetch_prices assets =
  let ids = List.map Config.coingecko_id assets in
  let label = "prices" in
  fetch_json ~label (http_get ~label (coingecko_url ids)) (parse_prices assets)

let distinct_assets wallets =
  List.concat_map
    (fun (w : Config.wallet) ->
       List.concat_map (fun (network, _) -> assets_of_network network) w.addresses)
    wallets
  |> List.fold_left (fun acc a -> if List.mem a acc then acc else a :: acc) []
  |> List.rev

(* Every balance request and the price request run together, so wall time is
   that of the slowest endpoint. *)
let fetch_all (wallets : Config.wallet list) =
  let targets =
    List.concat_map
      (fun (w : Config.wallet) ->
         List.map (fun (network, address) -> (w.label, network, address)) w.addresses)
      wallets
  in
  let fetch_target (wallet, network, address) =
    fetch_network network address >|= fun result ->
    { Snapshot.wallet; network; address; result }
  in
  Lwt.both (Lwt_list.map_p fetch_target targets) (fetch_prices (distinct_assets wallets))
  >|= fun (balances, prices) ->
  match prices with
  | Ok prices -> { Snapshot.balances; prices; price_error = None }
  | Error e -> { Snapshot.balances; prices = []; price_error = Some e }
