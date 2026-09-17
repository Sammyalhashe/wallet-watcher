(* Concurrent balance and price fetching.

   Every entry point returns a [result] rather than raising: a single
   unreachable endpoint should degrade one row of the dashboard, not abort the
   whole refresh. *)

open Lwt.Infix

let timeout_s = 20.0
let user_agent = "portfolio-tui/0.1"

(* {1 Numeric helpers}

   Balances are parsed into [float] rather than [int]. 1 ETH is 10^18 wei and
   OCaml's native int tops out near 4.6 * 10^18, so anything above ~4.6 ETH
   would silently overflow. Float keeps ~15 significant digits, which is far
   more than a portfolio view needs. *)

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

(* {1 JSON helpers} — total, so malformed responses surface as [Error]. *)

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

(* Read a numeric value at [keys], accepting ints, floats or numeric strings. *)
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

(* {1 HTTP} *)

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
    (* Truncate: some gateways answer errors with a full HTML page. *)
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

(* Fetch [url], parse the body as JSON, then hand it to [extract]. *)
let fetch_json ~label thread extract =
  thread >|= fun response ->
  let* body = response in
  let* json = parse_json body in
  extract json |> Result.map_error (fun e -> Printf.sprintf "%s: %s" label e)

(* A JSON-RPC 2.0 envelope; surfaces a server-side [error] member as [Error]. *)
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

(* {1 Per-chain balance fetchers}

   Each returns the balance in whole units of the chain's native asset. *)

let eth_rpc_url = "https://ethereum-rpc.publicnode.com"
let sol_rpc_url = "https://api.mainnet-beta.solana.com"
let sui_rpc_url = "https://fullnode.mainnet.sui.io:443"
let btc_api_url = "https://mempool.space/api/address/"
let taostats_url = "https://api.taostats.io/api/v3/account/"

(* The response decoders are kept separate from the requests so they can be
   exercised against recorded payloads in the test suite. *)

let parse_eth_balance json =
  let* result = rpc_result json in
  match result with
  | `String hex ->
    let* wei = float_of_hex hex in
    Ok (wei /. 1e18)
  | _ -> Error "result is not a hex string"

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
    (* [suix_getAllBalances] returns every coin type held; keep native SUI. *)
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

let fetch_eth address =
  let label = "ETH " ^ address in
  fetch_json ~label
    (rpc_call ~label ~url:eth_rpc_url ~meth:"eth_getBalance"
       ~params:[ `String address; `String "latest" ])
    parse_eth_balance

let fetch_sol address =
  let label = "SOL " ^ address in
  fetch_json ~label
    (rpc_call ~label ~url:sol_rpc_url ~meth:"getBalance" ~params:[ `String address ])
    parse_sol_balance

let fetch_btc address =
  let label = "BTC " ^ address in
  fetch_json ~label
    (http_get ~label (btc_api_url ^ Uri.pct_encode address))
    parse_btc_balance

let fetch_sui address =
  let label = "SUI " ^ address in
  fetch_json ~label
    (rpc_call ~label ~url:sui_rpc_url ~meth:"suix_getAllBalances"
       ~params:[ `String address ])
    parse_sui_balance

let parse_tao_balance json =
  (* The v3 payload has moved around between releases; accept the shapes seen
     in the wild before giving up. [rao] is TAO * 10^9. *)
  let candidates =
    [ [ "data"; "0"; "balance_total" ]; [ "data"; "0"; "balance_free" ];
      [ "balance_total" ]; [ "balance_free" ]; [ "balance" ] ]
  in
  let lookup keys =
    (* Support a numeric index into "data" as well as object members. *)
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

(* Taostats requires an API key. Without one we report the row as unavailable
   rather than inventing a number. *)
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
    fetch_json ~label request parse_tao_balance

let fetch_balance (w : Config.wallet) =
  match w.chain with
  | Config.ETH -> fetch_eth w.address
  | Config.SOL -> fetch_sol w.address
  | Config.BTC -> fetch_btc w.address
  | Config.SUI -> fetch_sui w.address
  | Config.TAO -> fetch_tao w.address

(* {1 Prices} *)

let coingecko_url ids =
  "https://api.coingecko.com/api/v3/simple/price?ids=" ^ String.concat "," ids
  ^ "&vs_currencies=usd"

(* Returns one (chain, usd) pair per chain that CoinGecko priced. Chains it
   omits are simply absent, and render as an unknown price. *)
let parse_prices chains json =
  Ok
    (List.filter_map
       (fun chain ->
          match path [ Config.coingecko_id chain; "usd" ] json with
          | Some v -> Option.map (fun p -> (chain, p)) (as_number v)
          | None -> None)
       chains)

let fetch_prices chains =
  let ids = List.map Config.coingecko_id chains in
  let label = "prices" in
  fetch_json ~label (http_get ~label (coingecko_url ids)) (parse_prices chains)

(* {1 Concurrent entry point} *)

type snapshot = {
  balances : (Config.wallet * (float, string) result) list;
  prices : (Config.chain * float) list;
  price_error : string option;
}

(* Fires every balance request and the price request together, so total wall
   time is that of the slowest single endpoint. *)
let fetch_all (wallets : Config.wallet list) =
  let chains =
    List.fold_left
      (fun acc (w : Config.wallet) ->
         if List.mem w.chain acc then acc else w.chain :: acc)
      [] wallets
    |> List.rev
  in
  Lwt.both
    (Lwt_list.map_p (fun w -> fetch_balance w >|= fun r -> (w, r)) wallets)
    (fetch_prices chains)
  >|= fun (balances, prices) ->
  match prices with
  | Ok prices -> { balances; prices; price_error = None }
  | Error e -> { balances; prices = []; price_error = Some e }
