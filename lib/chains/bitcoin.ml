(* Bitcoin: an address's confirmed balance from mempool.space, or an output
   descriptor's UTXO total from your own bitcoind via scantxoutset. *)

open Json

let api_url = "https://mempool.space/api/address/"

let parse_balance json =
  let* funded = number_at [ "chain_stats"; "funded_txo_sum" ] json in
  let* spent = number_at [ "chain_stats"; "spent_txo_sum" ] json in
  Ok ((funded -. spent) /. 1e8)

let fetch_address address =
  let label = "BTC " ^ address in
  Http.fetch_single ~asset:Config.btc ~label
    (Http.get ~label (api_url ^ Uri.pct_encode address))
    parse_balance

let is_descriptor value = String.contains value '('

let replace_all ~pattern ~by s =
  let n = String.length s and m = String.length pattern in
  let buf = Buffer.create n in
  let rec go i =
    if i > n - m then Buffer.add_substring buf s i (n - i)
    else if String.sub s i m = pattern then (Buffer.add_string buf by; go (i + m))
    else (Buffer.add_char buf s.[i]; go (i + 1))
  in
  go 0;
  Buffer.contents buf

(* Core only accepts <0;1> from v29; the edit invalidates any checksum, which
   is optional, so it is dropped. *)
let expand_multipath descriptor =
  let body =
    match String.index_opt descriptor '#' with
    | Some i -> String.sub descriptor 0 i
    | None -> descriptor
  in
  let receive = replace_all ~pattern:"<0;1>" ~by:"0" body in
  if receive = body then [ descriptor ]
  else [ receive; replace_all ~pattern:"<0;1>" ~by:"1" body ]

(* bitcoind rejects a range on a descriptor without a wildcard. *)
let scan_object descriptor =
  `Assoc
    (("desc", `String descriptor)
     :: (if String.contains descriptor '*' then [ ("range", `Int 1000) ] else []))

let scan_params descriptors = [ `String "start"; `List (List.map scan_object descriptors) ]

let parse_scan json =
  let* result = Http.rpc_result json in
  match member "success" result with
  | Some (`Bool false) -> Error "scan did not complete"
  | _ -> number_at [ "total_amount" ] result

let rpc_credentials () =
  let get name = match Sys.getenv_opt name with Some "" -> None | v -> v in
  match
    (get "BITCOIND_RPC_URL", get "BITCOIND_RPC_USER", get "BITCOIND_RPC_PASSWORD")
  with
  | Some url, Some user, Some password -> Ok (url, user, password)
  | _ ->
    Error
      "BTC: set BITCOIND_RPC_URL, BITCOIND_RPC_USER and BITCOIND_RPC_PASSWORD to scan descriptors"

(* bitcoind runs one scan at a time and each can take minutes. *)
let scan_lock = Lwt_mutex.create ()
let scan_timeout_s = 600.

let fetch_descriptor descriptor =
  let label = "BTC descriptor" in
  match rpc_credentials () with
  | Error e -> Lwt.return (Error e)
  | Ok (url, user, password) ->
    Lwt_mutex.with_lock scan_lock (fun () ->
      Http.fetch_single ~asset:Config.btc ~label
        (Http.post_json ~headers:(Http.with_basic_auth ~user ~password)
           ~timeout_s:scan_timeout_s ~label url
           (Http.rpc_payload ~meth:"scantxoutset"
              ~params:(scan_params (expand_multipath descriptor))))
        parse_scan)

let fetch value = if is_descriptor value then fetch_descriptor value else fetch_address value
