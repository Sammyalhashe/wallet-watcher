(* EVM chains: native balances on Ethereum, Base and Avalanche, plus WETH.e on Avalanche. *)

open Lwt.Infix
open Json

let ethereum_rpc_url = "https://ethereum-rpc.publicnode.com"
let base_rpc_url = "https://base-rpc.publicnode.com"
let avalanche_rpc_url = "https://avalanche-c-chain-rpc.publicnode.com"
let bridged_weth_address = "0x49D5c2BdFfac6CE2BFdB6640F4F80f226bc10bAB"
let balance_of_selector = "0x70a08231"

let parse_native_balance json =
  let* result = Http.rpc_result json in
  match result with
  | `String hex ->
    let* wei = float_of_hex hex in
    Ok (wei /. 1e18)
  | _ -> Error "result is not a hex string"

let parse_erc20_balance ~decimals json =
  let* result = Http.rpc_result json in
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

let get_native_balance ~label ~url address =
  Http.fetch_json ~label
    (Http.rpc_call ~label ~url ~meth:"eth_getBalance"
       ~params:[ `String address; `String "latest" ])
    parse_native_balance

let fetch_native ~asset ~url address =
  get_native_balance ~label:(Config.string_of_asset asset ^ " " ^ address) ~url address
  >|= Result.map (fun amount -> [ (asset, amount) ])

let fetch_ethereum = fetch_native ~asset:Config.ETH ~url:ethereum_rpc_url
let fetch_base = fetch_native ~asset:Config.ETH ~url:base_rpc_url

let fetch_bridged_weth address =
  let label = "ETH (WETH.e) " ^ address in
  match balance_of_calldata address with
  | Error e -> Lwt.return (Error (label ^ ": " ^ e))
  | Ok data ->
    let call = `Assoc [ ("to", `String bridged_weth_address); ("data", `String data) ] in
    Http.fetch_json ~label
      (Http.rpc_call ~label ~url:avalanche_rpc_url ~meth:"eth_call"
         ~params:[ call; `String "latest" ])
      (parse_erc20_balance ~decimals:18)

(* WETH.e is a secondary leg: its failure must not discard the AVAX balance. *)
let combine_avalanche native weth =
  let* avax = native in
  match weth with
  | Ok eth -> Ok [ (Config.AVAX, avax); (Config.ETH, eth) ]
  | Error _ -> Ok [ (Config.AVAX, avax) ]

let fetch_avalanche address =
  Lwt.both
    (get_native_balance ~label:("AVAX " ^ address) ~url:avalanche_rpc_url address)
    (fetch_bridged_weth address)
  >|= fun (native, weth) -> combine_avalanche native weth
