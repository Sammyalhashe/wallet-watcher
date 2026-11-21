(* Solana: native SOL via the getBalance JSON-RPC call. *)

open Json

let rpc_url = "https://api.mainnet-beta.solana.com"

let parse_balance json =
  let* result = Http.rpc_result json in
  let* lamports = number_at [ "value" ] result in
  Ok (lamports /. 1e9)

let fetch address =
  let label = "SOL " ^ address in
  Http.fetch_single ~asset:Config.sol ~label
    (Http.rpc_call ~label ~url:rpc_url ~meth:"getBalance" ~params:[ `String address ])
    parse_balance
