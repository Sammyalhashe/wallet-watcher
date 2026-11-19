(* Bitcoin: confirmed balance from mempool.space's address API. *)

open Json

let api_url = "https://mempool.space/api/address/"

let parse_balance json =
  let* funded = number_at [ "chain_stats"; "funded_txo_sum" ] json in
  let* spent = number_at [ "chain_stats"; "spent_txo_sum" ] json in
  Ok ((funded -. spent) /. 1e8)

let fetch address =
  let label = "BTC " ^ address in
  Http.fetch_single ~asset:Config.BTC ~label
    (Http.get ~label (api_url ^ Uri.pct_encode address))
    parse_balance
