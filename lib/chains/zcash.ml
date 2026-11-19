(* Zcash: transparent-address balances from Blockchair; shielded ones are refused. *)

open Json

let api_url = "https://api.blockchair.com/zcash/dashboards/address/"

let parse_balance address json =
  let* zatoshi = number_at [ "data"; address; "address"; "balance" ] json in
  Ok (zatoshi /. 1e8)

let has_shielded_prefix address =
  List.exists (fun prefix -> String.starts_with ~prefix address) [ "zs"; "zc"; "u1" ]

let fetch address =
  if has_shielded_prefix address then
    Lwt.return (Error "ZEC: shielded addresses are not supported")
  else
    let label = "ZEC " ^ address in
    Http.fetch_single ~asset:Config.ZEC ~label
      (Http.get ~label (api_url ^ Uri.pct_encode address))
      (parse_balance address)
