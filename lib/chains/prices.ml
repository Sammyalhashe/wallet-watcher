(* USD spot prices from CoinGecko. *)

open Json

let url ids =
  "https://api.coingecko.com/api/v3/simple/price?ids=" ^ String.concat "," ids
  ^ "&vs_currencies=usd"

(* Assets CoinGecko omits are absent, and render as an unknown price. *)
let parse assets json =
  Ok
    (List.filter_map
       (fun asset ->
          match path [ Config.coingecko_id asset; "usd" ] json with
          | Some v -> Option.map (fun p -> (asset, p)) (as_number v)
          | None -> None)
       assets)

let fetch assets =
  let ids = List.map Config.coingecko_id assets in
  let label = "prices" in
  Http.fetch_json ~label (Http.get ~label (url ids)) (parse assets)
