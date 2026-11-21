(* EVM tokens and DeFi positions with USD values from the Zerion API (needs
   ZERION_API_KEY). One request per address and chain. *)

open Json

let api_url = "https://api.zerion.io/v1/wallets/"
let dust_usd = 1.0

let positions_url ~chain address =
  api_url ^ Uri.pct_encode address
  ^ "/positions/?filter%5Bpositions%5D=no_filter&filter%5Bchain_ids%5D=" ^ Uri.pct_encode chain
  ^ "&filter%5Btrash%5D=only_non_trash&currency=usd"

let string_at keys json = match path keys json with Some (`String s) -> Some s | _ -> None

let parse_position item =
  let attr = member "attributes" item |> Option.value ~default:`Null in
  let kind = string_at [ "position_type" ] attr |> Option.value ~default:"wallet" in
  let sign = if kind = "loan" then -1. else 1. in
  let* symbol =
    match string_at [ "fungible_info"; "symbol" ] attr with
    | Some s -> Ok s
    | None -> missing [ "fungible_info"; "symbol" ]
  in
  let* amount = number_at [ "quantity"; "float" ] attr in
  let usd = Option.bind (member "value" attr) as_number |> Option.map (fun v -> sign *. v) in
  let detail =
    match (kind, string_at [ "protocol" ] attr) with
    | "wallet", _ -> None
    | _, Some protocol -> Some (protocol ^ " " ^ kind)
    | _, None -> Some kind
  in
  Ok ({ asset = String.uppercase_ascii symbol; amount = sign *. amount; usd; detail }
      : Snapshot.holding)

let displayable item = path [ "attributes"; "flags"; "displayable" ] item <> Some (`Bool false)

let is_dust (h : Snapshot.holding) =
  match h.usd with Some v -> Float.abs v < dust_usd | None -> false

let parse_positions json =
  match member "data" json with
  | Some (`List items) ->
    let* positions =
      List.filter displayable items
      |> List.fold_left
           (fun acc item ->
              let* acc = acc in
              let* p = parse_position item in
              Ok (p :: acc))
           (Ok [])
      |> Result.map List.rev
    in
    Ok (List.filter (fun h -> not (is_dust h)) positions)
  | _ -> missing [ "data" ]

let fetch ~key ~chain address =
  let label = "Zerion " ^ chain ^ " " ^ address in
  Http.fetch_json ~label
    (Http.get ~headers:(Http.with_basic_auth ~user:key ~password:"") ~label
       (positions_url ~chain address))
    parse_positions
