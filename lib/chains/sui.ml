(* Sui: native SUI from suix_getAllBalances. *)

open Json

let rpc_url = "https://fullnode.mainnet.sui.io:443"

let parse_balance json =
  let* result = Http.rpc_result json in
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

let fetch address =
  let label = "SUI " ^ address in
  Http.fetch_single ~asset:Config.sui ~label
    (Http.rpc_call ~label ~url:rpc_url ~meth:"suix_getAllBalances"
       ~params:[ `String address ])
    parse_balance
