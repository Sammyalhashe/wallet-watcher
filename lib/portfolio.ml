(* Portfolio analytics: per-asset totals, USD valuation and per-wallet share.

   Share is defined within an asset, per the spec:
     Share(Wallet_i) = Balance(Wallet_i) / Total_Asset_Balance * 100 *)

type holding = {
  wallet : Config.wallet;
  amount : (float, string) result;
  usd : float option;   (* None when the balance failed or the price is unknown *)
  share : float option; (* percent of this asset's total; None if that total is 0 *)
}

type asset = {
  chain : Config.chain;
  price : float option;
  total_amount : float;
  total_usd : float option;
  holdings : holding list;
}

type t = {
  assets : asset list;
  total_usd : float;
  price_error : string option;
}

(* Chains in order of first appearance, so the view follows config.json. *)
let chains_in_order balances =
  List.fold_left
    (fun acc ((w : Config.wallet), _) ->
       if List.mem w.chain acc then acc else w.chain :: acc)
    [] balances
  |> List.rev

let build_asset prices chain balances =
  let mine =
    List.filter (fun ((w : Config.wallet), _) -> w.chain = chain) balances
  in
  let price = List.assoc_opt chain prices in
  let total_amount =
    List.fold_left
      (fun acc (_, amount) -> match amount with Ok a -> acc +. a | Error _ -> acc)
      0. mine
  in
  let holdings =
    List.map
      (fun (wallet, amount) ->
         let usd =
           match (amount, price) with
           | Ok a, Some p -> Some (a *. p)
           | _ -> None
         in
         let share =
           match amount with
           | Ok a when total_amount > 0. -> Some (a /. total_amount *. 100.)
           | _ -> None
         in
         { wallet; amount; usd; share })
      mine
  in
  let total_usd = Option.map (fun p -> total_amount *. p) price in
  { chain; price; total_amount; total_usd; holdings }

let of_snapshot (s : Chain_api.snapshot) =
  let assets =
    List.map
      (fun chain -> build_asset s.prices chain s.balances)
      (chains_in_order s.balances)
  in
  let total_usd =
    List.fold_left
      (fun acc (a : asset) -> match a.total_usd with Some v -> acc +. v | None -> acc)
      0. assets
  in
  { assets; total_usd; price_error = s.price_error }

(* Each asset's share of total net worth. [None] where the asset could not be
   valued, so an unpriced holding is reported as unknown rather than as 0%. *)
let allocation t =
  List.map
    (fun (a : asset) ->
       let pct =
         match a.total_usd with
         | Some v when t.total_usd > 0. -> Some (v /. t.total_usd *. 100.)
         | _ -> None
       in
       (a, pct))
    t.assets

(* Every failed balance fetch, for the status line. *)
let errors t =
  List.concat_map
    (fun a ->
       List.filter_map
         (fun h ->
            match h.amount with
            | Error e -> Some (h.wallet, e)
            | Ok _ -> None)
         a.holdings)
    t.assets
