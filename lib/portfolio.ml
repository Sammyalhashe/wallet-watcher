(* Portfolio analytics over a fetched snapshot. Terminal-free, so any front end
   (Notty, lambda-term) can embed it. *)

type position = {
  wallet : string;
  network : Config.network;
  asset : Config.asset;
  amount : float;
  address : string;
}

type failure = {
  wallet : string;
  network : Config.network;
  address : string;
  error : string;
}

type t = {
  positions : position list;
  failures : failure list;
  prices : (Config.asset * float) list;
  price_error : string option;
  total_usd : float;
}

type slice = {
  name : string;
  amount : float option;
  usd : float option;
  pct : float option;
}

(* {1 Helpers} *)

let price_of t asset = List.assoc_opt asset t.prices

let usd_of_position prices (p : position) =
  Option.map (fun price -> p.amount *. price) (List.assoc_opt p.asset prices)

(* None when nothing is known, so "unpriced" never collapses into zero. *)
let sum_known values =
  List.fold_left
    (fun acc v ->
       match (acc, v) with
       | None, v -> v
       | Some a, Some b -> Some (a +. b)
       | acc, None -> acc)
    None values

let sum_amounts positions =
  List.fold_left (fun acc (p : position) -> acc +. p.amount) 0. positions

let distinct key items =
  List.fold_left
    (fun acc x -> if List.mem (key x) acc then acc else key x :: acc)
    [] items
  |> List.rev

let group_by key items =
  List.map
    (fun k -> (k, List.filter (fun x -> key x = k) items))
    (distinct key items)

let held t = List.filter (fun (p : position) -> p.amount > 0.) t.positions

let of_snapshot (s : Snapshot.t) =
  let positions, failures =
    List.fold_left
      (fun (positions, failures) (b : Snapshot.balance) ->
         match b.result with
         | Ok holdings ->
           ( positions
             @ List.map
                 (fun (asset, amount) ->
                    { wallet = b.wallet; network = b.network; asset; amount;
                      address = b.address })
                 holdings,
             failures )
         | Error error ->
           ( positions,
             failures
             @ [ { wallet = b.wallet; network = b.network; address = b.address;
                   error } ] ))
      ([], []) s.balances
  in
  let total_usd =
    Option.value ~default:0.
      (sum_known (List.map (usd_of_position s.prices) positions))
  in
  { positions; failures; prices = s.prices; price_error = s.price_error;
    total_usd }

let assets t = distinct (fun (p : position) -> p.asset) t.positions
let wallets t = distinct (fun (p : position) -> p.wallet) t.positions

(* {1 Slices} *)

let by_descending_value (a : slice) (b : slice) =
  match (a.usd, b.usd) with
  | Some x, Some y -> compare y x
  | Some _, None -> -1
  | None, Some _ -> 1
  | None, None ->
    compare (Option.value b.amount ~default:0.) (Option.value a.amount ~default:0.)

let slice_of t ~name ~show_amount positions : slice =
  { name;
    amount = (if show_amount then Some (sum_amounts positions) else None);
    usd = sum_known (List.map (usd_of_position t.prices) positions);
    pct = None }

let with_pct_of_usd slices =
  let total = sum_known (List.map (fun (s : slice) -> s.usd) slices) in
  List.map
    (fun (s : slice) ->
       match (s.usd, total) with
       | Some u, Some total when total > 0. -> { s with pct = Some (u /. total *. 100.) }
       | _ -> s)
    slices
  |> List.stable_sort by_descending_value

let with_pct_of_amount slices =
  let total =
    List.fold_left
      (fun acc (s : slice) -> acc +. Option.value s.amount ~default:0.)
      0. slices
  in
  List.map
    (fun (s : slice) ->
       match s.amount with
       | Some a when total > 0. -> { s with pct = Some (a /. total *. 100.) }
       | _ -> s)
    slices
  |> List.stable_sort by_descending_value

let by_asset t =
  group_by (fun (p : position) -> p.asset) (held t)
  |> List.map (fun (asset, ps) ->
       slice_of t ~name:(Config.string_of_asset asset) ~show_amount:true ps)
  |> with_pct_of_usd

let by_wallet t =
  group_by (fun (p : position) -> p.wallet) (held t)
  |> List.map (fun (wallet, ps) -> slice_of t ~name:wallet ~show_amount:false ps)
  |> with_pct_of_usd

let asset_by_wallet t asset =
  List.filter (fun (p : position) -> p.asset = asset) (held t)
  |> group_by (fun (p : position) -> p.wallet)
  |> List.map (fun (wallet, ps) -> slice_of t ~name:wallet ~show_amount:true ps)
  |> with_pct_of_amount

let wallet_by_asset t label =
  List.filter (fun (p : position) -> p.wallet = label) (held t)
  |> group_by (fun (p : position) -> p.asset)
  |> List.map (fun (asset, ps) ->
       slice_of t ~name:(Config.string_of_asset asset) ~show_amount:true ps)
  |> with_pct_of_usd

(* {1 Table} *)

type row = {
  position : position;
  usd_value : float option;
  share : float option;  (* of the asset's units, across all wallets *)
}

type asset_block = {
  coin : Config.asset;
  price : float option;
  subtotal_amount : float;
  subtotal_usd : float option;
  rows : row list;
}

let table t =
  List.map
    (fun coin ->
       let mine = List.filter (fun (p : position) -> p.asset = coin) t.positions in
       let subtotal_amount = sum_amounts mine in
       let rows =
         List.map
           (fun p ->
              { position = p;
                usd_value = usd_of_position t.prices p;
                share =
                  (if subtotal_amount > 0. then Some (p.amount /. subtotal_amount *. 100.)
                   else None) })
           mine
       in
       { coin;
         price = price_of t coin;
         subtotal_amount;
         subtotal_usd = sum_known (List.map (fun r -> r.usd_value) rows);
         rows })
    (assets t)
