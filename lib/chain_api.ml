(* Dispatches each (network, address) to its chain module and gathers one snapshot. *)

open Lwt.Infix

(* Without a Zerion key, EVM networks fall back to native balances only. *)
let zerion_or native network address =
  match Sys.getenv_opt "ZERION_API_KEY" with
  | None | Some "" -> native address
  | Some key -> Zerion.fetch ~key ~chain:(Config.string_of_network network) address

let fetch_network : Config.network -> string -> Snapshot.fetch_result Lwt.t = function
  | Config.Ethereum -> zerion_or Evm.fetch_ethereum Config.Ethereum
  | Config.Base -> zerion_or Evm.fetch_base Config.Base
  | Config.Avalanche -> zerion_or Evm.fetch_avalanche Config.Avalanche
  | Config.Bitcoin -> Bitcoin.fetch
  | Config.Solana -> Solana.fetch
  | Config.Sui -> Sui.fetch
  | Config.Zcash -> Zcash.fetch
  | Config.Bittensor -> Bittensor.fetch

let assets_of_network : Config.network -> Config.asset list = function
  | Config.Ethereum | Config.Base -> [ Config.eth ]
  | Config.Avalanche -> [ Config.avax; Config.eth ]
  | Config.Bitcoin -> [ Config.btc ]
  | Config.Solana -> [ Config.sol ]
  | Config.Sui -> [ Config.sui ]
  | Config.Zcash -> [ Config.zec ]
  | Config.Bittensor -> [ Config.tao ]

let distinct_assets wallets =
  List.concat_map
    (fun (w : Config.wallet) ->
       List.concat_map (fun (network, _) -> assets_of_network network) w.addresses)
    wallets
  |> List.fold_left (fun acc a -> if List.mem a acc then acc else a :: acc) []
  |> List.rev

(* Every balance request and the price request run together, so wall time is
   that of the slowest endpoint. *)
let fetch_all (wallets : Config.wallet list) =
  let targets =
    List.concat_map
      (fun (w : Config.wallet) ->
         List.map (fun (network, address) -> (w.label, network, address)) w.addresses)
      wallets
  in
  let fetch_target (wallet, network, address) =
    fetch_network network address >|= fun result ->
    { Snapshot.wallet; network; address; result }
  in
  Lwt.both (Lwt_list.map_p fetch_target targets) (Prices.fetch (distinct_assets wallets))
  >|= fun (balances, prices) ->
  match prices with
  | Ok prices -> { Snapshot.balances; prices; price_error = None }
  | Error e -> { Snapshot.balances; prices = []; price_error = Some e }
