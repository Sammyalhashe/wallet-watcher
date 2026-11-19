(* Pure navigation: view/focus state, events and the transition function. *)

open Wallet_watcher

type view = Table | Pie

type focus =
  | Root_by_coin
  | Root_by_wallet
  | Coin of Config.asset
  | Wallet of string

type state = { view : view; focus : focus; selected : int }

type event =
  | Toggle_view
  | Switch_root
  | Move of int
  | Drill
  | Up
  | Quit

let initial = { view = Table; focus = Root_by_coin; selected = 0 }

let slices_of (p : Portfolio.t) = function
  | Root_by_coin -> Portfolio.by_asset p
  | Root_by_wallet -> Portfolio.by_wallet p
  | Coin asset -> Portfolio.asset_by_wallet p asset
  | Wallet label -> Portfolio.wallet_by_asset p label

(* Must equal [Palette.max_slices]; the TUI has one glyph per slice. *)
let max_slices = 8

let rows p focus = Portfolio.fold ~max_slices (slices_of p focus)

let index_of_name p focus name =
  let rec find i = function
    | [] -> 0
    | (s : Portfolio.slice) :: _ when s.name = name && not s.folded -> i
    | _ :: rest -> find (i + 1) rest
  in
  find 0 (rows p focus)

let selected_drillable_name p state =
  match List.nth_opt (rows p state.focus) state.selected with
  | Some { Portfolio.folded = false; name; _ } -> Some name
  | _ -> None

let drill_target p state =
  match (state.focus, selected_drillable_name p state) with
  | Root_by_coin, Some name ->
    List.find_opt (fun a -> Config.string_of_asset a = name) (Portfolio.assets p)
    |> Option.map (fun a -> Coin a)
  | Root_by_wallet, Some name ->
    List.find_opt (String.equal name) (Portfolio.wallets p)
    |> Option.map (fun l -> Wallet l)
  | _ -> None

let step p state = function
  | Quit -> None
  | Toggle_view ->
    Some { state with view = (match state.view with Table -> Pie | Pie -> Table) }
  | _ when state.view = Table -> Some state
  | Switch_root -> (
    match state.focus with
    | Root_by_coin -> Some { state with focus = Root_by_wallet; selected = 0 }
    | Root_by_wallet -> Some { state with focus = Root_by_coin; selected = 0 }
    | Coin _ | Wallet _ -> Some state)
  | Move delta ->
    let last = max 0 (List.length (rows p state.focus) - 1) in
    Some { state with selected = max 0 (min last (state.selected + delta)) }
  | Drill -> (
    match drill_target p state with
    | Some focus -> Some { state with focus; selected = 0 }
    | None -> Some state)
  | Up -> (
    match state.focus with
    | Root_by_coin | Root_by_wallet -> Some state
    | Coin asset ->
      Some { state with focus = Root_by_coin;
                        selected = index_of_name p Root_by_coin (Config.string_of_asset asset) }
    | Wallet label ->
      Some { state with focus = Root_by_wallet;
                        selected = index_of_name p Root_by_wallet label })

let event_of_input = function
  | `Key (`ASCII ('c' | 'C'), mods) ->
    if List.mem `Ctrl mods then Some Quit else Some Toggle_view
  | `Key (`ASCII 'q', _) -> Some Quit
  | `Key (`Tab, _) -> Some Switch_root
  | `Key ((`ASCII 'j' | `Arrow `Down), _) -> Some (Move 1)
  | `Key ((`ASCII 'k' | `Arrow `Up), _) -> Some (Move (-1))
  | `Key ((`Enter | `ASCII 'l'), _) -> Some Drill
  | `Key ((`Backspace | `ASCII 'h' | `Escape), _) -> Some Up
  | _ -> None
